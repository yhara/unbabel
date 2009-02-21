require 'tempfile'
require 'fileutils'

class String
  def unindent
    self =~ /^(\s*)/
    indent = $1
    self.lines.map{|line|
      line.sub(indent, "")
    }.join
  end
end

# String#lines for ruby <= 1.8.6
unless "".respond_to?(:lines)
  class String
    def lines
      self.split(/\r?\n/).map{|line| line+"\n"}
    end
  end
end
    

module Unbabel

  # Signeture class: parse signetures
  class Signeture

    REXP_TYPE = /[\[\]\w]+/
    REXP_SIGNETURE = /(#{REXP_TYPE})\s*::\s*(#{REXP_TYPE}(\s*->\s*#{REXP_TYPE})*)\s*$/
    TYPE_TABLE = [
      ["Int", Integer],
      ["String", String],
    ]

    def self.parse(*args)
      new(*args)
    end

    def initialize(str, comment)
      line = find_signeture(str, Regexp.new(comment))
      if line
        parse_signeture(line)
      else
        raise "signeture not found"
      end
    end
    attr_reader :func_name, :return_type, :arg_types

    def find_signeture(str, rexp_comment)
      str.lines.find{|line|
        line =~ /#{rexp_comment}\s*#{REXP_SIGNETURE}/
      }
    end

    def parse_signeture(line)
      line =~ REXP_SIGNETURE
      @func_name = $1.strip
      @arg_types = $2.split(/->/).map{|x| x.strip}
      @return_type = @arg_types.pop
    end
    #TODO 4: multi signetures?

  end

  class Runner

    def self.run(*args)
      new(*args).run
    end

    def initialize(src, args, langdef)
      @src = src
      @args = args
      @langdef = langdef
    end

    def run
      sig = Unbabel::Signeture.parse(@src, @langdef.comment)
      stub = make_stub(@src, @args, @langdef)
      result = run_stub(stub, @langdef)
      return parse_result(result, sig)
    end

    def make_stub(src, args, langdef)
      sig = Unbabel::Signeture.new(src, langdef.comment)
      stub = langdef.stub
      return stub.gsub(/<FUNCTION_DEFINITION>/, src).
                  gsub(/<FUNCTION_NAME>/, sig.func_name).
                  gsub(/<ARGUMENTS>/, langdef.format_args(args))
    end

    def run_stub(script, langdef)
      # make a temporary file
      tempfile = Tempfile.open('babel21')
      tempfile.write(script)
      tempfile.close(false)

      # run it
      if langdef.name == "Java"
        temp_java  = File.expand_path("Unbabel.java", File.dirname(tempfile.path))
        temp_jclass = File.expand_path("Unbabel.class", File.dirname(tempfile.path))
        system "cp #{tempfile.path} #{temp_java}"
        cmd = "#{langdef.command} #{temp_java}"
      else
        cmd = "#{langdef.command} #{tempfile.path}"
      end
      result = `#{cmd}`

      # delete the file
      if langdef.name == "Java"
        FileUtils.rm(temp_java)
        FileUtils.rm(temp_jclass)
      end
      tempfile.open
      tempfile.close(true)

      result
    end

    def parse_result(result, sig)
      case sig.return_type
      when "Int"
        result.to_i
      when "String"
        result
      when "[String]"
        result =~ /\((.*)\)/
        $1.split(/ /).map{|x| 
          x =~ /"(.*)"/
          $1
        }
      when "[Int]"
        result =~ /\((.*)\)/
        $1.split(/ /).map{|x| x.to_i}
      else
        raise "unknown type: #{sig.return_type}"
      end
    end
    #TODO: support [[Int]], [Object], Number, etc.

  end

  LangDef = Struct.new(:name, :comment, :command, :argtype, :stub)

  class LangDef

    def self.load(path)
      defs = {}
      stub = ""

      in_stub = false
      src = File.read(path)
      src.lines.each do |line|
        if in_stub 
          stub << line
        else
          key, value = line.chomp.split(/:/).map{|x| x.strip}
          if key == "stub"
            in_stub = true
          else
            defs[key] = value
          end
        end
      end

      new(defs["name"], defs["comment"], defs["command"],
          defs["argtype"], stub)
    end

    # call format_sexp, format_square, etc.
    def format_args(args)
      method = "format_#{self.argtype}"
      self.__send__(method, args)
    end
    #TODO 5: type check by the signeture

    def format_sexp(args)
      args.map{|x| format_object(x, " ", "'")}.join(' ')
    end

    def format_square(args)
      args.map{|x| x.inspect}.join(', ')
    end

    def format_paren(args)
      args.map{|x| format_object(x, ", ", "")}.join(', ')
    end

    private

    def format_object(x, delimiter, list_prefix)
      case x
      when Array
        list_prefix + "(" + 
          x.map{|item| format_object(item, delimiter, "")}.join(delimiter) + ")"
      when String
        x.inspect
      when Numeric
        x.to_s
      else
        raise "This type of arguments are not supported: #{x.class}: #{x}"
      end
    end

  end

  class Language
    # LANGDEF = LangDef.new(...)   # each subclass has own constant

    def initialize(src)
      @src = src
    end

    def call(*args)
      Unbabel::Runner.run(@src, args, self.class.const_get(:LANGDEF))
    end
    alias [] call

  end

  # TODO: configurable template path
  dir = File.expand_path("../../templates/", File.dirname(__FILE__)) 
  Dir.glob("#{dir}/*.unb").each do |path|
    langdef = Unbabel::LangDef.load(path)

    lang_class = Class.new(Language)
    lang_class.const_set("LANGDEF", langdef)

    Unbabel.const_set(langdef.name, lang_class)
  end

end
