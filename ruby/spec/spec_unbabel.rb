$LOAD_PATH << File.expand_path("../lib", File.dirname(__FILE__))
require 'unbabel'

$KCODE = "u" #for Japanese string test

### Constants

SRC_SCHEME_FIB = <<EOD
;; fib :: Int -> Int
(define (fib x)
  (case x
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- x 2)) (fib (- x 1))))))
EOD

SRC_SCHEME_SPLIT = <<EOD
;; split :: String -> [String]
(define (split str delimiter)
  (string-split str delimiter))
EOD

PATH_SCHEME_LANGDEF = File.expand_path("../../templates/scheme.unb",
                                       File.dirname(__FILE__))

### Utilities

describe "String#unindent" do
  it "should unindent a multi-line string" do
    (<<-EOD).unindent.should == "foo\nbar\n"
      foo
      bar
    EOD
  end
end

describe "Return value of String#lines" do
  before :all do
    @str = "foo\nbar\nbarz"
  end

  it "should return first line" do
    @str.lines.first.should == "foo\n"
  end
end

### Signeture

describe "Unbabel::Signeture" do
  it "should parse the signeture of an unary function" do
    sig = Unbabel::Signeture.parse((<<-EOD).unindent, /;/)
      ;; fib :: Int -> Int
    EOD
    sig.func_name.should == "fib"
    sig.return_type.should == "Int"
    sig.arg_types.should == ["Int"]
  end

  it "should parse the signeture of an binary function" do
    sig = Unbabel::Signeture.parse((<<-EOD).unindent, /;/)
      ;; split :: String -> String -> [String]
    EOD
    sig.func_name.should == "split"
    sig.return_type.should == "[String]"
    sig.arg_types.should == ["String", "String"]
  end
end

### LangDef

describe "Bebel::LangDef" do
  it "should load a langdef file" do
    langdef = Unbabel::LangDef.load(PATH_SCHEME_LANGDEF)

    langdef.name.should == "Scheme"
    langdef.comment.should == ";"
    langdef.command.should == "gosh"
    langdef.argtype.should == "sexp"
    langdef.stub.rstrip.should == (<<-EOD).unindent.rstrip
      <FUNCTION_DEFINITION>

      (define (main args)
        (write (<FUNCTION_NAME> <ARGUMENTS>)))
    EOD
  end
end

describe "format_sexp" do
  before :all do
    @langdef = Unbabel::LangDef.load(PATH_SCHEME_LANGDEF)
  end

  it "should format an integer" do
    @langdef.format_args([10]).should == "10"
  end

  it "should format an string (Japanese, UTF-8 encoded)" do
    @langdef.format_args(["あ"]).should == '"あ"'
  end

  it "should format two arguments" do
    @langdef.format_args(["foo", 99]).should == '"foo" 99'
  end

  it "should format an array" do
    @langdef.format_args([[1,2,3]]).should == "'(1 2 3)"
  end

  it "should format a nested array" do
    @langdef.format_args([1, [2, [3, 4]], 5]).should == "1 '(2 (3 4)) 5"
  end
end

describe "format_square" do
  before :all do
    @langdef = Unbabel::LangDef.new("Dummy", "#", "dummy", "square", "dummy_stub")
  end

  it "should format an integer" do
    @langdef.format_args([10]).should == "10"
  end

  it "should format an string (Japanese, UTF-8 encoded)" do
    @langdef.format_args(["あ"]).should == '"あ"'
  end

  it "should format two arguments" do
    @langdef.format_args(["foo", 99]).should == '"foo", 99'
  end

  it "should format an array" do
    @langdef.format_args([[1,2,3]]).should == "[1, 2, 3]"
  end

  it "should format a nested array" do
    @langdef.format_args([1, [2, [3, 4]], 5]).should == "1, [2, [3, 4]], 5"
  end
end

describe "format_paren" do
  before :all do
    @langdef = Unbabel::LangDef.new("Dummy", "#", "dummy", "paren", "dummy_stub")
  end

  it "should format an integer" do
    @langdef.format_args([10]).should == "10"
  end

  it "should format an string (Japanese, UTF-8 encoded)" do
    @langdef.format_args(["あ"]).should == '"あ"'
  end

  it "should format two arguments" do
    @langdef.format_args(["foo", 99]).should == '"foo", 99'
  end

  it "should format an array" do
    @langdef.format_args([[1,2,3]]).should == "(1, 2, 3)"
  end

  it "should format a nested array" do
    @langdef.format_args([1, [2, [3, 4]], 5]).should == "1, (2, (3, 4)), 5"
  end
end

### Runner

describe "Unbabel::Runner" do 
  before :all do
    @SRC = SRC_SCHEME_FIB
    @STUB_EXPECTED = (<<-EOD).unindent
      #{@SRC}

      (define (main args)
        (write (fib 10)))
    EOD

    @runner = Unbabel::Runner.new("", [], [])
    @langdef = Unbabel::LangDef.load(PATH_SCHEME_LANGDEF)
    @sig = Unbabel::Signeture.new(@SRC, @langdef.comment)
  end

  it "should make a stub script" do
    @runner.make_stub(@SRC, [10], @langdef).should == @STUB_EXPECTED
  end

  it "should run a stub script" do
    script = @STUB_EXPECTED
    @runner.run_stub(script, @langdef).should == "55"
  end

  it "should parse result of a stub script" do
    @runner.parse_result("55", @sig).should == 55
  end

  it "should parse an array-of-int result" do
    sig = Unbabel::Signeture.parse((<<-EOD), ";")
      ;; foo :: Int -> [Int]
    EOD
    @runner.parse_result("(1 2 3)", sig).should == [1, 2, 3]
  end

  it "should run a snippet" do
    runner = Unbabel::Runner.new(@SRC, [10], @langdef)
    runner.run.should == 55
  end

  it "should parse an array-of-string result" do
    runner = Unbabel::Runner.new(SRC_SCHEME_SPLIT, ["foo/bar/baz", "/"], @langdef)
    runner.run.should == %w(foo bar baz)
  end

end

### Integraton test

describe "Unbabel21" do 

  it "should call Scheme fibonacchi" do
    fib = Unbabel::Scheme.new(SRC_SCHEME_FIB)
    fib[2].should == 1
    fib[3].should == 2
  end

  it "should call Scheme function of String -> Array" do
    split = Unbabel::Scheme.new(SRC_SCHEME_SPLIT)
    split["foo/bar/baz", "/"].should == %w(foo bar baz)
  end

  it "should call Ruby function :-)" do
    fib = Unbabel::Ruby.new((<<-EOD).unindent)
      # fib :: Int -> Int
      def fib(n)
        case n
        when 0 then 0
        when 1 then 1
        else fib(n-2) + fib(n-1)
        end
      end
    EOD
    fib[10].should == 55
  end

end
