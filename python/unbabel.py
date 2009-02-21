#
# unbabel.py
#

import re
import string
import sexp
import os

class Signature:

  """ make a instance of Signature
      >>> sig = Signature("# fib :: Int -> Int", "#")
      >>> sig.funcname
      'fib'
      >>> sig.rettype
      'Int'
      >>> sig.argtypes
      ['Int']
  """
  def __init__(self, src, comment):
    self.parse(src, comment)

  RE_SIGNATURE = "\s*(\S+)\s*::\s*((?:\S+\s*->\s*)+)(\S+)\s*"
  
  def parse(self, src, comment):
    rexp = re.compile(comment + self.RE_SIGNATURE, re.MULTILINE)
    match = rexp.search(src)
    if match:
      (self.funcname, argtypes, self.rettype) = match.groups()
      self.argtypes = map(string.strip, argtypes.split("->")[0:-1])
    else:
      raise RuntimeError("signature not found in script")

# langdef

class LangDef:

  """ make a instance of LangDef
      >>> l = LangDef("../templates/ruby.unb")
      >>> (l.name, l.comment, l.command, l.argtype)
      ('Ruby', '#', 'ruby', 'square')
      >>> l.stub.split("\\n")[0]
      '<FUNCTION_DEFINITION>'
      >>> l.stub.split("\\n")[-3]  # 3..('A`)
      'print Babel.to_sexp(<FUNCTION_NAME>(<ARGUMENTS>))'
  """
  def __init__(self, path):
    f = open(path, "r")
    s = f.read()
    f.close()
    self.parse(s)

  def parse(self, s):
    in_stub = False
    stub = ""
    for line in s.split("\n"):
      if in_stub:
        stub += line + "\n"
      elif line == "stub:":
        in_stub = True
      else:
        (key, value) = line.split(":")
        if key == "name":
          self.name = value
        elif key == "comment":
          self.comment = value
        elif key == "command":
          self.command = value
        elif key == "argtype":
          self.argtype = value
        else:
          raise RuntimeError("unkown key in langdef: " + key)
    self.stub = stub

# stub

class Stub:

  """ make a stub script
      >>> foo = \"\"\"
      ... # test
      ... # foo :: [[Int]] -> Int
      ... foobar
      ... \"\"\"
      >>> stub = Stub(foo, LangDef("../templates/ruby.unb"), [[10, 20], [3, 4]])
      >>> stub.script
      "\\n# test\\n# foo :: [[Int]] -> Int\\nfoobar\\n\\n\\nclass Babel\\n  def self.to_sexp(x)\\n    case x\\n    when Numeric\\n      x.to_s\\n    when String\\n      x.inspect\\n    when Array\\n      '(' + x.map{|item| Babel.to_sexp(item)}.join(' ') + ')'\\n    else\\n      raise\\n    end\\n  end\\nend\\nprint Babel.to_sexp(foo([10, 20], [3, 4]))\\n\\n"
  """
  def __init__(self, src, langdef, args):
    self.langdef = langdef
    self.make(src, args)
  
  def make(self, src, args):
    sig = Signature(src, self.langdef.comment)
    self.script = re.sub("<FUNCTION_DEFINITION>", src,
                  re.sub("<FUNCTION_NAME>", sig.funcname,
                  re.sub("<ARGUMENTS>", self.format_args(args, sig),
                  self.langdef.stub)))
  
  def format_args(self, args, sig):
    formatted = map( lambda arg : self.format_arg(arg, sig, True), args)
    return ", ".join(formatted)
    
  def format_arg(self, arg, sig, outer):
    # TODO: typecheck using sigature
    if isinstance(arg, int):
      return str(arg)
    elif isinstance(arg, str):
      # TODO: I need something like Ruby's String#inspect
      return '"' + re.sub(r"\\", "\\\\", 
                   re.sub(r"\t", "\\t",
                   re.sub(r"\n", "\\n", arg))) + '"'
    elif isinstance(arg, list):
      (pre, between, post) = self.parens(outer)
      formatted = map( lambda x : self.format_arg(x, sig, False), arg)
      return pre + between.join(formatted) + post
    else:
      raise RuntimeError, "unknown type: " + arg

  PARENS = {
    "square": ("[", ", ", "]"),
    "paren":  ("(", ", ", ")"),
    "sexp":   ("(", " ",  ")")
  }
  def parens(self, outer):
    argtype = self.langdef.argtype
    try:
      (pre, between, post) = self.PARENS[argtype]
    except KeyError:
      raise RuntimeError("unknown argtype: " + argtype)
        
    if argtype == "sexp" and outer:
      return ("'" + pre, between, post)
    else:
      return (pre, between, post)

# runner

class Runner: 

  """ run a stub script
      >>> to_i = \"\"\"
      ... # to_i :: String -> Int
      ... def to_i(s); s.to_i; end
      ... \"\"\"
      >>> stub = Stub(to_i, LangDef("../templates/ruby.unb"), ["10"])
      >>> Runner(stub).run()
      10
  """
  def __init__(self, stub):
    self.stub = stub

  def run(self):
    result = self.run_command(self.stub.langdef.command, self.stub.script)
    return sexp.read_sexp(result)
    # TODO: type check by signature

  def run_command(self, command, script):
    (stdin, stdout) = os.popen2(command)
    stdin.write(script)
    stdin.close()
    return stdout.read()

# integration

def init(module, path):
  def makelang(langdef):
    def lang(src):
      """ returns Ruby function
          >>> fib = ruby("# fib :: Int -> Int\\ndef fib(n); 55; end") 
          >>> fib(10)
          55
      """
      def runner(*args):
        stub = Stub(src, langdef, args)
        return Runner(stub).run()
      return runner
    return lang

  for file in os.listdir(path):
    if file.endswith(".unb"):
      langdef = LangDef(os.path.join(path, file))
      setattr(module, file[0:-4], makelang(langdef))

if __name__ == "__main__":
  import doctest
  doctest.testmod()
  
