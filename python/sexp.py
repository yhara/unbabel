# taken from http://inforno.net/articles/2008/08/08/sexp-parser-in-python-re-scanner
# thanks a lot!

import re, sys
from unicodedata import east_asian_width
 
try:
  from re import Scanner
except ImportError:
  from sre import Scanner
 
class ParseError(StandardError): pass
 
class Symbol(unicode):
  def __repr__(self):
    return "Symbol(%s)"%unicode.__repr__(self)
 
class TokenProcessor(object):
  PAREN = {"]":"[", ")":"("}
  def __init__(self, value):
    self.result = []
    self.append = self.result.append
    self.string = value
    self.paren_stack = []
    self.pos = 0
 
  def __call__(self, name):
    def _(*a):
      self.before(*a)
      return getattr(self, name)(*a)
    return _
 
  def before(self, scanner, s):
    self.pos += len(s)
    self.skip(scanner, s)
 
  def error(self, scanner, s): self.raise_error("unknown token: %s"%s)
 
  def skip_whitespaces(self, scanner, s): self.append(",")
 
  def skip(self, scanner, s):
    last = "".join(self.result[-2:])
    if last in ["[,", ",,", ",]"]:
      self.result[-2:] = sorted(last, key=ord)[1]
 
  def atom(self, scanner, s):
    if s in ["(", "["]:
      self.append("[")
      self.paren_stack.append(s)
    elif s in [")", "]"]:
      if not self.paren_stack:
        self.raise_error("missing opening parenthesis.")
      if self.PAREN[s] != self.paren_stack.pop():
        self.raise_error("missing closing parenthesis.")
      self.append("]")
    elif re.match(r"""^(".*)$""", s or ""):
      self.append("u"+s)
    elif re.match(r"""^((\-?\d[\de\.]+)|(\s*)|(.*"))$""", s or ""):
      self.append(s)
    else:
      self.append("Symbol(u\"%s\")"%s)
 
  def raise_error(self, msg="parse error", range=3):
    lines = self.string.split("\n")
    curline = self.string[:self.pos].count("\n")
    linepos = self.pos - len("\n".join(lines[:curline]))
    buf = ["\n"]
    for i in xrange(max(0, curline-range), curline+1):
      buf.append("% 5d: %s"%(i+1, lines[i]))
    width = 6 + sum(east_asian_width(c) == 'W' and 2 or 1 for c in lines[i])
    buf.append("%s~"%(" "*width))
    buf.append("line %d, %d: %s"%(curline+1,linepos, msg))
    raise ParseError(("\n".join(buf)).encode(sys.stderr.encoding))
 
def read_sexp(sexp):
  processor = TokenProcessor(sexp)
  scanner = Scanner([
    (r"\s+", processor("skip_whitespaces")),
    (r";[^\n]*\n", processor("skip")),
    (r""""(?:[^"])*"|(\]|\[|\)|\(|[^\(\)\s]+)""", processor("atom")),
    (r".*", processor("error"))
  ], re.M)
  scanner.scan(processor.string)
  if processor.paren_stack:
    processor.raise_error("missing closing parenthesis.")
  result = eval("".join(processor.result).lstrip(","))
  return (isinstance(result, tuple) and (result[0],0) or (result,0))[0]
 
