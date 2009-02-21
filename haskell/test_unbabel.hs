import Unbabel
import Test.HUnit

langdef_ruby :: String
langdef_ruby = "name:Ruby\n\
\comment:#\n\
\command:ruby\n\
\argtype:square\n\
\stub:\n\
\<FUNCTION_DEFINITION>\n\
\\n\
\class Unbabel\n\
\  def self.to_sexp(x)\n\
\    case x\n\
\    when Numeric\n\
\      x.to_s\n\
\    when String\n\
\      x.inspect\n\
\    when Array\n\
\      '(' + x.map{|item| Unbabel.to_sexp(item)}.join(' ') + ')'\n\
\    else\n\
\      raise\n\
\    end\n\
\  end\n\
\end\n\
\print Unbabel.to_sexp(<FUNCTION_NAME>(<ARGUMENTS>))"

langdef_ruby_expected :: LangDef
langdef_ruby_expected = LangDef {
                          name = "Ruby",
                          comment = "#",
                          command = "ruby",
                          argtype = "square",
                          stub = "<FUNCTION_DEFINITION>\n\
\\n\
\class Unbabel\n\
\  def self.to_sexp(x)\n\
\    case x\n\
\    when Numeric\n\
\      x.to_s\n\
\    when String\n\
\      x.inspect\n\
\    when Array\n\
\      '(' + x.map{|item| Unbabel.to_sexp(item)}.join(' ') + ')'\n\
\    else\n\
\      raise\n\
\    end\n\
\  end\n\
\end\n\
\print Unbabel.to_sexp(<FUNCTION_NAME>(<ARGUMENTS>))\n"
  }

fib_ruby :: String
fib_ruby = "# fib :: Int -> Int\n\
\def fib(n)\n\
\  case n\n\
\  when 0 then 0\n\
\  when 1 then 1\n\
\  else fib(n-2) + fib(n-1)\n\
\  end\n\
\end\n"

fibRuby :: Script
fibRuby = Script fib_ruby langdef_ruby_expected

------------------------------------------------------------------------
signatureTest =
  [ "signatureOf should return signature of script" ~:
    (signatureOf fibRuby) ~?= Signature {
                                funcName = "fib",
                                returnType = "Int",
                                argTypes = ["Int"]
                              }
  ]
------------------------------------------------------------------------

langdefTest = 
  [ "isStubBegin should detect the line where stub begins" ~:
    (isStubBegin "stub:", isStubBegin "fooabr") ~?= (True, False)

  , "parseLine should split a line into key and value" ~:
    parseLine "foo:bar" ~?= ("foo", "bar")

  , "parseLangDef should parse a langdef string" ~: 
    parseLangDef langdef_ruby ~?= langdef_ruby_expected
  ]

------------------------------------------------------------------------

stubTest = 
  [ "makeStub should make a stub script" ~:
    (makeStub fibRuby [ArgInt 10]) ~?= "# fib :: Int -> Int\n\
\def fib(n)\n\
\  case n\n\
\  when 0 then 0\n\
\  when 1 then 1\n\
\  else fib(n-2) + fib(n-1)\n\
\  end\n\
\end\n\
\\n\
\\n\
\class Unbabel\n\
\  def self.to_sexp(x)\n\
\    case x\n\
\    when Numeric\n\
\      x.to_s\n\
\    when String\n\
\      x.inspect\n\
\    when Array\n\
\      '(' + x.map{|item| Unbabel.to_sexp(item)}.join(' ') + ')'\n\
\    else\n\
\      raise\n\
\    end\n\
\  end\n\
\end\n\
\print Unbabel.to_sexp(fib(10))\n"
  ]

------------------------------------------------------------------------

parserTest =
  [ "parseResult should parse a result" ~:
    (parseResult "55" fibRuby) ~?= (IntValue 55)
  , "parseResult should parse a result of String type" ~:
    (parseResult "\"foo\"" $ Script "# bar :: Int -> String" langdef_ruby_expected) 
      ~?= (StrValue "foo")
  , "parseResult should parse a result of List type" ~:
    (parseResult "(\"a\" \"b\")" $ Script "# bar :: Int -> [String]" langdef_ruby_expected) 
      ~?= (ListValue [StrValue "a", StrValue "b"])
  , "parseResult should parse a result of List of List type" ~:
    (parseResult "((1) (2))" $ Script "# bar :: Int -> [[Int]]" langdef_ruby_expected) 
      ~?= (ListValue [ListValue[IntValue 1], ListValue[IntValue 2]])
  ]

------------------------------------------------------------------------

runnerTest =
  [ "runScript should run a stub script and return its result" ~:
      do result <- runScript fibRuby [ArgInt 10]
         result @?= (IntValue 55)
  ]

------------------------------------------------------------------------

testData = [ "signature" ~: signatureTest
           , "langdef" ~: langdefTest
           , "stub" ~: stubTest
           , "parser" ~: parserTest
           , "runner" ~: runnerTest
           ]

main = runTestTT (test testData)
