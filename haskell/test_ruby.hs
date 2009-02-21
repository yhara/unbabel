import Unbabel
import Test.HUnit

takeChars = "# take_chars :: String -> [Int] -> [String]\n\
\def take_chars(s, ns)\n\
\  s.scan(/./).values_at(*ns)\n\
\  # you can use String#chars in ruby >= 1.8.7 \n\
\end\n"

rubyTest = "test for calling Ruby function" ~:
  do langdef <- loadLangDef "../templates/ruby.unb"
     result <- runScript (Script takeChars langdef) 
                         [ArgStr "abcde", ArgList [ArgInt 1, ArgInt 3]]
     result @?= ListValue [StrValue "b", StrValue "d"]

main = runTestTT (test rubyTest)
