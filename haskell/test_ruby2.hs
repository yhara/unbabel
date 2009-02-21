-- Note: this test do not work!!
-- we are waiting a Haskell hacker to implement this
import Babel
import Test.HUnit

takeChars :: String -> [Int] -> IO [String]
takeChars = ruby "# take_chars :: String -> [Int] -> [String]\n\
\def take_chars(s, ns)\n\
\  s.scan(/./).values_at(*ns)\n\
\  # you can use String#chars in ruby >= 1.8.7 \n\
\end\n"

rubyTest = "test for calling Ruby function" ~:
  do result <- takeChars "abcde" [1,3]
     result @?= ["b", "d"]

main = runTestTT (test rubyTest)
