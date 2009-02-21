import Unbabel
import Test.HUnit

fib = "# fib :: Int -> Int\n\
\def fib(x):\n\
\  if x == 0:\n\
\    return 0\n\
\  elif x == 1:\n\
\    return 1\n\
\  else:\n\
\    return fib(x-2) + fib(x-1)"

main = runTestTT $ test $ "test for calling Python function" ~:
  (do langdef <- loadLangDef "../templates/python.unb"
      result <- runScript (Script fib langdef) [ArgInt 10]
      result @?= IntValue 55)

