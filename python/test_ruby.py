import unbabel
import unittest

FIB = """
# fib :: Int -> Int
def fib(n)
  case n
  when 0 then 0
  when 1 then 1
  else fib(n-2) + fib(n-1)
  end
end
"""

TAKE_CHARS = """
# take_chars :: String -> [Int] -> [String]
def take_chars(s, ns)
  s.scan(/./).values_at(*ns)
  # Note: you can use String#chars in ruby >= 1.8.7 
end
"""

class TestUnbabel(unittest.TestCase):

  def testFib(self):
    fib = unbabel.ruby(FIB)
    self.assertEqual(fib(10), 55)

  def testTakeChars(self):
    take_chars = unbabel.ruby(TAKE_CHARS)
    self.assertEqual(take_chars("abcde", [1,3]), ["b", "d"])

if __name__ == '__main__':
  unbabel.init(unbabel, "../templates/")
  suite = unittest.makeSuite(TestUnbabel)
  unittest.TextTestRunner(verbosity=2).run(suite)

