(use gauche.test)
(use unbabel)

(unbabel-init "../templates")

;----------------------------------------------------

(define FIB 
  "# fib :: Int -> Int
   def fib(n)
     case n
     when 0 then 0
     when 1 then 1
     else fib(n-2) + fib(n-1)
     end
   end")

(define fib (unbabel-ruby FIB))

(test* "unbabel-ruby should call Ruby function" 55 (fib 10))

;----------------------------------------------------

(define take-chars (unbabel-ruby "
  # take_chars :: String -> [Int] -> [String]
  def take_chars(s, ns)
    s.scan(/./).values_at(*ns)
    # you can use String#chars in ruby >= 1.8.7 
  end"))

(test* "unbabel-ruby should call Ruby function with complicated types"
  '("b" "d")
  (take-chars "abcde" '(1 3)))

;----------------------------------------------------
(test-end)
