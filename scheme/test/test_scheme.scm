(use gauche.test)
(use unbabel)

(unbabel-init "../templates")

;----------------------------------------------------

(define FIB 
  ";; fib :: Int -> Int
(define (fib x)
  (case x
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- x 2)) (fib (- x 1))))))")

(define fib (unbabel-scheme FIB))

(test* "unbabel-scheme should call Scheme function" 55 (fib 10))

;;----------------------------------------------------
;
;(define take-chars (unbabel-ruby "
;  # take_chars :: String -> [Int] -> [String]
;  def take_chars(s, ns)
;    s.scan(/./).values_at(*ns)
;    # you can use String#chars in ruby >= 1.8.7 
;  end"))
;
;(test* "unbabel-ruby should call Ruby function with complicated types"
;  '("b" "d")
;  (take-chars "abcde" '(1 3)))

;----------------------------------------------------
(test-end)
