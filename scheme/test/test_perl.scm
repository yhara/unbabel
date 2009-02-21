(use gauche.test)
(use unbabel)

(unbabel-init "../templates")

;----------------------------------------------------

(define FIB 
  "use strict;
   use warnings;

   # fib :: Int -> Int
   sub fib {
     my $x = shift;

     if($x == 0){
       return 0;
     }
     elsif($x == 1){
       return 1;
     }
     else{
       return fib($x-2) + fib($x-1);
     }
   }")

(define fib (unbabel-perl FIB))

(test* "unbabel-perl should call Perl function" 55 (fib 10))

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
