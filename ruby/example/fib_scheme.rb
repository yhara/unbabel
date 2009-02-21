require 'babel21'

fib = Babel::Scheme.new(<<EOD)
;; int fib(int)
(define (fib x)
  (case x
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- x 2)) (fib (- x 1))))))
EOD

p fib[2]  #=> 1
p fib[3]  #=> 2
p fib[10] #=> 55
