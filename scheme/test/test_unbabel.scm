#!/usr/bin/env gosh
(use gauche.test)
(use unbabel)
(test-start "Babel")

(unbabel-init "../templates")

;----------------------------------------------------------
(test-section "module code")
(test-module 'unbabel)

(select-module unbabel)
(use gauche.test)
(use srfi-8) ; receive

(define FIB "
# fib :: Int -> Int
def fib(n)
  case n
  when 0 then 0
  when 1 then 1
  else fib(n-2) + fib(n-1)
  end
end
")

;----------------------------------------------------------
(test-section "signature")

(test* "REXP_SIGNATURE should match with a signature"
   #t
   (not (not ((string->regexp REXP_SIGNATURE) "foo :: Int -> Int"))))

(test* "REXP_SIGNATURE should not match with a normal line"
   #f
   ((string->regexp REXP_SIGNATURE) "print 'hello world'"))

(test* "sigunature-find should find the signature line"
  "# fib :: Int -> Int"
  (signature-find FIB "#"))

(test* "signature-parse should parse a signature line"
  '("fib" "Int" ("Int"))
  (receive (x y z) (signature-parse "# fib :: Int -> Int" "#") (list x y z)))

;----------------------------------------------------------
(test-section "langdef")

(define *langdef* (langdef-load "../templates/ruby.unb"))
(test* "langdef-load should load ruby.unb"
  `(("stub" . "<FUNCTION_DEFINITION>

class Babel
  def self.to_sexp(x)
    case x
    when Numeric
      x.to_s
    when String
      x.inspect
    when Array
      '(' + x.map{|item| Babel.to_sexp(item)}.join(' ') + ')'
    else
      raise
    end
  end
end
print Babel.to_sexp(<FUNCTION_NAME>(<ARGUMENTS>))")
    ("argtype" . square)
    ("command" . ruby)
    ("comment" . ,(string->symbol "#"))
    ("name" . Ruby))
  *langdef*)

;----------------------------------------------------------
(test-section "runner")

(test* "string-substitute should substitute a pattern for a string"
  "fooBARbaz"
  (string-substitute "foobarbaz" "bar" "BAR"))

(test* "string-substitute* should substitute patterns for strings"
  "FOOBARBAZ"
  (string-substitute* "foobarbaz" "foo" "FOO" "bar" "BAR" "baz" "BAZ"))

(test* "format-arg should format an integer"
  "10"
  (format-arg 10 "Int" 'square))

(test* "format-arg should format a string"
  "\"10\""
  (format-arg "10" "String" 'square))

(test* "format-arg should format a list"
  '("[1, 2, 3]" "(1, 2, 3)" "'(1 2 3)" "'((1 2) (3 4))")
  (list (format-arg '(1 2 3) "[Int]" 'square)
        (format-arg '(1 2 3) "[Int]" 'paren)
        (format-arg '(1 2 3) "[Int]" 'sexp)
        (format-arg '((1 2) (3 4)) "[[Int]]" 'sexp)))

(test* "runner-make-stub should make a stub file"
  #`",|FIB|

class Babel
  def self.to_sexp(x)
    case x
    when Numeric
      x.to_s
    when String
      x.inspect
    when Array
      '(' + x.map{|item| Babel.to_sexp(item)}.join(' ') + ')'
    else
      raise
    end
  end
end
print Babel.to_sexp(fib(10))"
  (runner-make-stub FIB '(10) *langdef*))
  
(test* "runner-run-stub should run a stub script"
  "55"
  (runner-run-stub (runner-make-stub FIB '(10) *langdef*) *langdef*))

(test* "runner-parse-result should parse result of script"
  55
  (runner-parse-result "55" FIB *langdef*))

(test* "runner-parse-result should parse result of script"
  "66"
  (runner-parse-result "\"66\"" "# fib :: Int -> String" *langdef*))

(test* "runner-run should make and run a script"
  55
  (runner-run FIB '(10) *langdef*))

;----------------------------------------------------------
(select-module user)
(test-section "integration")

(test* "unbabel-ruby should call Ruby function" 
  55
  ((unbabel-ruby "
# fib :: Int -> Int
def fib(n)
  case n
  when 0 then 0
  when 1 then 1
  else fib(n-2) + fib(n-1)
  end
end") 10))

(test-end)
