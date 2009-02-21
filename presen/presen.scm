;;;
;;; presen.scm
;;;
;;; usage:
;;;  (load "presen.scm")
;;;  (define *presen*
;;;    '#(("hello"
;;;        ("foo"
;;;         "bar))
;;;       ("good bye"
;;;        ("thank you"
;;;         "very much"))))
;;;  (presen-start)
;;;
;;; history:
;;;  Version 1: ajax-reload
;;;  Version 0: initial version

(define (make-open-tag name props)
  (string-append 
    "<" 
    name
    (apply string-append
           (map (lambda (s) (string-append " " s)) props))
    ">"))

(define (wrap-by-tag tag content)
  (if (pair? tag)
    (string-append (make-open-tag (car tag) (cdr tag))
                   content
                   "</" (car tag) ">")
    (string-append "<" tag ">" 
                   content
                   "</" tag ">")))
 
(define (make-bq item)
  (wrap-by-tag "blockquote" item))
(define (make-li item)
  (wrap-by-tag "li" item))
(define (make-ul items)
  (wrap-by-tag "ul" (string-concat (map make-li items))))

(define (make-title title)
  (if (pair? title)
    (wrap-by-tag "h1" 
      (string-append 
        (car title)
        (wrap-by-tag '("div" " class='english'") (cdr title))))
    (wrap-by-tag "h1" title)))
 
(define (make-page title items)
  (string-append (make-title title)
                 (if (= (length items) 1)
                   (make-bq (car items))
                   (make-ul items))))

(define (presen-show n)
  (set-content! ($ "num") (number->string n))
  (let1 data (vector-ref *presen* n)
    (let ((title (car data))
          (items (cdr data)))
      (set-content! ($ "presen") 
        (make-page title items)))))

(define *num* 0)
(define *file* #f)

(define (presen-last-page)
  (- (vector-length *presen*) 1))

(define (presen-first-page)
  0)

(define (presen-page n)
  (set-content! ($ "bs-console") "") ;;temporary..
  (set! *num* (if (< n 0)
                0
                (if (<= (vector-length *presen*) n)
                  (- (vector-length *presen*) 1)
                  n)))
  (presen-show *num*))

(define (presen-reload n)
  (load *file*)
  (presen-show n))

(define (presen-start file)
  (set! *file* file)
  (load *file*)
  (presen-show *num*))

(define *last-undo* #f)
(define (presen-last! n)
  (if (= n (presen-last-page))
    (when *last-undo* (presen-page *last-undo*))
    (begin
      (set! *last-undo* n)
      (presen-page (presen-last-page)))))

(define *first-undo* #f)
(define (presen-first! n)
  (if (= n (presen-first-page))
    (when *first-undo* (presen-page *first-undo*))
    (begin
      (set! *first-undo* n)
      (presen-page (presen-first-page)))))

;;; events

(add-handler! ($ "head") "click"
  (lambda (e)
    (presen-page 0)))

(add-handler! ($ "next") "click"
  (lambda (e) 
    (presen-page (+ *num* 1))))

(add-handler! ($ "num") "click"
  (lambda (e)
    ;(load "presen.scm")
    (presen-reload *num*)))

(define *document* (js-eval "document"))

(add-handler! *document* "keydown"
  (lambda (e)
    (case (js-ref e "which")
      ((39 40 74) ; right
       (presen-page (+ *num* 1)))
      ((37 38 75) ; left
       (presen-page (- *num* 1)))
      ((82) ; r
       (presen-reload *num*))
      ((79) ; o
       (repl-toggle))
      ((65) ; a
       (presen-first! *num*))
      ((90) ; z
       (presen-last! *num*))
      ;(else (print (js-ref e "which")))
      )
    #f))

(add-handler! ($ "back") "click"
  (lambda (e) 
    (presen-page (- *num* 1))))

(add-handler! ($ "last") "click"
  (lambda (e)
    (presen-page (- (vector-length *presen*) 1))))

;;; repl :-)

(define (repl-toggle)
  (element-toggle! ($ "repl")))

(add-handler! ($ "repl-toggle") "click"
  (lambda (e)
    (repl-toggle)))

(add-handler! ($ "repl-eval") "click"
  (lambda (e)             
    (set-content! ($ "repl-dst") 
                  (html-escape (write-to-string (eval (get-content ($ "repl-src"))))))
    #f))

;;; utilities

(define (lines s)
  (string-split s "\n"))

(define (n-indent-of line)
  (let1 m (regexp-exec (string->regexp "\\s*") line)
    (if m
      (string-length (car m))
      0)))

(define (trim-first line n)
  (substring line n (string-length line)))

(define (unindent s)
  (let* ((ls (lines s))
         (n0 (n-indent-of (car ls)))
         (n (if (= n0 0) (n-indent-of (cadr ls)) n0)))
    (apply string-append
      (if (= n0 0) (car ls) (trim-first (car ls) n))
      "\n"
      (intersperse "\n"
                   (map (lambda (line)
                          (trim-first line n))
                        (cdr ls))))))

