(define-module unbabel
  (use gauche.process) ; process-output->string
  (use file.util) ; file->string-list, directory-list
  (use util.list) ; assoc-ref
  (use srfi-1) ; take-while!, alist-cons
  (use srfi-8) ; receive
  (use srfi-13) ; string-trim-both
  (export 
    unbabel-init))
(select-module unbabel)
;-----------------------------------------------------module start

;-----------------------------------------------------signature

(define REXP_TYPE "[\\[\\]\\w]+")
(define REXP_SIGNATURE 
  #`"(,|REXP_TYPE|)\\s*::\\s*(,|REXP_TYPE|(\\s*->\\s*,|REXP_TYPE|)*)\\s*$")

(define (signature-rexp comment)
  (string->regexp #`",|comment|\\s*,|REXP_SIGNATURE|"))

(define (signature-find src comment)
  (find (lambda (line) (rxmatch (signature-rexp comment) line))
        (string-split src "\n")))

(define (signature-parse line comment)
  (rxmatch-let ((signature-rexp comment) line) 
               (all func-name arg-types)
    (let ((types (map string-trim-both 
                      (string-split arg-types "->"))))
      (values func-name
              (last types)
              (reverse (cdr (reverse types)))))))

(define (signature-of src langdef)
  (let ((comment (assoc-ref langdef "comment")))
    (signature-parse (signature-find src comment) comment)))

;-----------------------------------------------------langdef

(define (langdef-parse-line line)
  (let ((tokens (string-split line ":")))
    (values (car tokens)
            (string-join (cdr tokens) ":"))))

(define (langdef-load path)
  (let loop ((lines (file->string-list path))
             (props '()))
    (receive (key value) (langdef-parse-line (car lines))
      (if (string=? key "stub")
        (alist-cons "stub" (string-join (cdr lines) "\n") props)
        (loop (cdr lines) (alist-cons key (string->symbol value) props))))))

;-----------------------------------------------------runner

(define (string-substitute str before after)
  (let* ((start (string-scan str before))
         (end (begin
                (unless start (error #`"string-substitude: \",|before|\" not found in \",|str|\""))
                (+ start (string-length before)))))
    (string-replace str after start end)))

(define-syntax string-substitute*
  (syntax-rules ()
    ((_ str before after)
      (string-substitute str before after))
    ((_ str before after . patterns)
      (string-substitute* (string-substitute str before after)
                          . patterns))))

(define (format-arg arg type argtype . outer?)
  (define (parens-of argtype)
    (case argtype
      ((square) (values "[" ", " "]"))
      ((paren)  (values "(" ", " ")"))
      ((sexp)   (values (if (null? outer?) "'(" "(") " " ")"))
      (else (error (format "unknown argtype: ~s" argtype)))))

  (cond ((and (number? arg) (or (string=? type "Int") (string=? type "Float")))
          (number->string arg))
        ((and (string? arg) (string=? type "String"))
          (write-to-string arg))
        ((and (list? arg) (string-prefix? "[" type))
          (receive (pre between post) (parens-of argtype)
            (let ((type (substring type 1 (- (string-length type) 1))))
              (string-append 
                pre 
                (string-join (map (cut format-arg <> type argtype #f) arg) between)
                post))))
        (else
          (error #`"type mismatch: ,|type| wanted but got ,|arg|"))))

(define (format-args args arg-types argtype)
  (map (lambda (arg type)
         (format-arg arg type argtype))
       args
       arg-types))

(define (runner-make-stub src args langdef)
  (receive (func-name return-type arg-types) (signature-of src langdef)
    (string-substitute* (assoc-ref langdef "stub")
                        "<FUNCTION_DEFINITION>" src
                        "<FUNCTION_NAME>" func-name
                        "<ARGUMENTS>" (string-join (format-args args arg-types 
                                                      (assoc-ref langdef "argtype")) 
                                                   ", "))))
                        ;; TODO: see argtype

(define (runner-run-stub stub langdef)
  (call-with-process-io (symbol->string (assoc-ref langdef "command"))
    (lambda (stdout stdin)
      (display stub stdin)
      (close-output-port stdin)
      (port->string stdout))))

(define (runner-parse-result result src langdef)
  (read-from-string result))
;; TODO: type check by signature
;  (receive (func-name return-type arg-types) (signature-of src langdef)
;    (cond
;      ((string=? return-type "Int")
;       (string->number result))
;      ((string=? return-type "String")
;       result))))

(define (runner-run src args langdef)
  (when (eq? langdef #f)
    (error #`"bad langdef: ,|langdef|"))
  (let ((stub (runner-make-stub src args langdef)))
    (runner-parse-result (runner-run-stub stub langdef)
                         src langdef)))

;-----------------------------------------------------integration
(define *langdefs* '())

(define (define-lang name langdef)
  (let ((func-name (string->symbol #`"unbabel-,|name|")))
    (set! *langdefs* (alist-cons func-name langdef *langdefs*))
    (eval `(define (,func-name src)
             (lambda (arg . rest)
               (runner-run src (cons arg rest) (assoc-ref *langdefs* ',func-name))))
          (current-module))
    (eval `(export ,func-name) (current-module))))

(define (unbabel-init dir)
  (for-each (lambda (path)
              (let* ((langdef (langdef-load path))
                     (orig-name (assoc-ref langdef "name"))
                     (name (if orig-name (string-downcase (symbol->string orig-name))
                             (error #`"name not found in ,path"))))
                (define-lang name langdef)))
            (directory-list dir :filter (pa$ string-suffix? ".unb")
                                :add-path? #t)))

;-----------------------------------------------------module end
(provide "unbabel")
