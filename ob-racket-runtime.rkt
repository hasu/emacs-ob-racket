#lang racket/base

#|
|#

(provide ob-racket-begin-print-elisp
         ob-racket-begin-print-table)

(require racket/dict racket/extflonum racket/math
         racket/set racket/string
         compatibility/mlist)

;;; 
;;; Racket data to Emacs Lisp.
;;; 

(define-syntax-rule
  (ob-racket-begin-print-elisp body ...)
  (display
   (datum->elisp
    (let ()
      body ...))))

(define (char-code->string num)
  (string (integer->char num)))

;; Characters not needing escaping in Emacs Lisp character, symbol, or string syntax.
(define (safe-ascii-code->string num)
  (cond
    ((= num 33) "!")
    ((= num 36) "$")
    ((= num 37) "%")
    ((= num 38) "&")
    ((= num 42) "*")
    ((= num 43) "+")
    ((= num 45) "-")
    ((= num 47) "/")
    ((<= 48 num 57) ;; 0-9
     (char-code->string num))
    ((= num 58) ":")
    ((= num 60) "<")
    ((= num 61) "=")
    ((= num 62) ">")
    ((= num 63) "?")
    ((= num 64) "@")
    ((<= 65 num 90) ;; A-Z
     (char-code->string num))
    ((= num 91) "[")
    ((= num 93) "]")
    ((= num 94) "^")
    ((= num 95) "_")
    ((<= 97 num 122) ;; a-z
     (char-code->string num))
    ((= num 123) "{")
    ((= num 125) "}")
    ((= num 126) "~")
    (else #f)))

(define (shorthand-escaped-ascii-code->string num)
  (cond
    ((= num 7) "\\a")
    ((= num 8) "\\b")
    ((= num 9) "\\t")
    ((= num 10) "\\n")
    ((= num 11) "\\v")
    ((= num 12) "\\f")
    ((= num 13) "\\r")
    ((= num 27) "\\e")
    ((= num 127) "\\d")
    (else #f)))

(define (char->elisp ch)
  (define num (char->integer ch))
  (cond
    ((safe-ascii-code->string num)
     => (lambda (str) (string-append "?" str)))
    ((shorthand-escaped-ascii-code->string num)
     => (lambda (str) (string-append "?" str)))
    ((= num 32) "\\s")
    ((= num 34) "?\\\"")
    ((= num 35) "?\\#")
    ((= num 39) "?\\'")
    ((= num 40) "?\\(")
    ((= num 41) "?\\)")
    ((= num 44) "?\\,")
    ((= num 46) "?\\.")
    ((= num 59) "?\\;")
    ((= num 92) "?\\\\")
    ((= num 96) "?\\`")
    ((= num 124) "?\\|")
    ((> num 4194303)
     ;; Not an Emacs Lisp character.
     (format "~s" ch))
    (else
     ;; Print everything else as a number.
     ;; In Emacs Lisp, characters are just numbers.
     (number->string num))))

(define (string-pad s up-to-len pad-ch)
  (define len (string-length s))
  (if (>= len up-to-len)
      s
      (let ((n (- up-to-len len)))
        (string-append (make-string n pad-ch) s))))

(define (u-escape v)
  (string-append "\\u" (string-pad (format "~x" v) 4 #\0)))

(define (U-escape v)
  (string-append "\\U00" (string-pad (format "~x" v) 6 #\0)))

(define (string-char->elisp ch)
  (define num (char->integer ch))
  (cond
    ((shorthand-escaped-ascii-code->string num)
     => (lambda (str) str))
    ((= num 34) "\\\"")
    ((= num 92) "\\\\")
    ((<= num 127) (string ch))
    ((<= num #xffff)
     (u-escape num))
    ((<= num #x10ffff)
     ;; Maximum Unicode code point, so we cannot use Unicode escapes for anything larger.
     (U-escape num))
    ((<= num 4194303)
     ;; These codes are still allowed for characters in Emacs multibyte strings.
     (format "\\x~x\\ " num))
    (else
     ;; Not an Emacs Lisp character.
     (format "~s" ch))))

(define (string->elisp str)
  (string-append "\""
                 (apply string-append
                        (map string-char->elisp (string->list str)))
                 "\""))

(define (symbol-escape str)
  (regexp-replace*
   #rx"([^a-zA-Z0-9+=*/_~!@$%^&:<>{}?-])"
   str
   "\\\\\\1"))

(define (symbol->elisp sym)
  (define str (symbol->string sym))
  (cond
    ((string=? str "")
     ;; Special syntax for the empty symbol.
     "\\#\\#")
    ((regexp-match? #rx"^[0-9]" str)
     (string-append "\\" (symbol-escape str)))
    (else
     (symbol-escape str))))

(define (number->elisp num)
  (cond
    ((exact-integer? num)
     ;; Emacs Lisp supports integers, with machine-dependent range.
     ;; Out of range integers may be treated as being of the floating point type.
     (format "~v" num))
    ((not (real? num))
     ;; Emacs Lisp does not support imaginary numbers.
     (format "~s" num))
    ((nan? num)
     (if (negative? num)
         "-0.0e+NaN"
         "0.0e+NaN"))
    ((infinite? num)
     (if (negative? num)
         "-1.0e+INF"
         "1.0e+INF"))
    ((flonum? num)
     ;; Emacs Lisp supports floating point numbers. Flonums should
     ;; `print' in an Emacs Lisp compatible way, favoring shorter
     ;; representations.
     (format "~v" num))
    ((single-flonum? num)
     ;; Single-precision numbers print in a way that is incompatible
     ;; with Emacs Lisp, but we can convert to something else.
     (format "~a" (exact->inexact (inexact->exact num))))
    (else
     ;; Emacs Lisp does not support exact rational number syntax, so
     ;; convert to something else.
     (format "~a" (exact->inexact num)))))

(define (datum->elisp datum)
  (cond
    ((boolean? datum)
     (if datum "t" "nil"))
    ((box? datum)
     (datum->elisp (list (unbox datum))))
    ((bytes? datum)
     (datum->elisp (bytes->list datum)))
    ((char? datum)
     (char->elisp datum))
    ((keyword? datum)
     (symbol->elisp (string->symbol (string-append ":" (keyword->string datum)))))
    ((null? datum)
     "nil")
    ((list? datum)
     (string-append "(" (string-join (map datum->elisp datum) " ") ")"))
    ((mpair? datum)
     ;; Emacs Lisp pairs actually are mutable, so this is appropriate.
     (datum->elisp (cons (mcar datum) (mcdr datum))))
    ((and (extflonum-available?) (extflonum? datum))
     ;; An extflonum is not a number, but we can turn it into one.
     (number->elisp (extfl->exact datum)))
    ((number? datum)
     (number->elisp datum))
    ((pair? datum)
     (format "(~a . ~a)" (datum->elisp (car datum)) (datum->elisp (cdr datum)))) 
    ((or (set? datum) (set-mutable? datum) (set-weak? datum))
     (datum->elisp (set->list datum)))
    ((string? datum)
     (string->elisp datum))
    ((symbol? datum)
     (symbol->elisp datum))
    ((syntax? datum)
     (datum->elisp (syntax->datum datum)))
    ((vector? datum)
     (string-append "[" (string-join (map datum->elisp (vector->list datum)) " ") "]"))
    ((void? datum)
     "nil")
    ((dict? datum)
     ;; Emacs Lisp would also have hash tables with exotic #s(....) read syntax.
     (datum->elisp (dict->list datum)))
    (else
     ;; Unsupported data type: use Racket string representation, which is at least `read'able.
     (datum->elisp (format "~s" datum)))))

;;; 
;;; Racket data to Org Babel table.
;;; 

(define-syntax-rule
  (ob-racket-begin-print-table body ...)
  (display
   (datum->table
    (let ()
      body ...))))

(define (datum->table datum)
  (cond
    ((list? datum)
     (string-append "(" (string-join (map datum->table datum) " ") ")"))
    ((pair? datum)
     (datum->table (list (car datum) (cdr datum))))
    ((mlist? datum)
     (datum->table (mlist->list datum)))
    ((mpair? datum)
     (datum->table (list (mcar datum) (mcdr datum))))
    ((and (extflonum-available?) (extflonum? datum))
     (datum->elisp datum))
    ((number? datum)
     (datum->elisp datum))
    ((or (set? datum) (set-mutable? datum) (set-weak? datum))
     (datum->table (set->list datum)))
    ((symbol? datum)
     (datum->elisp datum))
    ((syntax? datum)
     (datum->table (syntax->datum datum)))
    ((vector? datum)
     (datum->table (vector->list datum)))
    ((dict? datum)
     (datum->table (dict->list datum)))
    (else
     (datum->elisp (format "~s" datum)))))

#|

Copyright (C) 2019 the authors.

Authors: Tero Hasu

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#
