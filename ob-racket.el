;;; ob-racket.el --- Racket SRC blocks support for Org

;;; Commentary:
;;
;; Some support for "racket" blocks in Org mode Babel.

(require 'ob)

;;; Code:

(eval-when-compile
  (defvar org-babel-tangle-lang-exts))
(add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt"))

(defvar org-babel-default-header-args:racket nil
  "Default header arguments for a Racket SRC block.")

(defun org-babel-expand-body:racket (body params)
  "Expand BODY according to PARAMS, and return the expanded BODY.
The expansion assumes `define-values` and `values` to be
available with default Racket semantics."
  (let* ((result-type (cdr (assoc :result-type params)))
	 (pro (or (cdr (assoc :prologue params))
		  (when (eq 'value result-type)
		    "#lang racket\n")))
	 (epi (cdr (assoc :epilogue params)))
	 (var-defs
	  (let ((vars (org-babel--get-vars params)))
	    (when (> (length vars) 0)
	      (list
	       (concat
		"(define-values ("
		(mapconcat (lambda (var) (format "%s" (car var))) vars " ")
		") (values "
		(mapconcat (lambda (var) (format "%S" (cdr var))) vars " ")
		"))"))))))
    (mapconcat #'identity
	       (append (when pro
			 (list (ob-racket-expand-fmt pro)))
		       var-defs
		       (list body)
		       (when epi
			 (list (ob-racket-expand-fmt epi))))
	       "\n")))

(defun ob-racket-expand-fmt (fmt &optional params)
  "Expand a format list FMT, and return a string.
Substitute symbols according to the PARAMS alist. The FMT
argument may be a string, in which case it is returned as is."
  (if (stringp fmt)
      fmt
    (mapconcat
     (lambda (x)
       (cond
	((stringp x) x)
	((eq x 'ln) "\n")
	((eq x 'quot) "\"")
	((eq x 'apos) "\'")
	((symbolp x)
	 (let ((p (cdr (assq x params))))
	   (unless p
	     (error "Key %s not in %S" x params))
	   (format "%s" p)))
	(t (error "Expected string or symbol: %S" fmt))))
     fmt "")))

(defun org-babel-execute:racket (body params)
  "Evaluate a `racket` code block.
Evaluate the block BODY according to PARAMS.

Evaluation assumes `let' and `write' to be available with
default Racket semantics.

Some custom header arguments are supported for extra
control over how the evaluation is to happen.
These are:
- :eval-file <pathname> (file for code to evaluate)
- :cmd <shell-command> (defaults to '(\"racket -u\" eval-file))
- :eval-fun <lam-expr> (as: in-fn out-fn -> result-string)

The `shell-command` may also be a list of strings that
will be concatenated; the list may also contain one of
the following symbols:
- `eval-file`, replaced with source pathname
- `obj-file`, replaced with any target \"file\" pathname

For more control, the :eval-fun parameter may specify
a lambda expression to define how to process the block.
As special cases, :eval-fun may be specified as:
- \"body\", to have the result be the bare BODY content
- \"code\", to have the result be the expanded code
- \"file\", to have the result name a file containing the code"
  (let* ((eval-file
	  (or (cdr (assoc :eval-file params))
	      (org-babel-temp-file "org-babel-" ".rkt")))
         (result-type (cdr (assoc :result-type params)))
	 (v-body (cond
		  ((eq 'value result-type)
		   ;; any :prologue and :epilogue code goes to top-level
		   (format "(write (let () %s))" body))
		  ((eq 'output result-type)
		   body)
		  (t
		   (error "Expected :results of `output` or `value`"))))
	 ;; `full-body` will have any :prologue and :epilogue.
	 (full-body (org-babel-expand-body:racket v-body params))
	 (eval-fun (cdr (assoc :eval-fun params))))
    (cond
     ((equal eval-fun "body")
      body)
     ((equal eval-fun "code")
      full-body)
     ((equal eval-fun "debug")
      (format "params=%S" params))
     (t
      (with-temp-file eval-file
	(insert full-body))
      (cond
       ((equal eval-fun "file")
	(org-babel-process-file-name eval-file t))
       (t
	(let* ((in-fn
		(org-babel-process-file-name eval-file t))
	       (obj-file (cdr (assoc :file params)))
	       (out-fn
		(and obj-file
		     (org-babel-process-file-name obj-file t)))
	       (exec-f
		(function
		 (lambda (cmd)
		   (message cmd)
		   (shell-command-to-string cmd)))))
	  (cond
	   ((not eval-fun)
	    (let ((sh-cmd
		   (let ((cmd-fmt
			  (or (cdr (assoc :cmd params))
			      '("racket -u " eval-file)))
			 (fmt-par
			  `((eval-file
			     . ,(shell-quote-argument in-fn))
			    (obj-file
			     . ,(and out-fn
				     (shell-quote-argument out-fn))))))
		     (ob-racket-expand-fmt cmd-fmt fmt-par))))
	      (message sh-cmd)
	      (shell-command-to-string sh-cmd)))
	   ((listp eval-fun)
	    (funcall (eval eval-fun t) in-fn out-fn))
	   (t
	    (error "Expected lambda expression for :eval-fun"))))))))))

(defun org-babel-prep-session:racket (session params)
  "Error out due to lack of support.
SESSION and PARAMS are ignored."
  (error "Sessions not yet supported for `racket`"))

(provide 'ob-racket)

;;; ob-racket.el ends here
