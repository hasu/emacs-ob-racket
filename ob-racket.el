;;; ob-racket.el --- Racket SRC block support for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2019, 2020, 2021 the authors
;;
;; Authors: Tero Hasu
;; Homepage: https://github.com/hasu/emacs-ob-racket

;;; Commentary:
;;
;; Support for "racket" language blocks in Org mode Babel.
;;
;; Since Racket is itself such a customizable language, the Racket
;; Babel support defined here also features customization options and
;; mechanisms.
;;
;; As usual for Babel, the default SRC block header arguments for
;; "racket" blocks may be specified by setting
;; `org-babel-default-header-args:racket'.
;;
;; There are "racket" specific options that may be set. For cases
;; where a header argument does not specify the Racket language,
;; `ob-racket-default-lang' specifies the default language to use. The
;; default command used for evaluating "racket code blocks is
;; specified by `ob-racket-default-command'. For determining whether a
;; block of code already contains a #lang specifier, the
;; `ob-racket-hash-lang-regexp' is used. The variable
;; `ob-racket-preprocessor-function' determines the specifics of how
;; the code and the header arguments are affected by the presence of a
;; #lang line.
;;
;; To further customize the behavior, you may provide custom templates
;; to use for constructing Racket programs and shell commands to run
;; for evaluating the code. The variables
;; `ob-racket-custom-code-templates' and
;; `ob-racket-custom-command-templates' exist for that purpose. The
;; templates also for the most part determine which header arguments
;; affect SRC block handling. The default templates assume that
;; `define-values`, `values`, `let`, and `write` have their `racket`
;; language semantics.
;;
;; As templates may be specified as functions, it is possible for them
;; to do unsafe things. Org's standard `org-babel-safe-header-args'
;; facility may be used to indicate what header arguments are safe to
;; set, and with what values, given the set of templates in use.

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'ob)

;;; Code:

(eval-when-compile
  (defvar org-babel-tangle-lang-exts))
(add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt"))

(defvar org-babel-default-header-args:racket '((:session . "none"))
  "Default header arguments for a Racket SRC block.")

(defvar ob-racket-custom-code-templates nil
  "Code template overrides and additions.
A list of the form ((SYMBOL . TEMPLATE) ...). See
`ob-racket-expand-template' for details on how templates are
expanded.")

(defvar ob-racket-custom-command-templates nil
  "Command template overrides and additions.
A list of the form ((SYMBOL . TEMPLATE) ...). See
`ob-racket-expand-template' for details on how templates are
expanded.")

(defvar ob-racket-default-lang "racket"
  "Default #lang Racket language.
By default (as specified in `ob-racket-default-code-templates'),
this default #lang gets used when no :lang parameter is given for
a code block, and when the body of the code block does not appear
to already contain a #lang line (the `ob-racket-hash-lang-regexp'
pattern is used to look for it).")

(defvar ob-racket-default-command
  '(spaced racket "-u" (file :in-file) cmdline)
  "Template for command to execute.
May be overridden for a code block with the :command header
argument. As with some other Babel languages, a :cmdline header
argument may be used to specify arguments to the executed
script.")

(defvar ob-racket-hash-lang-regexp
  "^[[:space:]]*#lang[[:space:]]+\\(.+\\)$"
  "A regexp for finding \"#lang\" from body.
The regular expression should match the entire #lang line. The
first parenthesized expression should give the language name. To
prevent such ad-hoc search of a #lang specifier, either set this
regexp to nil, or give a :lang header argument for each SRC
block.")

(defvar ob-racket-debug nil
  "Whether to dump or preserve information.
The symbol `dump-params' means to dump parameters. The symbol
`dump-code' means to dump code template expansion results without
evaluating. The symbol `dump-command' means the same, but for
command templates. The symbol `keep-file' means to evaluate, but
leave the source file intact. The symbol `dump-result' means to
dump the result of code block evaluation without any (table,
list, etc.) formatting.")

(defvar ob-racket-preprocessor-function
  (lambda (body params)
    (when (and ob-racket-hash-lang-regexp
	       (not (assq :lang params))
	       (not (assq :lang-line params))
	       (not (assq :prologue params)))
      (save-match-data
	(when (let ((case-fold-search nil))
		(string-match ob-racket-hash-lang-regexp body))
	  (let* ((lang-line (match-string 0 body))
		 (result-type (cdr (assq :result-type params)))
		 (is-value-type (eq 'value result-type)))
	    (if is-value-type
		(setq params (cons (cons :lang-line lang-line) params)
		      body (replace-match "" t t body))
	      (setq params (cons (cons :lang-line nil) params)))))))
    (list body params))
  "A function for preprocessing the block.
If this variable is non-nil, it is used for preprocessing the SRC
block content and the header arguments right before calling
`ob-racket-expand-template' with the program template to arrive
at the complete Racket source file content to evaluate. Any
function set to this variable should accept a (BODY PARAMS)
argument list, and also return a '(BODY PARAMS) list.")

(defvar ob-racket-locate-runtime-library-function
  (lambda () (locate-library "ob-racket-runtime.rkt" t))
  "A function to locate a Racket-based runtime library.
Should be a function returning the full file path of a Racket
module. When this variable is set to nil, or when the function
returns nil, no conversion takes place.")

(defvar ob-racket-pre-runtime-library-load-hook nil
  "Hook run once before loading runtime library.
That is, all the hook functions are run once before first using
any ob-racket Racket runtime library, as known to
`ob-racket-locate-runtime-library-function'.")

(defvar ob-racket-have-run-pre-runtime-hook nil
  "Whether the pre-runtime hook has been run.
Set to a non-nil value once an attempt has been made to run
`ob-racket-pre-runtime-library-load-hook'.")

(defvar ob-racket-default-code-templates
  `(
    (program
     . ,(lambda (_env params)
	  (let* ((result-type (cdr (assq :result-type params)))
		 (is-value-type (eq 'value result-type)))
	    (if is-value-type
		(if (cdr (assq :elisp-printing-form params))
		    'elisp-value-program
		  'value-program)
	      'output-program))))
    (require-runtime-library
     . ,(lambda (_env params)
	  (format "%S" `(require (file ,(cdr (assq :runtime-library params)))))))
    (elisp-value-program
     . (prologue
	"\n" require-runtime-library
	"\n" "(" :elisp-printing-form
	"\n" define-vars
	"\n" :body ")"
	"\n" epilogue))
    (value-program
     . (prologue
	"\n" "(write (let ()"
	"\n" define-vars
	"\n" :body "))"
	"\n" epilogue))
    (output-program
     . (prologue
	"\n" define-vars
	"\n" :body
	"\n" epilogue))
    (prologue
     . (lang-line "\n"
	requires))
    (epilogue . nil)
    (lang-line
     . ,(lambda (_env params)
	  (let ((p (assq :lang params)))
	    (when (or (not p) (cdr p))
	      (let ((lang (or (cdr p) ob-racket-default-lang)))
		(when lang
		  `(spaced "#lang" ,lang)))))))
    (requires
     . ,(lambda (_env params)
	  (let ((req (assq :require params)))
	    (when req
	      `(parens (spaced "require" ,(cdr req)))))))
    (define-vars
      . ,(lambda (env params)
	   (let ((vars (org-babel--get-vars params)))
	     (when vars
	       (if (ob-racket-get-template 'define-var env params)
		   `(lines
		     ,@(mapcar
			(lambda (var)
			  `(parameterize
			    (:name ,(car var))
			    (parameterize
			     (:value ,(cdr var))
			     define-var)))
			vars))
		 `(parens
		   (spaced
		    "define-values"
		    (parens
		     (spaced ,@(mapcar
				(lambda (var)
				  `(parameterize
				    (:name ,(car var))
				    var-name))
				vars)))
		    (parens
		     (spaced "values"
			     ,@(mapcar
				(lambda (var)
				  `(parameterize
				    (:value ,(cdr var))
				    var-value))
				vars))))))))))
    (var-name
     . ,(lambda (_env params)
	  (symbol-name (cdr (assq :name params)))))
    (var-value
     . ,(lambda (_env params)
	  (let ((in-racket (equal "racket" (cdr (assq :vars-are params))))
		;; In this case we assume that `val' is a non-template,
		;; and something we may immediately coerce into a string.
		(val (cdr (assq :value params))))
	    (if (and in-racket (stringp val))
		;; Already a Racket expression.
		val
	      ;; May not format as Racket, but most
	      ;; other Babel language definitions
	      ;; also do not seem to do much else.
	      (format "%S" val)))))
    (with-define . (parens (spaced "define" var-name var-value)))
    )
  "Default code templates.
A list of the form ((SYMBOL . TEMPLATE) ...). See
`ob-racket-expand-template' for details on how templates are
expanded.")

(defvar ob-racket-default-command-templates
  `(
    (cmdline . nil)
    (command . ,(lambda (_env _params)
		  ob-racket-default-command))
    (racket . "racket")
    )
  "Default command templates.
A list of the form ((SYMBOL . TEMPLATE) ...). See
`ob-racket-expand-template' for details on how templates are
expanded.")

(defun ob-racket-get-template (name env params)
  "Get the definition of template NAME.
The name must be given as the symbol, not the keyword. If it has
no template definition, return nil. Both the environment ENV and
the header PARAMS are checked."
  (or (assq name env)
      (assq (intern (concat ":" (symbol-name name))) params)))

(defun ob-racket-expand-sexp (template env params)
  "Expand an S-expression, and return a string.
Like `ob-racket-expand-template', but regards TEMPLATE as an
S-expression representing a Racket program fragment. The symbol
table ENV is not referenced directly. As with templates, it is
possible to include Emacs Lisp keywords naming one of the PARAMS.
For now, do nothing special to pretty print symbols and literals
in a Racket reader compatible format. Datums not acceptable to
the Emacs Lisp reader can be escaped as (as-is STRING), causing
the STRING to be included as is."
  (pcase template
    (`(as-is ,(and (pred stringp) str))
     str)
    ((pred listp)
     (concat "("
	     (mapconcat
	      (lambda (template)
		(ob-racket-expand-sexp template env params))
	      template " ")
	     ")"))
    ((pred stringp)
     (format "%S" template))
    ((pred keywordp)
     (ob-racket-expand-template template env params))
    (_
     (format "%s" template))))

(defun ob-racket-expand-template (template env params)
  "Expand a TEMPLATE, and return a string.
The TEMPLATE argument may be a string, a symbol naming a template
in environment ENV, a keyword naming one of the PARAMS, a list of
such elements, or a special form named by its first symbol. As a
special case, if the keyword variant of a symbol names one of the
PARAMS, then that parameter is used instead of anything in ENV."
  (pcase template
    (`nil "") ;; equivalent to (concat)
    ((pred stringp) template)
    ((pred keywordp)
     (let ((p (assq template params)))
       (unless p
	 (error "No assignment for parameter %s in %S" template params))
       (ob-racket-expand-template (cdr p) env params)))
    ((pred symbolp)
     (let ((param-name (intern (concat ":" (symbol-name template)))))
       (if (assq param-name params)
	   (ob-racket-expand-template param-name env params)
	 (let ((p (assq template env)))
	   (unless p
	     (error "No definition for template %s in %S" template env))
	   (let* ((template (cdr p))
		  (template (if (functionp template)
				(funcall template env params)
			      template)))
	     (ob-racket-expand-template template env params))))))
    (`(concat . ,rest) ;; special form
     (mapconcat
      (lambda (template)
	(ob-racket-expand-template template env params))
      rest ""))
    (`(lines . ,rest) ;; special form
     (mapconcat
      (lambda (template)
	(ob-racket-expand-template template env params))
      rest "\n"))
    (`(spaced . ,rest) ;; special form
     (mapconcat
      (lambda (template)
	(ob-racket-expand-template template env params))
      rest " "))
    (`(join ,sep . ,rest) ;; special form
     (mapconcat
      (lambda (template)
	(ob-racket-expand-template template env params))
      rest
      (ob-racket-expand-template sep env params)))
    (`(parens . ,rest) ;; special form
     (ob-racket-expand-template `(concat "(" ,@rest ")") env params))
    (`(sexp . ,rest) ;; special form
     (ob-racket-expand-sexp rest env params))
    (`(format ;; special form
       ,(and (pred stringp) fmt)
       . ,(and (pred (lambda (xs)
		       (and (listp xs) (cl-every #'keywordp xs))))
	       args))
     (apply #'format fmt (mapcar
			  (lambda (x)
			    (let ((p (assq x params)))
			      (unless p
				(error "No assignment for parameter %s in %S"
				       x params))
			      (cdr p)))
			  args)))
    (`(file ,name) ;; special form
     (org-babel-process-file-name
      (ob-racket-expand-template name env params)))
    (`(parameterize ;; special form
       (,(and (pred keywordp) x) ,v)
       . ,rest)
     (ob-racket-expand-template rest env (cons (cons x v) params)))
    ((pred listp) ;; implicit default special form
     (ob-racket-expand-template (cons 'concat template) env params))
    (_
     (error "Unsupported template form: %S" template))))

(defun ob-racket-dump-if (mode data &optional as-text)
  "If in MODE, dump DATA.
If mode is the one specified by `ob-racket-debug', then dump
data, optionally AS-TEXT, and do a non-local return. Otherwise do
nothing."
  (when (eq mode ob-racket-debug)
    (error (concat "DEBUG: %s:\n" (if as-text "%s" "%S"))
	   mode data)))

(defun org-babel-expand-body:racket (body params)
  "Expand BODY according to PARAMS, and return the result."
  (when ob-racket-preprocessor-function
    (pcase (funcall ob-racket-preprocessor-function body params)
      (`(,b ,p) (setq body b params p))))
  (let ((templates (append ob-racket-custom-code-templates
			   ob-racket-default-code-templates))
	(params (cons (cons :body body) params)))
    (ob-racket-expand-template 'program templates params)))

(defun org-babel-execute:racket (body params)
  "Evaluate a `racket` code block.
Evaluate the block BODY according to PARAMS. Some non-standard,
subject-to-change parameters are supported for more control over
the way that the code block is evaluated, and how its `:var's and
results are processed."
  (ob-racket-dump-if 'dump-params params)
  (let* ((result-type (cdr (assq :result-type params)))
	 (is-value-type (eq 'value result-type))
	 (result-params
	  (let ((p (cdr (assq :result-params params))))
	    (if (or (member "file" p)
		    (member "list" p)
		    (member "scalar" p)
		    (member "table" p)
		    (member "vector" p)
		    (member "verbatim" p))
		p (cons "scalar" p))))
	 (runtime-library
	  (and is-value-type
	       ob-racket-locate-runtime-library-function
	       (funcall ob-racket-locate-runtime-library-function)))
	 (elisp-printing-form
	  (when runtime-library
            (unless ob-racket-have-run-pre-runtime-hook
              (setq ob-racket-have-run-pre-runtime-hook t)
              (run-hooks 'ob-racket-pre-runtime-library-load-hook))
	    (cond
	     ((equal "elisp" (cdr (assq :results-as params)))
	      "ob-racket-begin-print-elisp")
	     ((or (member "list" result-params)
		  (member "table" result-params)
		  (member "vector" result-params))
	      "ob-racket-begin-print-table"))))
	 (params `((:runtime-library . ,runtime-library)
		   (:elisp-printing-form . ,elisp-printing-form)
		   (:result-params . ,result-params)
		   ,@params))
	 (full-body (org-babel-expand-body:racket body params)))
    (ob-racket-dump-if 'dump-code full-body t)
    (let* ((in-file
	    (or (cdr (assq :in-file params))
		(org-babel-temp-file "org-babel-" ".rkt")))
	   (params `((:in-file . ,in-file)
		     ,@params))
	   (templates
	    (append ob-racket-custom-command-templates
		    ob-racket-default-command-templates))
	   (command
	    (ob-racket-expand-template 'command templates params)))
      (ob-racket-dump-if 'dump-command command t)
      (with-temp-file in-file
	(insert full-body))
      (prog1
	  (progn
	    (message command)
	    (let ((result (shell-command-to-string command)))
	      (ob-racket-dump-if 'dump-result result t)
	      (org-babel-result-cond result-params
		result
		(org-babel-reassemble-table
		 (if elisp-printing-form
		     (read result)
		   (org-babel-read result))
		 (org-babel-pick-name (cdr (assq :colname-names params))
				      (cdr (assq :colnames params)))
		 (org-babel-pick-name (cdr (assq :rowname-names params))
				      (cdr (assq :rownames params)))))))
	(unless (eq ob-racket-debug 'keep-file)
	  (delete-file in-file))))))

(defun org-babel-prep-session:racket (_session _params)
  "Error out due to lack of support.
SESSION and PARAMS are ignored."
  (error "Sessions are not supported for `racket`"))

(defun ob-racket-raco-make-runtime-library ()
  "Run raco make on the ob-racket Racket runtime library.
To do that run Racket as defined by ob-racket command templates.
Do nothing if there is no library that is known to
`ob-racket-locate-runtime-library-function'."
  (when ob-racket-locate-runtime-library-function
    (let ((runtime-library
           (funcall ob-racket-locate-runtime-library-function)))
      (when runtime-library
        (let* ((in-file
		(org-babel-temp-file "org-babel-" ".rkt"))
	       (command
	        (ob-racket-expand-template
                 'command
                 (append ob-racket-custom-command-templates
		         ob-racket-default-command-templates)
                 `((:in-file . ,in-file)))))
          (with-temp-file in-file
	    (insert
             (ob-racket-expand-template
              `(lines
                "#lang racket/base"
                "(require raco/all-tools)"
                "(define raco-make-spec (hash-ref (all-tools) \"make\"))"
                (parens
                 (spaced
                  "parameterize"
                  (parens
                   (parens
                    (spaced
                     "current-command-line-arguments"
                     (parens (spaced "vector" runtime-library)))))
                  "(dynamic-require (cadr raco-make-spec) #f)")))
              `((runtime-library
                 . ,(lambda (_env _params)
	              (format "%S" runtime-library))))
              nil)))
          (prog1
              (with-temp-buffer
                (shell-command command nil (current-buffer))
                (let ((error-message (buffer-string)))
                  (unless (= 0 (length error-message))
                    (message "Failed to raco make %S: %s"
                             runtime-library error-message))))
            (delete-file in-file)))))))

(provide 'ob-racket)

;;; ob-racket.el ends here
