;;; Sepia -- Simple Emacs-Perl InterAction: ugly, yet effective.
;;; (a.k.a. Septik -- Sean's Emacs-Perl Total Integration Kludge.)

;; Copyright (C) 2004 Sean O'Rourke.  All rights reserved, some wrongs
;; reversed.  This code is distributed under the same terms as Perl
;; itself.

;;; Commentary:

;; See the README file that comes with the distribution.

;;; Code:

(require 'perl)
(require 'epl)
(require 'generic-repl)
(require 'cperl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Xrefs -- use Perl to find definitions and uses.

(defvar sepia-use-completion t
  "* Use completion based on Xref database.  Turning this off may
speed up some operations, if you don't mind losing completion.")

(defvar sepia-eval-defun-include-decls t
  "* Generate and use a declaration list for ``sepia-eval-defun''.
Without this, code often will not parse; with it, evaluation may
be a bit less responsive.  Note that since this only includes
subs from the evaluation package, it may not always work.")

(defvar sepia-prefix-key "\M-."
  "* Prefix for functions in ``sepia-keymap''.")

(defvar sepia-root "~/src/perl/sepia/"
  "* Location of Sepia support files.")



(defvar sepia-keymap
  (let ((km (make-sparse-keymap)))
    (dolist (kv '(("c" . sepia-callers)
		  ("C" . sepia-callees)
		  ("v" . sepia-var-uses)
		  ("V" . sepia-var-defs)
;;		  ("V" . sepia-var-assigns)
;; 		  ("\M-." . sepia-dwim)
		  ("\M-." . sepia-location)
		  ("d" . sepia-w3m-perldoc-this)
		  ("f" . sepia-defs)
		  ("r" . sepia-rebuild)
		  ("m" . sepia-module-find)
		  ("n" . sepia-next)))
      (define-key km (car kv) (cdr kv)))
    km)
  "Keymap for Sepia functions.  This is just an example of how you
might want to bind your keys, which works best when bound to
`\\M-.'.")

(defun sepia-install-keys (&optional map)
"Install Sepia bindings in the current local keymap."
  (interactive)
  (let ((map (or map (current-local-map))))
    (define-key map sepia-prefix-key sepia-keymap)
    (define-key map "\M-," 'sepia-next)
    (define-key map "\C-\M-x" 'sepia-eval-defun)
    (define-key map "\C-c\C-l" 'sepia-eval-buffer)
    (define-key map "\C-c\C-d" 'sepia-w3m-view-pod)))

(defun perl-name (sym &optional mod)
  (cond
    ((symbolp sym) (substitute ?_ ?- (symbol-name sym)))
    (mod (format "%s::%s" mod sym))
    (t sym)))

;;;###autoload
(defun sepia-init ()
"Perform the initialization necessary to start Sepia, a set of
tools for developing Perl in Emacs.

The following keys are bound to the prefix
``sepia-prefix-key'' (`\\M-.' by default), which can be changed
by setting ``sepia-prefix'' before calling ``sepia-init'':

\\{sepia-keymap}

In addition to these keys, Sepia defines the following keys,
which may conflict with keys in your setup, but which are
intended to shadow similar functionality in elisp-mode:

`\\C-c\\C-d'        ``sepia-w3m-view-pod''
`\\C-c\\C-l'        ``sepia-eval-buffer''
`\\C-\\M-x'         ``sepia-eval-defun''
`\\M-,'             ``sepia-next'' (shadows ``tags-loop-continue'')
"
  (interactive)

  (ignore-errors
    (kill-process "perl")
    (setq perl-interpreter nil))
  (epl-init)
    ;; Load perl defs:

  (perl-eval (format "BEGIN { push @INC, \"%s\" };
use Emacs::Lisp;
use Data::Dumper;
require Sepia;
require Xref;" sepia-root) 'void-context)

  ;; Create glue wrappers for Module::Info funcs.
  (dolist (x '((name "Find module name.  Does not require loading.")
	       (version "Find module version.  Does not require loading.")
	       (inc-dir
"Find directory in which this module was found.  Does not require loading.")
	       (file
"Absolute path of file defining this module.  Does not require loading.")
	       (is-core
"Guess whether or not a module is part of the core distribution.
Does not require loading.")
	       (modules-used
"List modules used by this module.  Requires loading.")
	       (packages-inside
"List sub-packages in this module.  Requires loading.")
	       (superclasses
"List module's superclasses.  Requires loading.")))
    (apply #'define-modinfo-function x))

  ;; Create low-level wrappers for Sepia
  (dolist (x '((completions "Find completions in the symbol table.")
               (location "Find an identifier's location.")
	       (mod-subs "Find all subs defined in a package.")
	       (apropos "Find subnames matching RE.")
               ))
    (apply #'define-xref-function "Sepia" x))

  (dolist (x '((rebuild "Build Xref database for current Perl process.")
	       (redefined "Rebuild Xref information for a given sub.")

	       (callers "Find all callers of a function.")
	       (callees "Find all functions called by a function.")

	       (var-apropos "Find varnames matching RE.")
	       (mod-apropos "Find modules matching RE.")
	       (file-apropos "Find files matching RE.")

	       (var-defs "Find all definitions of a variable.")
	       (var-assigns "Find all assignments to a variable.")
	       (var-uses "Find all uses of a variable.")

	       (mod-redefined "Rebuild Xref information for a given package.")
	       (mod-files "Find the file defining a package.")
	       (mod-decls "Generate declarations for subs in a package.")
	       (guess-module-file "Guess file corresponding to module.")
	       (file-modules "List the modules defined in a file.")))
    (apply #'define-xref-function "Sepia::Xref" x))
  (add-hook 'cperl-mode-hook 'sepia-install-eldoc)
  (add-hook 'cperl-mode-hook 'sepia-doc-update)
  (add-hook 'sepia-repl-hook 'sepia-repl-init-syntax)
  (add-hook 'sepia-repl-hook 'sepia-install-eldoc)
  (if (boundp 'cperl-mode-map)
      (sepia-install-keys cperl-mode-map))
  (sepia-interact))

(defun define-xref-function (package name doc)
  "Define a lisp mirror for a low-level Sepia function."
  (let ((lisp-name (intern (format "xref-%s" name)))
	(pl-name (format "%s::%s" package (perl-name name))))
    (fmakunbound lisp-name)
    (eval `(defun ,lisp-name (&rest args)
	     ,doc
	     (apply #'perl-call ,pl-name 'list-context args)))))

(defun define-modinfo-function (name &optional doc)
"Define a lisp mirror for a function from Module::Info."
  (let ((name (intern (format "sepia-module-%s" name)))
	(pl-func (perl-name name))
	(full-doc (concat (or doc "") "

This function uses Module::Info, so it does not require that the
module in question be loaded.")))
    (when (fboundp name) (fmakunbound name))
    (eval `(defun ,name (mod)
	     ,full-doc
	     (interactive (list (sepia-interactive-arg 'module)))
             (sepia-maybe-echo
              (perl-call "Sepia::module_info" 'scalar-context
                         mod ,pl-func))))))

(defun sepia-thing-at-point (what)
  "Like ``thing-at-point'', but hacked to avoid REPL prompt."
  (let ((th (thing-at-point what)))
    (and th (not (string-match "[ >]$" th)) th)))

(defun sepia-interactive-arg (&optional type)
"Default argument for most Sepia functions.  TYPE is a symbol --
either 'file to look for a file, or anything else to use the
symbol at point."
  (let* ((default (case type
		    (file (or (thing-at-point 'file) (buffer-file-name)))
		    (t (sepia-thing-at-point 'symbol))))
	 (text (capitalize (symbol-name type)))
	 (choices (lambda (str &rest blah)
		    (let ((str (concat "^" str)))
		      (case type
			(variable (xref-var-apropos str))
			(function (xref-apropos str))
			(module (xref-mod-apropos str))
			(t nil)))))
	 (ret (if sepia-use-completion
		  (completing-read (format "%s [%s]: " text default)
				   choices nil nil nil 'sepia-history
				   default)
		  (read-string (format "%s [%s]: " text default)
			       nil 'sepia-history default))))
    (push ret sepia-history)
    ret))

(defun sepia-interactive-module ()
"Guess which module we should look things up in.  Prompting for a
module all the time is a PITA, but I don't think this (choosing
the current file's module) is a good alternative, either.  Best
would be to choose the module based on what we know about the
symbol at point."
  (let ((xs (xref-file-modules (buffer-file-name))))
    (if (= (length xs) 1)
	(car xs)
	nil)))

(defun sepia-maybe-echo (result)
  (when (interactive-p)
    (message "%s" result))
  result)

(defun sepia-module-find (mod)
"Find the file defining module MOD."
  (interactive (list (sepia-interactive-arg 'module)))
  (let ((fn (or (sepia-module-file mod)
		(xref-guess-module-file mod))))
    (when fn
      (message "Module %s in %s." mod fn)
      (pop-to-buffer (find-file-noselect (expand-file-name fn))))))

(defmacro ifa (test then &rest else)
  `(let ((it ,test))
     (if it ,then ,@else)))

(defun sepia-show-locations (locs)
  (when locs
    (pop-to-buffer (get-buffer-create "*sepia-places*"))
    (erase-buffer)
    (dolist (loc (sort locs (lambda (a b)
			      (or (string< (car a) (car b))
				  (and (string= (car a) (car b))
				       (< (second a) (second b)))))))
      (destructuring-bind (file line name &rest blah) loc
	(let ((str (ifa (find-buffer-visiting file)
			(with-current-buffer it
			  (ifa sepia-found-refiner
			       (funcall it line name)
			       (goto-line line))
			  (message "line for %s was %d, now %d" name line
				   (line-number-at-pos))
			  (setq line (line-number-at-pos))
                          (let ((tmpstr
                                 (buffer-substring (my-bol-from (point))
                                                   (my-eol-from (point)))))
                            (if (> (length tmpstr) 60)
                                (concat "\n    " tmpstr)
                                tmpstr)))
			"...")))
	  (insert (format "%s:%d:%s\n" (abbreviate-file-name file) line str)))))
    (grep-mode)
    (goto-char (point-min))))

(defun sepia-filter-by-module (x)
  "Filter to limit hits by module only."
  (when (or (not module) (string= module (fourth x)))
    (list x)))

(defun sepia-filter-by-all (x)
  "Filter to limit hits by module and file."
  (when (and (or (not module) (string= module (fourth x)))
	     (or (not file) (string= file (first x))))
    (list x)))

(defmacro define-sepia-query (name doc &optional gen test prompt)
  `(defun ,name (ident &optional module file line display-p)
     ,(concat doc "

With prefix arg, list occurences in a ``grep-mode'' buffer.
Without, place the occurrences on ``sepia-found'', so that
calling ``sepia-next'' will cycle through them.

Depending on the query, MODULE, FILE, and LINE may be used to
narrow the results, as long as doing so leaves some matches.
When called interactively, they are taken from the current
buffer.
")
     (interactive (list (sepia-interactive-arg ,(or prompt ''function))
			(sepia-interactive-module)
			(buffer-file-name)
			(line-number-at-pos (point))
			current-prefix-arg
			))
     (let ((ret
	    ,(if test
		 `(let ((tmp (,gen ident module file line)))
		    (or (mapcan #',test tmp) tmp))
		 `(,gen ident module file line))))
       ;; Always clear out the last found ring, because it's confusing
       ;; otherwise.
       (sepia-set-found nil ',(or prompt 'function))
       (if display-p
	   (sepia-show-locations ret)
	   (sepia-set-found ret ',(or prompt 'function))
	   (sepia-next)))))

(defun sepia-location (name &optional jump-to)
  (interactive (list (or (thing-at-point 'symbol)
                         (completing-read "Function: " 'xref-completions))
                     t))
  (let* ((fl (or (car (xref-location name))
                 (car (apply #'xref-location (xref-apropos name))))))
    (when (and fl (string-match "^(eval " (car fl)))
      (message "Can't find definition of %s in %s." name (car fl))
      (setq fl nil))
    (if jump-to
        (if fl (progn
                 (sepia-set-found (list fl))
                 (sepia-next))
            (message "No definition for %s." name))
        fl)))

;;;###autoload
(defun sepia-dwim (&optional display-p)
    "Try to DWIM:
* Find all definitions, if thing-at-point is a function
* Find all uses, if thing-at-point is a variable
* Find all definitions, if thing-at-point is a module
* Prompt otherwise
"
    (interactive "P")
    (multiple-value-bind (obj mod type raw) (sepia-ident-at-point)
      (if type
	  (progn
	    (sepia-set-found nil type)
	    (let ((ret (ecase type
			 (function (list (sepia-location raw)))
			 (variable (xref-var-uses raw))
			 (module `((,(car (xref-mod-files mod)) 1 nil nil))))))
	      (if display-p
		  (sepia-show-locations ret)
		  (sepia-set-found ret type)
		  (sepia-next))))
	  (call-interactively 'sepia-defs))))

(define-sepia-query sepia-defs
    "Find all definitions of sub."
  xref-apropos
  xref-location)

(define-sepia-query sepia-callers
    "Find callers of FUNC."
  xref-callers
  xref-location)

(define-sepia-query sepia-callees
    "Find a sub's callees."
  xref-callees
  xref-location)

(define-sepia-query sepia-var-defs
    "Find a var's definitions."
  xref-var-defs
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-sepia-query sepia-var-uses
    "Find a var's uses."
  xref-var-uses
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-sepia-query sepia-var-assigns
    "Find/list assignments to a variable."
  xref-var-assigns
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-sepia-query sepia-module-describe
    "Find all subroutines in a package."
  xref-mod-subs
  nil
  'module)

(defalias 'sepia-package-defs 'sepia-module-describe)

(define-sepia-query sepia-apropos
    "Find/list subroutines matching regexp."
  (lambda (name &rest blah) (xref-apropos name 1))
  xref-location
  'function)

(define-sepia-query sepia-var-apropos
    "Find/list variables matching regexp."
  xref-var-apropos
  xref-var-defs
  'variable)

(defun sepia-rebuild ()
  "Rebuild the Xref database."
  (interactive)
  (xref-rebuild))

;;;###autoload
(defun sepia-load-file (file rebuild-p)
  "Reload a file, possibly rebuilding the Xref database.  When
called interactively, reloads the current buffer's file, and
rebuilds the database unless a prefix argument is given."
  (interactive (list (buffer-file-name) (not prefix-arg)))
  (perl-load-file file)
  (if rebuild-p
      (xref-rebuild)))

(defvar sepia-found)
(defvar sepia-found-head)
(defvar sepia-found-refiner)
(defvar sepia-history nil)

(defun sepia-set-found (list &optional type)
  (setq list
	(remove-if (lambda (x)
		     (and (not (car x)) (string= (fourth x) "main")))
		   list))
  (setq sepia-found list
	sepia-found-head list)
  (setq sepia-found-refiner (sepia-refiner type)))

(defun sepia-refiner (type)
  (case type
    (function
     (lambda (line ident)
      (let ((sub-re (concat "^\\s *sub\\s +.*" ident "\\>")))
	;; Test this because sometimes we get lucky and get the line
	;; just right, in which case beginning-of-defun goes to the
	;; previous defun.
	(unless (looking-at sub-re)
	  (or (and line
		   (progn
		     (goto-line line)
		     (beginning-of-defun)
		     (looking-at sub-re)))
	      (progn (goto-char (point-min))
		     (re-search-forward sub-re nil t)))
	  (beginning-of-line)))))
    ;; Old version -- this may actually work better if
    ;; beginning-of-defun goes flaky on us.
;; 	   (or (re-search-backward sub-re
;; 				   (my-bol-from (point) -20) t)
;; 	       (re-search-forward sub-re
;; 				  (my-bol-from (point) 10) t))
;; 	   (beginning-of-line)
    (variable
     (lambda (line ident)
       (let ((var-re (concat "\\<" ident "\\>")))
	 (cond
	   (line (goto-line line)
		 (or (re-search-backward var-re (my-bol-from (point) -5) t)
		     (re-search-forward var-re (my-bol-from (point) 5) t)))
	   (t (goto-char (point-min))
	      (re-search-forward var-re nil t))))))
    (t (lambda (line ident) (and line (goto-line line))))))

(defun sepia-next ()
"Go to the next thing (e.g. def, use) found by sepia."
  (interactive)
  (if sepia-found
      (destructuring-bind (file line short &optional mod &rest blah)
	  (car sepia-found)
	(unless file
	  (setq file (and mod (sepia-module-file mod)))
	  (if file
	      (setf (caar sepia-found) file)
	      (error "No file for %s." (car sepia-found))))
	(message "%s at %s:%s" short file line)
        (when (file-exists-p file)
          (find-file (or file (car (xref-mod-files mod))))
          (when sepia-found-refiner
            (funcall sepia-found-refiner line short))
          (beginning-of-line)
          (recenter)
          (setq sepia-found (or (cdr sepia-found)
                                (progn
                                  (message "sepia: no more defs.")
                                  sepia-found-head)))))
      (message "No more definitions.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

(defun sepia-ident-at-point ()
  "Find the perl identifier at point, returning
\(values OBJECT MODULE TYPE RAW), where TYPE is either 'variable,
'function, or 'module.  If TYPE is 'module, OBJ is the last
component of the module name."
  (let ((cperl-under-as-char nil)
	(case-fold-search nil)
	var-p module-p modpart objpart)
    (condition-case c
	(destructuring-bind (sbeg . send) (bounds-of-thing-at-point 'symbol)
	  (destructuring-bind (wbeg . wend) (or (bounds-of-thing-at-point 'word)
						(cons (point) (point)))
	    (if (member (char-before send) '(?> ?\ ))
		(signal 'wrong-number-of-arguments 'sorta))
	    (setq var-p
		  (or (member (char-before wbeg) '(?@ ?$ ?%))
		      (member (char-before sbeg) '(?@ ?$ ?%))))
	    (setq module-p
		  (save-excursion (goto-char wbeg) (looking-at "[A-Z]")))
	    (setq modpart 
		  (if (= sbeg wbeg)
		      nil
		      (buffer-substring sbeg
					(if (= (char-before (1- wbeg)) ?\:)
					    (- wbeg 2)
					    (1- wbeg)))))
	    (setq objpart (buffer-substring wbeg wend))
	    (values (if module-p
			(list objpart modpart (if var-p 'variable 'function))
			objpart)
		    (if module-p
			(buffer-substring sbeg send)
			modpart)
		    (cond
		      (module-p 'module)
		      (var-p 'variable)
		      (t 'function))
                    (buffer-substring sbeg send))))
      (wrong-number-of-arguments (values nil nil nil)))))

(defun delete-thing-at-point (sym)
  (destructuring-bind (beg . end) (bounds-of-thing-at-point sym)
    (delete-region beg end)))

(defun sepia-complete-symbol ()
"Try to complete the word at point:
    * as a global variable, if it has a sigil (sorry, no lexical
      var completion).
    * as a module, if its last namepart begins with an uppercase
      letter.
    * as a function, otherwise.
The function currently ignores module qualifiers, which may be
annoying in larger programs.

The function is intended to be bound to \\M-TAB, like
``lisp-complete-symbol''."
  (interactive)
  (let ((tap (or (thing-at-point 'symbol)
                 (and (eq last-command 'sepia-complete-symbol) ""))))
    (if tap
        (let ((completions (xref-completions tap (sepia-buffer-package))))
          (case (length completions)
            (0 (message "No completions for %s." tap))
            (1 (delete-thing-at-point 'symbol)
               (insert (car completions)))
            (t (let ((old (thing-at-point 'symbol))
                     (new (try-completion "" completions)))
                 (if (string= new old)
                     (with-output-to-temp-buffer "*Completions*"
                       (display-completion-list completions))
                     (delete-thing-at-point 'symbol)
                     (insert new))))))
        (message "sepia: empty -- hit tab again to complete."))))

(defun sepia-indent-or-complete ()
"Indent the current line and, if indentation doesn't move point,
complete the symbol around point.  This function is intended to
be bound to TAB."
  (interactive)
  (let ((pos (point)))
    (cperl-indent-command)
    (when (and (= pos (point))
	       (eq last-command 'sepia-indent-or-complete))
      (sepia-complete-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scratchpad code

;;;###autoload
(defun sepia-scratchpad ()
"Create a buffer to interact with a Perl interpreter.  The buffer
is placed in cperl-mode; calling ``sepia-scratch-send-line'' will
evaluate the current line and display the result."
  (interactive)
  (switch-to-buffer (get-buffer-create "*perl-interaction*"))
  (cperl-mode)
  (local-set-key "\C-j" 'sepia-scratch-send-line))

(defun sepia-scratch-send-line (&optional scalarp)
"Send the current line to perl, and display the result."
  (interactive "P")
  (insert
   (sepia-eval (concat "do{"
		       (buffer-substring (my-bol-from (point))
					 (my-eol-from (point)))
		       "}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellany

(defun my-perl-frob-region (pre post beg end replace-p)
  (let* ((exp (concat pre "\""
		      (shell-quote-argument (buffer-substring beg end))
		      "\"" post))
	 (new-str (format "%s" (perl-eval exp 'scalar-context))))
    (if replace-p
	(progn (delete-region beg end)
	       (goto-char beg)
	       (insert new-str))
	(message new-str))))

(defun my-eol-from (pt &optional n)
  (save-excursion
    (goto-char pt)
    (end-of-line n)
    (point)))

(defun my-bol-from (pt &optional n)
  (save-excursion
    (goto-char pt)
    (beginning-of-line n)
    (point)))

(defun perl-pe-region (expr beg end &optional replace-p)
"Do the equivalent of perl -pe on region (i.e. evaluate an
expression on each line of region).  With prefix arg, replace the
region with the result."
  (interactive "MExpression:\nr\nP")
  (my-perl-frob-region
   "{ my $ret='';my $region = "
   (concat "; for (split /\n/, $region) { do { " expr
	   ";}; $ret.=\"$_\\n\"}; $ret}")
   (my-bol-from beg) (my-eol-from end) replace-p))
  
(defun perl-ize-region (expr beg end &optional replace-p)
"Evaluate a Perl expression on the region as a whole.  With
prefix arg, replace the region with the result."
  (interactive "MExpression:\nr\nP")
  (my-perl-frob-region "{ local $_ = "
		       (concat "; do { " expr ";}; $_ }")
		       beg end replace-p))

(defun sepia-guess-package (sub &optional file)
  "Guess which package SUB is defined in."
  (let ((defs (xref-apropos sub)))
    (or (and (= (length defs) 1)
	     (or (not file) (equal (caar defs) file))
	     (fourth (car defs)))
	(and file
	     (fourth (find-if (lambda (x) (equal (car x) file)) defs)))
	(car (xref-file-modules file))
	(sepia-buffer-package))))

;;;###autoload
(defun sepia-eval-defun ()
  "Re-evaluate the current sub in the appropriate package, and
rebuild its Xrefs."
  (interactive)
  (save-excursion
    (let ((beg (progn (beginning-of-defun) (point)))
	  (end (progn (end-of-defun) (point))))
      (goto-char beg)
      (when (looking-at "^sub\\s +\\([^ 	{]+\\)")
	(let* ((sub (match-string 1))
	       (sepia-eval-package
		(sepia-guess-package sub (buffer-file-name)))
	       (body (buffer-substring-no-properties beg end))
	       (sepia-eval-file (buffer-file-name))
	       (sepia-eval-line (line-number-at-pos beg)))
	  (sepia-eval (if sepia-eval-defun-include-decls
			  (concat
			   (apply #'concat (xref-mod-decls sepia-eval-package))
			   body)
			  body))
	  (xref-redefined sub sepia-eval-package)
	  (message "Defined %s" sub))))))

(defun sepia-extract-def (file line obj mod)
  (with-current-buffer (find-file-noselect (expand-file-name file))
    (save-excursion
      (funcall (sepia-refiner 'function) line obj)
      (beginning-of-line)
      (when (looking-at (concat "^\\s *sub\\>.*\\<" obj "\\>"))
	(buffer-substring (point)
			  (progn (end-of-defun) (point)))))))

(defun sepia-eval-no-run (string)
  (condition-case err
      (sepia-eval
       (concat "BEGIN { use B; B::minus_c(); $^C=1; } { "
               string
               "} BEGIN { die \"ok\\n\" }"))
    (perl-error (if (string-match "^ok\n" (cadr err))
                    t
                    (cadr err)))
    (error err)))

;;;###autoload
(defun sepia-eval-buffer (&optional no-update)
  "Re-evaluate the current file; unless prefix argument is given,
also rebuild the xref database."
  (interactive)
  (let ((sepia-eval-file (buffer-file-name)))
    (sepia-eval-no-run (buffer-string))
    (unless no-update
      (xref-rebuild))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL

(defvar sepia-eval-package "main"
  "Package in which ``sepia-eval'' evaluates perl expressions.")
(defvar sepia-eval-file nil
  "File in which ``sepia-eval'' evaluates perl expressions.")
(defvar sepia-eval-line nil
  "Line at which ``sepia-eval'' evaluates perl expressions.")
(defvar sepia-repl-hook nil
  "Hook run after Sepia REPL starts.")

(defun sepia-repl-init-syntax ()
  (local-unset-key ":")
  (set-syntax-table cperl-mode-syntax-table)
  (modify-syntax-entry ?> "."))

(defun sepia-set-eval-package (new-package)
  (setq sepia-eval-package new-package))

(defun sepia-get-eval-package ()
  sepia-eval-package)

(defun sepia-eval (string &optional discard)
  "Evaluate STRING as Perl code, returning the pretty-printed
value of the last expression.  If SOURCE-FILE is given, use this
as the file containing the code to be evaluated.  XXX: this is
the only function that requires EPL (the rest can use Pmacs)."
  (epl-eval (epl-init) nil 'scalar-context
  (concat
 "{ package " (or sepia-eval-package "main") ";"
 (if sepia-eval-file (concat "$Sepia::Xref::file = \"" sepia-eval-file "\";")
     "")
 (if sepia-eval-line (format "$Sepia::Xref::line = %d;" sepia-eval-line)
     "")
 (if discard
     (concat string "; '' }\n")
     (concat
      "require Data::Dumper;"
;;       "local $Data::Dumper::Indent=0;"
      "local $Data::Dumper::Deparse=1;"
      "local $_ = Data::Dumper::Dumper([do { " string "}]);"
      "s/^.*?=\\s*\\[//; s/\\];$//;$_}")))))

;;;###autoload
(defun sepia-interact ()
"Start or switch to a perl interaction buffer."
  (interactive)
  (unless (get-buffer "*perl-interaction*")
    (generic-repl "perl"))
  (pop-to-buffer (get-buffer "*perl-interaction*")))

(defun sepia-repl-header ()
  (let ((proc (aref perl-interpreter 2)))
    (format "%s [id=%d,d=%d,nr=%d] (%s)"
	    (process-name proc)
	    (process-id proc)
	    (aref perl-interpreter 7)
	    (aref perl-interpreter 5)
	    (process-status proc))))

(defun sepia-set-repl-dir ()
  (interactive)
  (repl-cd default-directory "perl"))

(defun sepia-set-cwd (dir)
  (perl-call "chdir" dir))

(defun sepia-input-complete-p (beg end)
  (and (> end beg)
       (let ((res (sepia-eval-no-run (buffer-substring beg end))))
         (if (eq res t)
             t
             (message "[%s]" res)
             nil))))

(defun sepia-eval-for-repl (string)
  (sepia-eval string (string-match ";\\s *$" string)))

(unless (assoc "perl" repl-supported-modes)
  (push '("perl"
          :map cperl-mode-map
          :eval sepia-eval-for-repl
          :complete sepia-complete-symbol
          :header sepia-repl-header
          :cd sepia-set-cwd
          :init (lambda () (run-hooks 'sepia-repl-hook))
          :comment-start "#"
          :get-package sepia-get-eval-package
          :expression-p sepia-input-complete-p
          :set-package sepia-set-eval-package)
	repl-supported-modes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doc-scanning

(defvar sepia-doc-map (make-hash-table :test #'equal))
(defvar sepia-var-doc-map (make-hash-table :test #'equal))
(defvar sepia-module-doc-map (make-hash-table :test #'equal))

;; (defvar sepia-use-long-doc t
;;   "Gather additional docs from POD following =item to report with eldoc.")

(defun sepia-doc-scan-buffer ()
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward
		 "^=\\(item\\|head2\\)\\s +\\([%$&@A-Za-z_].*\\)" nil t)
       if (let* ((s1 (match-string 2))
		 (s2 (let ((case-fold-search nil))
		       (replace-regexp-in-string
			"[A-Z]<\\([^>]+\\)>"
			(lambda (x) (match-string 1 s1)) s1)))
		 (longdoc
		  (let ((beg (progn (forward-line 2) (point)))
			(end (1- (re-search-forward "^=" nil t))))
		    (forward-line -1)
		    (goto-char beg)
		    (if (re-search-forward "^\\(.+\\)$" end t)
			(concat s2 ": "
				(substring-no-properties
				 (match-string 1)
				 0 (position ?. (match-string 1))))
			s2))))
	    (cond
	      ;; e.g. "C<foo(BLAH)>" or "$x = $y->foo()"
	      ((string-match "\\(\\sw+\\)\\s *\\($\\|(\\)" s2)
	       (list 'function (match-string-no-properties 1 s2)
		     (or (and (equal s2 (match-string 1 s2)) longdoc) s2)))
	      ;; e.g. "$x -- this is x" (note: this has to come second)
	      ((string-match "^[%$@]\\([^( ]+\\)" s2)
	       (list 'variable (match-string-no-properties 1 s2) longdoc))))
       collect it)))

(defun sepia-buffer-package ()
  (save-excursion
    (goto-char (point-min))
    (or (and (re-search-forward "^\\s *package\\s +\\([^ ;]+\\)" nil t)
	     (match-string-no-properties 1))
	"main")))

(defun sepia-doc-update ()
"Update documentation for a file.  This documentation, taken from
\"=item\" entries in the POD, is used for eldoc feedback."
  (interactive)
  (let ((pack (ifa (or
		    (car (xref-file-modules (buffer-file-name)))
		    (sepia-buffer-package))
		   (concat it "::")
		   "")))
    (dolist (x (sepia-doc-scan-buffer))
      (let ((map (ecase (car x)
		   (function sepia-doc-map)
		   (variable sepia-var-doc-map))))
	(puthash (second x) (third x) map)
	(puthash (concat pack (second x)) (third x) map)))))

(defun sepia-symbol-info ()
"Eldoc function for Sepia-mode.  Looks in ``sepia-doc-map'' and
``sepia-var-doc-map'', then tries calling
``cperl-describe-perl-symbol''."
  (save-excursion
    (multiple-value-bind (obj mod type) (sepia-ident-at-point)
      (or (and type
	       (let ((map (ecase type
			    (function sepia-doc-map)
			    (variable sepia-var-doc-map)
			    (module sepia-module-doc-map))))
		 (if mod
                     (gethash mod map)
		     (gethash obj map))))
	  (and obj
	       ;; Loathe cperl a bit.
	       (flet ((message (&rest blah) (apply #'format blah)))
		 (let* ((cperl-message-on-help-error nil)
			(hlp (car (cperl-describe-perl-symbol obj))))
		   (when hlp
		     ;; cperl's docstrings are too long.
		     (setq hlp (replace-regexp-in-string "\\s \\{2,\\}" "  " hlp))
		     (if (> (length hlp) 75)
			 (concat (substring hlp 0 72) "...")
			 hlp)))))
	  ""))))

(defun sepia-install-eldoc ()
"Install Sepia hooks for eldoc support (probably requires Emacs >= 21.3)."
  (interactive)
  (set (make-variable-buffer-local
	'eldoc-print-current-symbol-info-function)
       #'sepia-symbol-info)
  (if cperl-lazy-installed (cperl-lazy-unstall))
  (eldoc-mode 1)
  (setq eldoc-idle-delay 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error jump:

(defun sepia-extract-next-warning (pos &optional end)
  (when (and (re-search-forward "^\\(.+\\) at \\(.+\\) line \\([0-9]+\\)\\.$"
				end t)
	     (not (string= "(eval " (substring (match-string 2) 0 6))))
    (list (match-string 2)
	  (read-from-string (match-string 3))
	  (msg (match-string 1)))))

(defun sepia-goto-error-at (pos)
  "Visit the source of the error on line at POS (point if called
interactively)."
  (interactive "d")
  (ifa (sepia-extract-warning (my-bol-from pos) (my-eol-from pos))
       (destructuring-bind (file line msg) it
	 (find-file file)
	 (goto-line line)
	 (message "%s" msg))
       (error "No error to find.")))

(defun sepia-perl-display-errors (beg end)
  (interactive "r")
  (goto-char beg)
  (loop with msgs = (make-hash-table :test #'equal)
     for w = (sepia-extract-warning (my-bol-from (point)) end)
     while w
     do (destructuring-bind (file line msg) w
	  (puthash file msgs (cons line msg)))
     finally
       (with-current-buffer (get-buffer-create "*perl-warnings*")
	 (let ((inhibit-read-only t))
	   (erase-buffer))
	 (dolist (k (sort (hash-table-keys msgs) #'string<))
	   (let ((v (gethash k msgs)))
	     (insert (format "%s:%d:\n%s\n"
			     (abbreviate-file-name k) (car v) (cdr v)))))
	 (goto-char (point-min))
	 (grep-mode))))

(provide 'sepia)
;;; sepia.el ends here
