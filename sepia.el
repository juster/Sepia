;;; Sepia -- Simple Emacs-Perl InterAction: ugly, yet effective.
;;; (a.k.a. Septik -- Sean's Emacs-Perl Total Integration Kludge.)

;; Copyright (C) 2004-2007 Sean O'Rourke.  All rights reserved, some
;; wrongs reversed.  This code is distributed under the same terms as
;; Perl itself.

;;; Commentary:

;; See the README file that comes with the distribution.

;;; Code:

(require 'cperl-mode)
(require 'comint)
(require 'cl)
;; try optional modules, but don't bitch if we fail:
(require 'sepia-w3m nil t)
(require 'sepia-tree nil t)
(require 'sepia-ido nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint communication

(defvar sepia-perl5lib nil
"* Extra PERL5LIB directory for Sepia.pm")

(defvar sepia-program-name "perl"
"* Perl program name.")

(defvar sepia-process nil
"The perl process with which we're interacting.")
(defvar sepia-output nil
"Current perl output for a response to `sepia-eval-raw', appended
to by `perl-collect-output'.")
(defvar sepia-passive-output ""
"Current perl output for miscellaneous user interaction, used to
look for \";;;###\" lisp evaluation markers.")

(defvar sepia-perl-builtins nil
"List of Perl builtins for completion.")

(defun sepia-collect-output (string)
"Collect perl output for `sepia-eval-raw' into sepia-output."
  (setq sepia-output (concat sepia-output string))
  "")

(defun sepia-eval-raw (str)
"Evaluate perl code STR, returning a pair (RESULT-STRING . OUTPUT)."
  (let (ocpof)
    (unwind-protect
         (let ((sepia-output "")
               (start 0))
           (with-current-buffer (process-buffer sepia-process)
             (setq ocpof comint-preoutput-filter-functions
                   comint-preoutput-filter-functions '(sepia-collect-output)))
           (setq str (concat "local $Sepia::stopdie=0;"
                             "local $Sepia::stopwarn=0;"
                             "{ package " (sepia-buffer-package) ";"
                             str " }\n"))
           (comint-send-string sepia-process
                               (concat (format "<<%d\n" (length str)) str))
           (while (not (and sepia-output
                            (string-match "> $" sepia-output)))
             (accept-process-output sepia-process))
           (if (string-match "^;;;[0-9]+\n" sepia-output)
               (cons
                (let* ((x (read-from-string sepia-output
                                            (+ (match-beginning 0) 3)))
                       (len (car x))
                       (pos (cdr x)))
                  (prog1 (substring sepia-output (1+ pos) (+ len pos 1))
                    (setq start (+ pos len 1))))
                (and (string-match ";;;[0-9]+\n" sepia-output start)
                     (let* ((x (read-from-string
                                sepia-output
                                (+ (match-beginning 0) 3)))
                            (len (car x))
                            (pos (cdr x)))
                       (substring sepia-output (1+ pos) (+ len pos 1)))))
               (cons sepia-output nil)))
      (with-current-buffer (process-buffer sepia-process)
        (setq comint-preoutput-filter-functions ocpof)))))

(defun sepia-eval (str &optional context detailed)
"Evaluate STR in CONTEXT (void by default), and return its result
as a Lisp object.  If DETAILED is specified, return a
pair (RESULT . OUTPUT)."
  (let* ((tmp (sepia-eval-raw
               (case context
                 (list-context 
                  (concat "Sepia::tolisp([" str "])"))
                 (scalar-context
                  (concat "Sepia::tolisp(scalar(" str "))"))
                 (t (concat str ";1")))))
         (res (car tmp))
         (errs (cdr tmp)))
    (setq res (if context (car (read-from-string res)) 1))
    (if detailed
        (cons res errs)
        res)))

(defun sepia-call (fn context &rest args)
"Call perl function FN in CONTEXT with arguments ARGS, returning
its result as a Lisp value."
  (sepia-eval (concat fn "(" (mapconcat #'sepia-lisp-to-perl args ", ") ")")
              context))

(defun sepia-watch-for-eval (string)
"Monitor inferior Perl output looking for Lisp evaluation
requests.  The format for these requests is
\"\\n;;;###LENGTH\\nDATA\".  Only one such request can come from
each inferior Perl prompt."
  (setq sepia-passive-output (concat sepia-passive-output string))
  (cond
    ((string-match "^;;;###[0-9]+" sepia-passive-output)
     (when (string-match "^;;;###\\([0-9]+\\)\n\\(?:.\\|\n\\)*\\(\n.*> \\)"
                         sepia-passive-output)
       (let* ((len (car (read-from-string
                         (match-string 1 sepia-passive-output))))
              (pos (1+ (match-end 1)))
              (res (ignore-errors (eval (car (read-from-string
                                              sepia-passive-output pos
                                              (+ pos len)))))))
         (insert (format "%s => %s\n"
                         (substring sepia-passive-output pos (+ pos len)) res))
         (goto-char (point-max))
         (comint-set-process-mark)
         (sepia-eval "''" 'scalar-context)
         (message "%s => %s" (substring sepia-passive-output pos (+ pos len))
                  res)
         (setq sepia-passive-output "")))
     "")
    (t (setq sepia-passive-output "") string)))

(defun sepia-comint-setup ()
"Set up the inferior Perl process buffer."
  (comint-mode)
  (set (make-local-variable 'comint-dynamic-complete-functions)
       '(sepia-complete-symbol comint-dynamic-complete-filename))
  (set (make-local-variable 'comint-preoutput-filter-functions)
       '(sepia-watch-for-eval))
  (set (make-local-variable 'comint-use-prompt-regexp) t)
  (modify-syntax-entry ?: "_")
  (modify-syntax-entry ?> ".")
  (use-local-map (copy-keymap (current-local-map)))
  (sepia-install-keys)
  (local-set-key (kbd "TAB") 'comint-dynamic-complete)
  (local-set-key "\C-a" 'comint-bol)
  (set (make-local-variable 'comint-prompt-regexp)
       "^[^>\n]*> *")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymaps, user variables, setup.

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

(defvar sepia-keymap
  (eval-when (load eval)
    (let ((km (make-sparse-keymap)))
      (dolist (kv '(("c" . sepia-callers)
                    ("C" . sepia-callees)
                    ("a" . sepia-apropos)
                    ("A" . sepia-var-apropos)
                    ("v" . sepia-var-uses)
                    ("V" . sepia-var-defs)
                    ;;		  ("V" . sepia-var-assigns)
                    ("\M-." . sepia-dwim)
                    ;; ("\M-." . sepia-location)
                    ("l" . sepia-location)
                    ("f" . sepia-defs)
                    ("r" . sepia-rebuild)
                    ("m" . sepia-module-find)
                    ("n" . sepia-next)
                    ("t" . find-tag)))
        (define-key km (car kv) (cdr kv)))
      (when (featurep 'sepia-w3m)
        (define-key km "d" 'sepia-w3m-perldoc-this))
      (when (featurep 'sepia-ido)
        (define-key km "j" 'sepia-jump-to-symbol))
      km))
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
    (define-key map "\C-c\C-l" 'sepia-load-file)
    (define-key map "\C-c\C-d" 'sepia-w3m-view-pod)
    (define-key map (kbd "TAB") 'sepia-indent-or-complete)))

(defun perl-name (sym &optional mod)
"Convert a Perl name to a Lisp name."
  (setq sym (substitute ?_ ?- (if (symbolp sym) (symbol-name sym) sym)))
  (if mod
      (concat mod "::" sym)
      sym))

;;;###autoload
(defun sepia-init (&optional noinit)
"Perform the initialization necessary to start Sepia.

The following keys are bound to the prefix
``sepia-prefix-key'' (`\\M-.' by default), which can be changed
by setting ``sepia-prefix'' before calling ``sepia-init'':

\\{sepia-keymap}
In addition to these keys, Sepia defines the following keys,
which may conflict with keys in your setup, but which are
intended to shadow similar functionality in elisp-mode:

`\\C-c\\C-d'        ``sepia-w3m-view-pod''
`\\C-c\\C-l'        ``sepia-load-file''
`\\C-\\M-x'         ``sepia-eval-defun''
`\\M-,'             ``sepia-next'' (shadows ``tags-loop-continue'')
"
  (interactive "P")
  (ignore-errors
    (kill-process "perl")
    (setq sepia-process nil))
  (unless noinit
    ;; Load perl defs:
    (setq sepia-process
          (get-buffer-process
           (comint-exec (get-buffer-create "*perl-interaction*")
                        "perl" sepia-program-name nil
                        (append (and sepia-perl5lib
                                     (mapcar
                                      (lambda (x) (concat "-I" x))
                                      (split-string sepia-perl5lib ":")))
                                '("-MData::Dumper" "-MSepia" "-MSepia::Xref"
                                  "-e" "Sepia::repl(*STDIN)")))))
      (with-current-buffer "*perl-interaction*"
        (sepia-comint-setup))
      (accept-process-output sepia-process 0 1)

  ;; Create glue wrappers for Module::Info funcs.
  (dolist (x '((name "Find module name.\n\nDoes not require loading.")
	       (version "Find module version.\n\nDoes not require loading.")
	       (inc-dir
"Find directory in which this module was found.\n\nDoes not require loading.")
	       (file
"Absolute path of file defining this module.\n\nDoes not require loading.")
	       (is-core
"Guess whether or not a module is part of the core distribution.
Does not require loading.")
	       (modules-used
"List modules used by this module.\n\nRequires loading.")
	       (packages-inside
"List sub-packages in this module.\n\nRequires loading.")
	       (superclasses
"List module's superclasses.\n\nRequires loading.")))
    (apply #'define-modinfo-function x))

  ;; Create low-level wrappers for Sepia
  (dolist (x '((completions "Find completions in the symbol table.")
               (location "Find an identifier's location.")
	       (mod-subs "Find all subs defined in a package.")
	       (mod-decls "Generate declarations for subs in a package.")
	       (mod-file "Find the file defining a package.")
	       (apropos "Find subnames matching RE.")
               (lexicals "Find lexicals for a sub.")
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
	       (guess-module-file "Guess file corresponding to module.")
	       (file-modules "List the modules defined in a file.")))
    (apply #'define-xref-function "Sepia::Xref" x))

  ;; Initialize built hash
  (sepia-init-perl-builtins))
  (add-hook 'cperl-mode-hook 'sepia-install-eldoc)
  (add-hook 'cperl-mode-hook 'sepia-doc-update)
  (add-hook 'cperl-mode-hook 'sepia-cperl-mode-hook)
  (when (boundp 'cperl-mode-map)
    (sepia-install-keys cperl-mode-map))
  (when (boundp 'perl-mode-map)
    (sepia-install-keys perl-mode-map))
  (unless noinit
    (sepia-interact)))

(defun sepia-cperl-mode-hook ()
  (set (make-local-variable 'beginning-of-defun-function)
       'sepia-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'sepia-end-of-defun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Xref

(defun define-xref-function (package name doc)
  "Define a lisp mirror for a low-level Sepia function."
  (let ((lisp-name (intern (format "xref-%s" name)))
	(pl-name (perl-name name package)))
    (fmakunbound lisp-name)
    (eval `(defun ,lisp-name (&rest args)
	     ,doc
	     (apply #'sepia-call ,pl-name 'list-context args)))))

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
              (sepia-call "Sepia::module_info" 'scalar-context
                         mod ,pl-func))))))

(defun sepia-thing-at-point (what)
  "Like ``thing-at-point'', but hacked to avoid REPL prompt."
  (let ((th (thing-at-point what)))
    (and th (not (string-match "[ >]$" th)) th)))

(defvar sepia-sub-re "^\\s *sub\\s +\\(.+\\_>\\)")


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

(defun sepia-find-module-file (mod)
  (or (sepia-module-file mod)
      (car (xref-guess-module-file mod))))

(defun sepia-module-find (mod)
"Find the file defining module MOD."
  (interactive (list (sepia-interactive-arg 'module)))
  (let ((fn (sepia-find-module-file mod)))
    (when fn
      (message "Module %s in %s." mod fn)
      (pop-to-buffer (find-file-noselect (expand-file-name fn))))))

(defmacro ifa (test then &rest else)
  `(let ((it ,test))
     (if it ,then ,@else)))

(defun sepia-show-locations (locs)
  (when locs
    (pop-to-buffer (get-buffer-create "*sepia-places*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (loc (sort (remove nil locs) ; XXX where's nil from?
                         (lambda (a b)
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
      (goto-char (point-min)))))

(defmacro define-sepia-query (name doc &optional gen test prompt)
  "Define a sepia querying function."
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

(defun sepia-location (name &optional jump-to)
  "Find the definition of NAME.

When called interactively (or with JUMP-TO true), go directly
to this location."
  (interactive (list (or (thing-at-point 'symbol)
                         (completing-read "Function: " 'xref-completions))
                     t))
  (let* ((fl (or (car (xref-location name))
                 (car (remove-if #'null
                                 (apply #'xref-location (xref-apropos name)))))))
    (when (and fl (string-match "^(eval " (car fl)))
      (message "Can't find definition of %s in %s." name (car fl))
      (setq fl nil))
    (if jump-to
        (if fl (progn
                 (sepia-set-found (list fl) 'function)
                 (sepia-next))
            (message "No definition for %s." name))
        fl)))

;;;###autoload
(defun sepia-dwim (&optional display-p)
    "Try to do the right thing with identifier at point.
* Find all definitions, if thing-at-point is a function
* Find all uses, if thing-at-point is a variable
* Find documentation, if thing-at-point is a module
* Prompt otherwise
"
    (interactive "P")
    (multiple-value-bind (type obj) (sepia-ident-at-point)
      (sepia-set-found nil type)
      (let* (module-doc-p
             (ret
              (cond
                ((member type '(?% ?$ ?@)) (xref-var-defs obj))
                ((or (equal type ?&)
                     (let (case-fold-search)
                       (string-match "^[^A-Z]" obj)))
                 (list (sepia-location obj)))
                (t
                 (setq module-doc-p t)
                 `((,(sepia-w3m-perldoc-this obj) 1 nil nil))))))
        (unless module-doc-p
          (if display-p
              (sepia-show-locations ret)
              (sepia-set-found ret type)
              (sepia-next))))))

(defun sepia-rebuild ()
  "Rebuild the Xref database."
  (interactive)
  (xref-rebuild))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl motion commands.

;;; XXX -- these are a hack to prevent infinite recursion calling
;;; e.g. beginning-of-defun from beginning-of-defun-function.
;;; `beginning-of-defun' should handle this.
(defmacro sepia-safe-bodf (&optional n)
  `(let ((beginning-of-defun-function
          (if (and (boundp 'beginning-of-defun-function)
                   (eq beginning-of-defun-function 'sepia-beginning-of-defun))
              nil
              beginning-of-defun-function)))
     (beginning-of-defun ,n)))

(defmacro sepia-safe-eodf (&optional n)
  `(let ((end-of-defun-function
          (if (and (boundp 'end-of-defun-function)
                   (eq end-of-defun-function 'sepia-end-of-defun))
              nil
              end-of-defun-function)))
     (end-of-defun ,n)))

(defun sepia-beginning-of-defun (&optional n)
"Move to beginning of current function.

If prefix argument given, move N functions backward."
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (if (and (not (= here (point)))
             (looking-at sepia-sub-re))
        (point)
        (sepia-safe-bodf n)
        (let* ((end (point))
               (beg (progn (forward-line -3) (point))))
          (goto-char end)
          (re-search-backward sepia-sub-re beg t)))))

(defun sepia-end-of-defun (&optional n)
  "Move to end of current function.

If prefix argument given, move N functions forward."
  (interactive "p")
  (let ((here (point)))
    ;; (sepia-safe-bodf)
    (when (looking-at sepia-sub-re)
      (forward-line 1))
    (sepia-safe-eodf n)
    (when (and (>= here (point))
               (re-search-forward sepia-sub-re nil t))
      (sepia-safe-eodf))
    (point)))

(defun sepia-defun-around-point (&optional where)
  "Return the text of function around point."
  (interactive "d")
  (unless where
    (setq where (point)))
  (save-excursion
    (goto-char where)
    (and (sepia-beginning-of-defun)
         (match-string-no-properties 1))))

(defun sepia-lexicals-at-point (&optional where)
  "Find lexicals in scope at point."
  (interactive "d")
  (unless where
    (setq where (point)))
  (let ((subname (sepia-defun-around-point where))
        (mod (sepia-buffer-package)))
    (xref-lexicals (perl-name subname mod))))

;;;###autoload
(defun sepia-load-file (file &optional rebuild-p collect-warnings)
  "Reload a file (interactively, the current buffer's file).

With REBUILD-P (or a prefix argument when called interactively),
also rebuild the xref database."
  (interactive (list (expand-file-name (buffer-file-name))
                     prefix-arg
                     (format "*%s errors*" (buffer-file-name))))
  (save-buffer)
  (let* ((tmp (sepia-eval (format "do '%s' ? 1 : $@" file) 'scalar-context t))
         (res (car tmp))
         (errs (cdr tmp)))
    (message "sepia: %s returned %s" (abbreviate-file-name file) res)
    (when (and collect-warnings
               (> (length errs) 1))
      (with-current-buffer (get-buffer-create collect-warnings)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))
          (insert errs)
          (sepia-display-errors (point-min) (point-max))
          (pop-to-buffer (current-buffer))))))
  (when rebuild-p
    (xref-rebuild)))

(defvar sepia-found)
(defvar sepia-found-head)
(defvar sepia-found-refiner)
(defvar sepia-history nil)

(defun sepia-set-found (list &optional type)
  (setq list
	(remove-if (lambda (x)
                     (or (not x)
                         (and (not (car x)) (string= (fourth x) "main"))))
		   list))
  (setq sepia-found list
	sepia-found-head list)
  (setq sepia-found-refiner (sepia-refiner type)))

(defun sepia-refiner (type)
  (case type
    (function
     (lambda (line ident)
      (let ((sub-re (concat "^\\s *sub\\s +.*" ident "\\_>")))
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
       (let ((var-re (concat "\\_<" ident "\\_>")))
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
	  (setq file (and mod (sepia-find-module-file mod)))
	  (if file
	      (setf (caar sepia-found) file)
	      (error "No file for %s." (car sepia-found))))
	(message "%s at %s:%s" short file line)
        (when (file-exists-p file)
          (find-file (or file (sepia-find-module-file mod)))
          (when sepia-found-refiner
            (funcall sepia-found-refiner line short))
          (beginning-of-line)
          (recenter)
          (setq sepia-found (or (cdr sepia-found)
                                sepia-found-head))))
      (message "No more definitions.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

(defun sepia-ident-at-point ()
  "Find the Perl identifier at point."
  (save-excursion
    (when (looking-at "[%$@*&]")
      (forward-char 1))
    (let* ((beg (progn
                 (when (re-search-backward "[^A-Za-z_0-9:]" nil 'mu)
                   (forward-char 1))
                 (point)))
          (sigil (if (= beg (point-min))
                     nil
                     (char-before (point))))
          (end (progn
                 (when (re-search-forward "[^A-Za-z_0-9:]" nil 'mu)
                   (forward-char -1))
                 (point))))
      (list (when (member sigil '(?$ ?@ ?% ?* ?&)) sigil)
            (buffer-substring-no-properties beg end)))))

(defun sepia-function-at-point ()
  "Find the Perl function called at point."
  (condition-case nil
      (save-excursion
        (let ((pt (point))
              bof)
          (sepia-beginning-of-defun)
          (setq bof (point))
          (goto-char pt)
          (sepia-end-of-defun)
          (when (and (>= pt bof) (< pt (point)))
            (goto-char bof)
            (looking-at "\\s *sub\\s +")
            (forward-char (length (match-string 0)))
            (concat (or (sepia-buffer-package) "")
                    "::"
                    (cadr (sepia-ident-at-point))))))
    (error nil)))

(defun sepia-complete-symbol ()
  "Try to complete the word at point.
The word may be either a global variable if it has a
sigil (sorry, no lexicals), a module, or a function.  The
function currently ignores module qualifiers, which may be
annoying in larger programs.

The function is intended to be bound to \\M-TAB, like
``lisp-complete-symbol''."
  (interactive)
  (let ((win (get-buffer-window "*Completions*" 0)))
    (if (and (eq last-command this-command)
             win (window-live-p win) (window-buffer win)
             (buffer-name (window-buffer win)))
        ;; If this command was repeated, and
        ;; there's a fresh completion window with a live buffer,
        ;; and this command is repeated, scroll that window.
        (with-current-buffer (window-buffer win)
          (if (pos-visible-in-window-p (point-max) win)
              (set-window-start win (point-min))
              (save-selected-window
                (select-window win)
                (scroll-up))))

        (multiple-value-bind (type name) (sepia-ident-at-point)
          (let ((len  (+ (if type 1 0) (length name)))
                (completions (xref-completions
                              name
                              (case type
                                (?$ "SCALAR")
                                (?@ "ARRAY")
                                (?% "HASH")
                                (?& "CODE")
                                (?* "IO")
                                (t ""))
                              (and (not (eq major-mode 'comint-mode))
                                   (sepia-function-at-point)))))
            (when (and (not completions)
                       (or (not type) (eq type ?&)))
              (when (string-match ".*::([^:]+)$" name)
                (setq name (match-string 1 name)))
              (setq completions (all-completions name sepia-perl-builtins)))
            (case (length completions)
              (0 (message "No completions for %s." name) nil)
              (1 ;; (delete-ident-at-point)
               (delete-region (- (point) len) (point))
               (insert (if type (string type) "") (car completions))
               ;; Hide stale completions buffer (stolen from lisp.el).
               (if win (with-selected-window win (bury-buffer)))
               t)
              (t (let ((old name)
                       (new (try-completion "" completions)))
                   (if (string= new old)
                       (with-output-to-temp-buffer "*Completions*"
                         (display-completion-list completions))
                       (let ((win (get-buffer-window "*Completions*" 0)))
                         (if win (with-selected-window win (bury-buffer))))
                       (delete-region (- (point) len) (point))
                       (insert (if type (string type) "") new)))
                 t)))
          ))))

(defvar sepia-indent-expand-abbrev t
"* If non-NIL, `sepia-indent-or-complete' tries `expand-abbrev'.")

(defun sepia-indent-or-complete ()
"Indent the current line or complete the symbol around point.

Specifically, try completion when indentation doesn't move point.
This function is intended to be bound to TAB."
  (interactive)
  (let ((pos (point)))
    (let (beginning-of-defun-function
          end-of-defun-function)
      (cperl-indent-command))
    (unless (or (not sepia-indent-expand-abbrev)
                (expand-abbrev))
      (when (and (= pos (point))
                 (not (bolp))
                 (or (eq last-command 'sepia-indent-or-complete)
                     (looking-at "\\_>")))
        (sepia-complete-symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scratchpad code

;;;###autoload
(defun sepia-scratch ()
  "Create a buffer to interact with a Perl interpreter.

The buffer is placed in cperl-mode; calling
``sepia-scratch-send-line'' will evaluate the current line and
display the result."
  (interactive)
  (switch-to-buffer (get-buffer-create "*perl-scratch*"))
  (cperl-mode)
  (local-set-key "\C-j" 'sepia-scratch-send-line))

(defun sepia-scratch-send-line (&optional scalarp)
  "Send the current line to perl, and display the result."
  (interactive "P")
  (insert
   (sepia-eval (concat "do{"
		       (buffer-substring (my-bol-from (point))
					 (my-eol-from (point)))
		       "}") 'scalar-context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellany

(defun my-perl-frob-region (pre post beg end replace-p)
  "Pass buffer text from BEG to END through a Perl command."
  (let* ((exp (concat pre "<<'SEPIA_END_REGION';\n"
                      (buffer-substring-no-properties beg end)
                      (if (= (char-before end) ?\n) "" "\n")
		      "SEPIA_END_REGION\n" post))
	 (new-str (sepia-eval exp 'scalar-context)))
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

;; asdf asdf asdf
;; asdf asdf asdf

(defun perl-pe-region (expr beg end &optional replace-p)
  "Do the equivalent of perl -pe on region

\(i.e. evaluate an expression on each line of region).  With
prefix arg, replace the region with the result."
  (interactive "MExpression: \nr\nP")
  (my-perl-frob-region
   "do { my $ret='';my $region = "
   (concat "; for (split /\n/, $region) { do { " expr
	   ";}; $ret.=\"$_\\n\"}; $ret}")
   (my-bol-from beg) (my-eol-from end) replace-p))

(defun perl-ne-region (expr beg end &optional replace-p)
  "Do the moral equivalent of perl -ne on region

\(i.e. evaluate an expression on each line of region).  With
prefix arg, replace the region with the result."
  (interactive "MExpression:\nr\nP")
  (my-perl-frob-region
   "do { my $ret='';my $region = "
   (concat "; for (split /\n/, $region) { $ret .= do { " expr
	   ";} }; ''.$ret}")
   (my-bol-from beg) (my-eol-from end) replace-p))
  
(defun perl-ize-region (expr beg end &optional replace-p)
  "Evaluate a Perl expression on the region as a whole.

With prefix arg, replace the region with the result."
  (interactive "MExpression:\nr\nP")
  (my-perl-frob-region "do { local $_ = "
		       (concat "; do { " expr ";}; $_ }")
		       beg end replace-p))

(defun sepia-guess-package (sub &optional file)
  "Guess which package SUB is defined in."
  (let ((defs (xref-location (xref-apropos sub))))
    (or (and (= (length defs) 1)
	     (or (not file) (equal (caar defs) file))
	     (fourth (car defs)))
	(and file
	     (fourth (find-if (lambda (x) (equal (car x) file)) defs)))
	(car (xref-file-modules file))
	(sepia-buffer-package))))

;;;###autoload
(defun sepia-eval-defun ()
  "Re-evaluate the current function and rebuild its Xrefs."
  (interactive)
  (save-excursion
    (let* ((pt (point))
           (end (progn (end-of-defun) (point)))
           (beg (progn (goto-char pt) (beginning-of-defun) (point))))
      (goto-char beg)
      (when (looking-at "^sub\\s +\\(.+\\_>\\)")
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
      (when (looking-at (concat "^\\s *sub\\_>.*\\_<" obj "\\_>"))
	(buffer-substring (point)
			  (progn (end-of-defun) (point)))))))

(defun sepia-eval-no-run (string &optional discard collect-warnings)
  (condition-case err
      (sepia-eval
       (concat "\nBEGIN { use B; B::minus_c(); $^C=1; } { "
               string
               "}\nBEGIN { die \"ok\\n\" }")
       discard collect-warnings)
    (perl-error (if (string-match "^ok\n" (cadr err))
                    nil
                    (cadr err)))
    (error err)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL

(defvar sepia-eval-file nil
  "File in which ``sepia-eval'' evaluates perl expressions.")
(defvar sepia-eval-line nil
  "Line at which ``sepia-eval'' evaluates perl expressions.")

;;;###autoload
(defun sepia-interact ()
  "Start or switch to a perl interaction buffer."
  (interactive)
  (pop-to-buffer (get-buffer "*perl-interaction*")))

(defun sepia-set-cwd (dir)
  (sepia-call "Cwd::chdir" dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doc-scanning

(defvar sepia-doc-map (make-hash-table :test #'equal))
(defvar sepia-var-doc-map (make-hash-table :test #'equal))
(defvar sepia-module-doc-map (make-hash-table :test #'equal))

(defun sepia-doc-scan-buffer ()
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward
		 "^=\\(item\\|head[2-9]\\)\\s +\\([%$&@A-Za-z_].*\\)" nil t)
       if (ignore-errors
            (let* ((s1 (match-string 2))
                   (s2 (let ((case-fold-search nil))
                         (replace-regexp-in-string
                          "[A-Z]<\\([^>]+\\)>" "\\1" s1)))
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
                ;; e.g. "$x -- this is x"
                ((string-match "^[%$@]\\([A-Za-z0-9_:]+\\)\\s *--\\s *\\(.*\\)"
                               s2)
                 (list 'variable (match-string-no-properties 1 s2)
                       (or (and (equal s2 (match-string 1 s2)) longdoc) s2)))
                ;; e.g. "C<foo(BLAH)>" or "$x = $y->foo()"
                ((string-match "\\([A-Za-z0-9_:]+\\)\\s *\\(\\$\\|(\\)" s2)
                 (list 'function (match-string-no-properties 1 s2)
                       (or (and (equal s2 (match-string 1 s2)) longdoc) s2)))
                ;; e.g. "$x this is x" (note: this has to come last)
                ((string-match "^[%$@]\\([^( ]+\\)" s2)
                 (list 'variable (match-string-no-properties 1 s2) longdoc)))))
       collect it)))

(defun sepia-buffer-package ()
  (save-excursion
    (or (and (re-search-backward "^\\s *package\\s +\\([^ ;]+\\)\\s *;" nil t)
	     (match-string-no-properties 1))
	"main")))

(defun sepia-doc-update ()
  "Update documentation for a file.

This documentation, taken from \"=item\" entries in the POD, is
used for eldoc feedback."
  (interactive)
  (let ((pack (ifa (sepia-buffer-package) (concat it "::") "")))
    (dolist (x (sepia-doc-scan-buffer))
      (let ((map (ecase (car x)
		   (function sepia-doc-map)
		   (variable sepia-var-doc-map))))
	(puthash (second x) (third x) map)
	(puthash (concat pack (second x)) (third x) map)))))

(defun sepia-symbol-info ()
  "Eldoc function for Sepia-mode.

Looks in ``sepia-doc-map'' and ``sepia-var-doc-map'', then tries
calling ``cperl-describe-perl-symbol''."
  (save-excursion
    (multiple-value-bind (type obj) (sepia-ident-at-point)
      (when (consp obj)
        (setq obj (car obj)))
      (unless type
        (setq type 'function))
      (if (and obj (member type '(function variable module)))
        (or (gethash obj (ecase (or type 'function)
                           (function sepia-doc-map)
                           (variable sepia-var-doc-map)
                           (module sepia-module-doc-map)))
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
  "Install Sepia hooks for eldoc support."
  (interactive)
  (set-variable 'eldoc-documentation-function 'sepia-symbol-info t)
  (if cperl-lazy-installed (cperl-lazy-unstall))
  (eldoc-mode 1)
  (setq eldoc-idle-delay 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error jump:

(defun sepia-extract-next-warning (pos &optional end)
  (catch 'foo
    (while (re-search-forward "^\\(.+\\) at \\(.+?\\) line \\([0-9]+\\)"
                              end t)
      (unless (string= "(eval " (substring (match-string 2) 0 6))
        (throw 'foo (list (match-string 2)
                          (parse-integer (match-string 3))
                          (match-string 1)))))))

(defun sepia-goto-error-at (pos)
  "Visit the source of the error on line at point."
  (interactive "d")
  (ifa (sepia-extract-next-warning (my-bol-from pos) (my-eol-from pos))
       (destructuring-bind (file line msg) it
	 (find-file file)
	 (goto-line line)
	 (message "%s" msg))
       (error "No error to find.")))

(defun sepia-display-errors (beg end)
  "Display source causing errors in current buffer from BEG to END."
  (interactive "r")
  (goto-char beg)
  (let ((msgs nil))
    (loop for w = (sepia-extract-next-warning (my-bol-from (point)) end)
       while w
       do (destructuring-bind (file line msg) w
            (push (format "%s:%d:%s\n" (abbreviate-file-name file) line msg)
                  msgs)))
    (erase-buffer)
    (goto-char (point-min))
    (mapcar #'insert (nreverse msgs))
    (goto-char (point-min))
    (grep-mode)))

(defun sepia-lisp-to-perl (thing)
  "Convert elisp data structure to Perl."
  (cond
    ((null thing) "undef")
    ((symbolp thing)
     (let ((pname (substitute ?_ ?- (symbol-name thing)))
           (type (string-to-char (symbol-name thing))))
       (if (member type '(?% ?$ ?@ ?*))
           pname
           (concat "\\*" pname))))
    ((stringp thing) (format "\"%s\"" thing))
    ((integerp thing) (format "%d" thing))
    ((numberp thing) (format "%g" thing))
    ((and (consp thing) (not (consp (cdr thing))))
     (concat (sepia-lisp-to-perl (car thing)) " => "
             (sepia-lisp-to-perl (cdr thing))))
    ;; list
    ((or (not (consp (car thing)))
         (listp (cdar thing)))
     (concat "[" (mapconcat #'sepia-lisp-to-perl thing ", ") "]"))
    ;; hash table
    (t
     (concat "{" (mapconcat #'sepia-lisp-to-perl thing ", ") "}"))))

(defun sepia-init-perl-builtins ()
  (setq sepia-perl-builtins (make-hash-table))
  (dolist (s '("abs"
"accept"
"alarm"
"atan2"
"bind"
"binmode"
"bless"
"caller"
"chdir"
"chmod"
"chomp"
"chop"
"chown"
"chr"
"chroot"
"close"
"closedir"
"connect"
"continue"
"cos"
"crypt"
"dbmclose"
"dbmopen"
"defined"
"delete"
"die"
"dump"
"each"
"endgrent"
"endhostent"
"endnetent"
"endprotoent"
"endpwent"
"endservent"
"eof"
"eval"
"exec"
"exists"
"exit"
"exp"
"fcntl"
"fileno"
"flock"
"fork"
"format"
"formline"
"getc"
"getgrent"
"getgrgid"
"getgrnam"
"gethostbyaddr"
"gethostbyname"
"gethostent"
"getlogin"
"getnetbyaddr"
"getnetbyname"
"getnetent"
"getpeername"
"getpgrp"
"getppid"
"getpriority"
"getprotobyname"
"getprotobynumber"
"getprotoent"
"getpwent"
"getpwnam"
"getpwuid"
"getservbyname"
"getservbyport"
"getservent"
"getsockname"
"getsockopt"
"glob"
"gmtime"
"goto"
"grep"
"hex"
"import"
"index"
"int"
"ioctl"
"join"
"keys"
"kill"
"last"
"lc"
"lcfirst"
"length"
"link"
"listen"
"local"
"localtime"
"log"
"lstat"
"map"
"mkdir"
"msgctl"
"msgget"
"msgrcv"
"msgsnd"
"next"
"oct"
"open"
"opendir"
"ord"
"pack"
"package"
"pipe"
"pop"
"pos"
"print"
"printf"
"prototype"
"push"
"quotemeta"
"rand"
"read"
"readdir"
"readline"
"readlink"
"readpipe"
"recv"
"redo"
"ref"
"rename"
"require"
"reset"
"return"
"reverse"
"rewinddir"
"rindex"
"rmdir"
"scalar"
"seek"
"seekdir"
"select"
"semctl"
"semget"
"semop"
"send"
"setgrent"
"sethostent"
"setnetent"
"setpgrp"
"setpriority"
"setprotoent"
"setpwent"
"setservent"
"setsockopt"
"shift"
"shmctl"
"shmget"
"shmread"
"shmwrite"
"shutdown"
"sin"
"sleep"
"socket"
"socketpair"
"sort"
"splice"
"split"
"sprintf"
"sqrt"
"srand"
"stat"
"study"
"sub"
"sub*"
"substr"
"symlink"
"syscall"
"sysopen"
"sysread"
"sysseek"
"system"
"syswrite"
"tell"
"telldir"
"tie"
"tied"
"time"
"times"
"truncate"
"uc"
"ucfirst"
"umask"
"undef"
"unlink"
"unpack"
"unshift"
"untie"
"utime"
"values"
"vec"
"wait"
"waitpid"
"wantarray"
"warn"
"write"
))
        (puthash s t sepia-perl-builtins)))

(provide 'sepia)
;;; sepia.el ends here
