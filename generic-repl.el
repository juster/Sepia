;;; generic-repl.el -- Mode for any-language REPL (stolen from Slime).

;;; License
;;     Copyright (C) 2004 Sean O'Rourke; mostly copied from Slime,
;;     which is Copyright (C) 2003 Eric Marsden, Luke Gorrie, Helmut
;;     Eller
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


;; This mode is designed to provide a comfortable REPL to something
;; with only the ability to evaluate a string and return the result as
;; a string (see the documentation for ``repl-supported-modes'' for
;; details).
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;; 
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start input-end  
;;
;; output-start and input-start are right inserting markers;
;; output-end and input-end left inserting.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start <= input-end.

;; Small helper.
(defun repl-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(repl-make-variables-buffer-local
 ;; Local variables in the REPL buffer.
 (defvar repl-input-history '()
   "History list of strings read from the REPL buffer.")
 
 (defvar repl-input-history-position 0)

 (defvar repl-prompt-start-mark)
 (defvar repl-input-start-mark)
 (defvar repl-input-end-mark)
 (defvar repl-last-input-start-mark)
;;; Stream output
 (defvar repl-output-start nil
   "Marker for the start of the output for the evaluation.")
 (defvar repl-output-end nil
   "Marker for end of output. New output is inserted at this mark.")

;;; Handlers:
 (defvar repl-eval-func nil
   "Function to call to evaluate text from the REPL.")
 (defvar repl-eval-async-func nil
   "Function to call to evaluate text from the REPL asynchronously.")
 (defvar repl-get-package-func nil
   "Function to call to change the package in which code is evaluated.")
 (defvar repl-set-package-func nil
   "Function to retrieve the package in which code is evaluated.")
 (defvar repl-input-complete-func nil
   "Function to tell if the input forms a complete, evaluable
   statement/expression.")
 (defvar repl-completion-func nil
   "Function to complete the symbol at point.")
 (defvar repl-header-func nil
   "Function to generate extra header information.")
 (defvar repl-cd-func nil
   "Function to change inferior process working directory.")
 )

(defvar repl-supported-modes nil
  "An alist of languages supported by generic-repl mode.  Each
entry's car should be a language's name as a string, while the
cdr should be a plist of configuration options.  A mode is
required to supply
  :eval           a synchronous evaluation function, which should
                  accept a single string, and either return a
                  string or throw an error.
  :eval-async     an asynchronous evaluation function, which should
                  accept a string and a function to be called one
                  or more times with output.  Calling this
                  function with a nil argument indicates end of
                  output.

It may optionally supply any of:
  :map            base keymap
  :init           initialize state after REPL starts up.
  :get-package    return package in which forms are evaluated
  :set-package    change default evaluation package
  :expression-p   test whether the input forms a complete expression
  :complete       function to complete symbol at point
  :header         return additional information to display in header-line
  :cd             change evaluation directory
  :comment-start  single-line comment starting character, used
                  to comment out messages from generic-repl.
")

(defun generic-repl (lang)
"Start a generic repl for language LANG, which must be defined in
``repl-supported-modes''.  Interactively, prompt for LANG."
  (interactive
   (list (completing-read "Sublanguage: " repl-supported-modes)))
  (assert (assoc lang repl-supported-modes))
  (switch-to-buffer (get-buffer-create (format "*%s-interaction*" lang)))
  (repl-mode lang))

(defun repl-mode (x) 
  "Major mode for interacting with a X interpreter in X-mode."
  (let ((defn (cdr (assoc x repl-supported-modes))))
    (assert (and (or (plist-get defn :eval)
		     (plist-get defn :eval-async))
		 (not (and (plist-get defn :eval)
			   (plist-get defn :eval-async))))
	    t "Must specify sync XOR async evaluation function.")
    (setq
     major-mode 'repl-mode
     comment-start (plist-get defn :comment-start)
     repl-eval-func (plist-get defn :eval)
     repl-eval-async-func (plist-get defn :eval-async)
     repl-get-package-func (plist-get defn :get-package)
     repl-set-package-func (plist-get defn :set-package)
     repl-input-complete-func (plist-get defn :expression-p)
     repl-completion-func (plist-get defn :complete)
     repl-header-func (plist-get defn :header)
     repl-cd-func (plist-get defn :cd)
     mode-name (format "%s-REPL" x)
     )
    (use-local-map (repl-make-keymap
		    (if (plist-get defn :map)
			(symbol-value (plist-get defn :map))
			nil)))
    (dolist (markname (list 'repl-output-start
			    'repl-output-end
			    'repl-prompt-start-mark
			    'repl-input-start-mark
			    'repl-input-end-mark
			    'repl-last-input-start-mark))
      (set markname (make-marker))
      (set-marker (symbol-value markname) (point)))
    (set-marker-insertion-type repl-input-end-mark t)
    (set-marker-insertion-type repl-output-end t)
    (set-marker-insertion-type repl-prompt-start-mark t)
    (repl-insert-prompt "" 0)
    (repl-set-header)
    (if (plist-get defn :init)
	(funcall (plist-get defn :init)))))

(defun repl-make-keymap (parent)
  (let ((kmap (make-sparse-keymap))) 
    (set-keymap-parent kmap parent)
    (dolist (kv '(("\C-m" repl-return)
		  ("\C-j" repl-newline-and-indent)
		  ("\C-a" repl-bol)
		  ("\C-e" repl-eol)
		  ("\M-p" repl-previous-input)
		  ("\M-n" repl-next-input)
		  ("\M-r" repl-previous-matching-input)
		  ("\M-s" repl-next-matching-input)
		  ([tab] repl-complete-symbol)
		  ("\C-c\C-o" repl-clear-output)
		  ("\C-c\C-t" repl-clear-buffer)
		  ("\C-c\C-n" repl-next-prompt)
		  ("\C-c\C-p" repl-previous-prompt)
		  ("\M-\C-a" repl-beginning-of-defun)
		  ("\M-\C-e" repl-end-of-defun)))
      (define-key kmap (car kv) (cadr kv)))
    kmap))

(defun repl-complete-symbol ()
  (interactive)
  (if (fboundp repl-completion-func)
      (funcall repl-completion-func)))

(defmacro repl-propertize-region (props &rest body)
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(put 'repl-propertize-region 'lisp-indent-function 1)

(defsubst repl-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (repl-propertize-region props (apply #'insert args)))

(defface repl-output-face
  '((t (:inherit font-lock-string-face)))
  "Face for output in the REPL."
  :group 'repl)

(defface repl-input-face
  '((t (:bold t)))
  "Face for previous input in the REPL."
  :group 'repl)

(defface repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the REPL."
  :group 'repl)

(defun repl-insert-prompt (result &optional time)
  "Goto to point max, insert RESULT and the prompt.  Set
repl-output-end to start of the inserted text repl-input-start to
end end."
  (goto-char (point-max))
  (let ((start (point)))
    (unless (bolp) (insert "\n"))
    (repl-insert-propertized '(face repl-result-face) result)
    (unless (bolp) (insert "\n"))
    (let ((prompt-start (point)))
      (repl-propertize-region
          '(face font-lock-keyword-face 
                 read-only t
                 intangible t
                 repl-prompt t
                 ;; emacs stuff
                 rear-nonsticky (repl-prompt read-only face intangible)
                 ;; xemacs stuff
                 start-open t end-open t)
        (insert (if (fboundp repl-get-package-func)
		    (funcall repl-get-package-func)
		    "") "> "))
      (set-marker repl-output-end start)
      (set-marker repl-prompt-start-mark prompt-start)
      (repl-mark-input-start)
      (let ((time (or time 0.2)))
        (cond ((zerop time)
               (repl-move-output-mark-before-prompt (current-buffer)))
              (t 
               (run-at-time time nil 'repl-move-output-mark-before-prompt
                            (current-buffer))))))))

(defun repl-move-output-mark-before-prompt (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion 
        (goto-char repl-prompt-start-mark)
        (repl-mark-output-start)))))

(defun repl-current-input ()
  "Return the current input as string.  The input is the region from
after the last prompt to the end of buffer."
  (buffer-substring-no-properties
   repl-input-start-mark
   (save-excursion
     (goto-char repl-input-end-mark)
     (when (eq (char-before) ?\n)
       (backward-char 1))
     (point))))

(defun repl-add-to-input-history (string)
  (when (and (plusp (length string))
	     (eq ?\n (aref string (1- (length string)))))
    (setq string (substring string 0 -1)))
  (unless (equal string (car repl-input-history))
    (push string repl-input-history))
  (setq repl-input-history-position -1))

(defun repl-send-string (string)
  (repl-add-to-input-history string)
;;  (with-current-buffer (repl-output-buffer)
  (if repl-eval-async-func
      (funcall repl-eval-async-func string
	       (lexical-let ((buf (current-buffer)))
		 (lambda (&optional string)
		   (with-current-buffer buf
		     (cond
		       (string (goto-char (point-max))
			       (repl-insert-propertized
				'(face repl-result-face)
				string))
		       (t (repl-insert-prompt "" 0)))))))
      (condition-case err
	  (repl-insert-prompt (funcall repl-eval-func string))
	(error (repl-comment (format "ERROR: %s" (cdr err)))
	       (repl-insert-prompt "" 0)))))

(defun repl-comment (str)
  (let ((beg (point)))
    (insert str)
    (comment-region beg (point))))

(defun repl-show-abort ()
;;   (with-current-buffer (repl-output-buffer)
    (repl-with-output-end-mark 
     (unless (bolp) (insert "\n"))
     (repl-comment "Evaluation aborted\n")))
  
(defun repl-mark-input-start ()
  (set-marker repl-last-input-start-mark
              (marker-position repl-input-start-mark))
  (set-marker repl-input-start-mark (point) (current-buffer))
  (set-marker repl-input-end-mark (point) (current-buffer)))

(defun repl-mark-output-start ()
  (set-marker repl-output-start (point))
  (set-marker repl-output-end (point)))

(defun repl-mark-output-end ()
  (add-text-properties repl-output-start repl-output-end
                       '(face repl-output-face rear-nonsticky (face))))

(defun repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (if (and (>= (point) repl-input-start-mark)
           (repl-same-line-p (point) repl-input-start-mark))
      (goto-char repl-input-start-mark)
    (beginning-of-line 1)))

(defun repl-eol ()
  "Go to the end of line or the prompt."
  (interactive)
  (if (and (<= (point) repl-input-end-mark)
           (repl-same-line-p (point) repl-input-end-mark))
      (goto-char repl-input-end-mark)
    (end-of-line 1)))

(defun repl-in-input-area-p ()
   (and (<= repl-input-start-mark (point))
        (<= (point) repl-input-end-mark)))
  
(defun repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  (if (repl-in-input-area-p)
      (goto-char repl-input-start-mark)
    (beginning-of-defun)))

(defun repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  (if (repl-in-input-area-p)
      (goto-char repl-input-end-mark)
    (end-of-defun)))

(defun repl-at-prompt-end-p ()
  (and (get-char-property (max 1 (1- (point))) 'repl-prompt)
       (not (get-char-property (point) 'repl-prompt))))
 
(defun repl-find-prompt (move)
  (let ((origin (point)))
    (loop (funcall move)
          (when (or (repl-at-prompt-end-p) (bobp) (eobp))
            (return)))
    (unless (repl-at-prompt-end-p)
      (goto-char origin))))

(defmacro with-lexical-bindings (variables &rest body)
  "Execute BODY with VARIABLES in lexical scope."
  `(lexical-let ,(mapcar (lambda (variable) (list variable variable))
                         variables)
     ,@body))

(put 'with-lexical-bindings 'lisp-indent-function 1)

(defun repl-search-property-change-fn (prop &optional backward)
  (with-lexical-bindings (prop)
    (if backward 
        (lambda () 
          (goto-char
           (previous-single-char-property-change (point) prop)))
        (lambda () 
          (goto-char
           (next-single-char-property-change (point) prop))))))

(defun repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (repl-find-prompt 
   (repl-search-property-change-fn 'repl-prompt t)))

(defun repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (repl-find-prompt
   (repl-search-property-change-fn 'repl-prompt)))

(defun repl-return (&optional force)
  "Evaluate the current input string, or insert a newline.  
Send the current input only if a whole expression has been
entered, or if prefix argument is suppled."
  (interactive)
  (assert (<= (point) repl-input-end-mark))
  (cond
    (force (repl-send-input) (insert "\n"))
    ((repl-input-complete-p repl-input-start-mark repl-input-end-mark)
     (goto-char repl-input-end-mark)
     (insert "\n")
     (repl-send-input))
    (t (repl-newline-and-indent);;  (message "[input not complete]")
       )))

(defun repl-input-complete-p (beg end)
  (if (fboundp repl-input-complete-func)
      (funcall repl-input-complete-func beg end)
      t))

(defun repl-send-input ()
  "Goto to the end of the input and send the current input."
  (let ((input (repl-current-input)))
    (goto-char repl-input-end-mark)
    (add-text-properties repl-input-start-mark (point)
                         '(face repl-input-face rear-nonsticky (face)))
    (repl-mark-output-start)
    (repl-mark-input-start)
    (repl-send-string (concat input "\n"))))

(defun repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region repl-prompt-start-mark (point-max))
    (insert "\n")
    (indent-according-to-mode)
    ))

(defun repl-delete-current-input ()
  (delete-region repl-input-start-mark repl-input-end-mark))

(defun repl-replace-input (string)
  (repl-delete-current-input)
  (insert-and-inherit string))

(defun repl-input-line-beginning-position ()
  (save-excursion
    (goto-char repl-input-start-mark)
    (line-beginning-position)))

(defun repl-clear-buffer ()
  (interactive)
  (set-marker repl-last-input-start-mark nil)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (repl-input-line-beginning-position))))

(defun repl-clear-output ()
  (interactive)
  (let ((start (save-excursion 
                 (repl-previous-prompt)
                 (point)))
        (end (1- (repl-input-line-beginning-position))))
    (when (< start end)
      (delete-region start end)
      (save-excursion
        (goto-char start)
	(repl-comment "output flushed\n")))))

(defun repl-same-line-p (pos1 pos2)
  "Return true if buffer positions PoS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (not (search-forward "\n" (max pos1 pos2) t))))

(defun repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive "sPackage: ")
;;   (with-current-buffer (repl-output-buffer)
    (let ((unfinished-input (repl-current-input)))
      (when (fboundp repl-set-package-func)
	(funcall repl-set-package-func package))
      (repl-insert-prompt "" 0)
      (insert unfinished-input)))

;;;;; History

(defvar repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun repl-history-replace (direction regexp)
  "Replace the current input with the next line in DIRECTION matching REGEXP.
DIRECTION is 'forward' or 'backward' (in the history list)."
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history-pos0 repl-input-history-position))
    (setq repl-history-pattern regexp)
    ;; Loop through the history list looking for a matching line
    (loop for pos = (+ history-pos0 step) then (+ pos step)
          while (and (<= 0 pos)
                     (< pos (length repl-input-history)))
          do (let ((string (nth pos repl-input-history)))
               (when (and (string-match regexp string)
                          (not (string= string (repl-current-input))))
                 (repl-replace-input string)
                 (setq repl-input-history-position pos)
                 (return)))
          finally (message "End of history; no matching item"))))

(defun repl-matching-input-regexp ()
  (if (memq last-command
            '(repl-previous-input repl-next-input))
      repl-history-pattern
    (concat "^" (regexp-quote (repl-current-input)))))

(defun repl-previous-input ()
  (interactive)
  (repl-history-replace 'backward (repl-matching-input-regexp)))

(defun repl-next-input ()
  (interactive)
  (repl-history-replace 'forward (repl-matching-input-regexp)))

(defun repl-previous-matching-input (regexp)
  (interactive "sPrevious element matching (regexp): ")
  (repl-history-replace 'backward regexp))

(defun repl-next-matching-input (regexp)
  (interactive "sNext element matching (regexp): ")
  (repl-history-replace 'forward regexp))

(defun repl-set-header (&optional msg)
"Update the header line to display MSG.  If MSG is nil, then show
the current working directory instead."
  (when (boundp 'header-line-format)
    (setq header-line-format
          (format "%s  %s"
		  (if (fboundp repl-header-func)
		      (funcall repl-header-func)
		      "")
                  (or msg (abbreviate-file-name default-directory))))))

(defun repl-cd (dir &optional name)
  "Change buffer and (optionally) process working directory to DIR."
  (interactive "d")
  (with-current-buffer
      (if name (get-buffer (format "*%s-interaction*" name)) (current-buffer))
    (setq default-directory dir)
    (if (fboundp repl-cd-func)
	(funcall repl-cd-func (expand-file-name dir)))
    (repl-set-header)))


(provide 'generic-repl)
;;; generic-repl.el ends here
