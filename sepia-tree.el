;;; sepia-tree.el -- tree-widget-based calle[re] navigation

;; Copyright (C) 2004 Sean O'Rourke.  All rights reserved, some wrongs
;; reversed.  This code is distributed under the same terms as Perl
;; itself.

;;; Commentary:

;; See the README file that comes with the distribution.

;;; Code:

(require 'sepia)
(require 'tree-widget)

(defun sepia-tree-button-cb (widget &rest blah)
  (let* ((pw (widget-get widget :parent))
	 (location (mapcar (lambda (x) (widget-get pw x))
			   '(:sepia-file :sepia-line :sepia-obj :sepia-mod))))
    (cond
      (current-prefix-arg
       (find-file-other-window (car location))
       (sepia-set-found (list location) 'function)
       (sepia-next))
      ((widget-get widget :sepia-shown-p)
       (save-excursion
	 (end-of-line)
	 (let ((inhibit-read-only t))
	   (delete-region (point)
			  (+ 1 (point) (widget-get widget :sepia-shown-p))))
	 (widget-put widget :sepia-shown-p nil)))
      (t
       (let ((str (apply #'sepia-extract-def location)))
	 (if str
	     (save-excursion
	       (end-of-line)
	       (widget-put widget :sepia-shown-p (length str))
	       (widget-insert "\n" str))
	     (message "(not found)")))))))

(defun sepia-tree-node-cb (widget &rest blah)
  (let ((func (widget-get widget :sepia-func)))
    (or (widget-get widget :args)
	(let ((children
	       (sort
		(sepia-uniquify (funcall func widget))
		#'sepia-tree-def-order
		)))
	  (if children
	      (mapcar
	       (lambda (x) (apply #'sepia-tree-node func x))
	       children)
	      (widget-put widget :has-children nil))))))

(defun sepia-tree-node (func file line obj mod)
  "Make a tree node for the object specified by FILE, LINE, OBJ,
and MOD.  The new node will have a property :sepia-X
corresponding to each of these values.  FUNC is a function that
will, given a widget, generate its children."
  `(tree-widget
    :node (push-button
	   :tag ,(format "%s::%s" mod obj)
	   :format "%[%t%]\n"
	   :help-echo ,(format "%s::%s -- %s:%s" mod obj file line)
	   :notify sepia-tree-button-cb)
    :dynargs sepia-tree-node-cb
    :has-children t
    :sepia-obj ,obj
    :sepia-mod ,mod
    :sepia-file ,file
    :sepia-line ,line
    :sepia-func ,func))

(defun sepia-tree-tidy-buffer (name)
  "Get/create a new, tidy buffer for the tree widget."
  (switch-to-buffer name)
  (kill-all-local-variables)
  (setq widget-image-enable nil);; because the widget images are ugly.
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapcar #'delete-overlay (car all))
    (mapcar #'delete-overlay (cdr all)))
  (toggle-read-only 1)
  (view-mode -1))

(defun sepia-build-tree-buffer (func defs bufname)
  (if defs
      (lexical-let ((func func))
	(sepia-tree-tidy-buffer bufname)
	(with-current-buffer bufname
	  (dolist (x defs)
	    (apply #'widget-create
		   (apply #'sepia-tree-node
			  (lambda (widget)
			    (funcall func (widget-get widget :sepia-obj)
				     (widget-get widget :sepia-mod)))
			  x)))
	  (use-local-map (copy-keymap widget-keymap))
;;	  (local-set-key "\M-." sepia-keymap)
	  (sepia-install-keys)
	  (let ((view-read-only nil))
	    (toggle-read-only 1))
	  (goto-char (point-min))
	  (message "Type C-h m for usage information")))
      (message "No items for %s" bufname)))

;;;###autoload
(defun sepia-callee-tree (obj mod)
  "Create a tree view of a function's callees.

Pressing RET on a function's name displays its definition.  With
prefix argument, RET instead visits in another window."
  (interactive (list (sepia-interactive-arg 'function)
		     (sepia-interactive-module)))
  (let* ((defs (xref-defs obj mod))
	 (mod (if (= (length defs) 1) (or (fourth (car defs)) mod) "*")))
    (sepia-build-tree-buffer #'xref-callees defs
			     (format "*%s::%s callees*" mod obj))))

(defun sepia-caller-tree (obj mod)
  "Create a tree view of a function's callers.

Pressing RET on a function's name displays its definition.  With
prefix argument, RET instead visits in another window."
  (interactive (list (sepia-interactive-arg 'function)
		     (sepia-interactive-module)))
  (let* ((defs (xref-defs obj mod))
	 (mod (if (= (length defs) 1) (or (fourth (car defs)) mod) "*")))
    (sepia-build-tree-buffer #'xref-callees 
			     defs
			     (format "*%s::%s callers*" mod obj))))

(defun sepia-uniquify (xs &optional test)
  (let ((h (make-hash-table :test (or test #'equal))))
    (dolist (x xs)
      (puthash x nil h))
    (hash-table-keys h)))

(defun sepia-tree-def-order (a b)
  (or (string< (fourth a) (fourth b))
      (and (string= (fourth a) (fourth b))
	   (string< (third a) (third b)))))

;;;###autoload
(defun sepia-module-callee-tree (mod)
  "Display a callee tree for each of MOD's subroutines.

Pressing RET on a function's name displays its definition.  With
prefix argument, RET instead visits in another window."
  (interactive (list (sepia-interactive-arg 'module)))
  (let ((defs (sort
	       (sepia-uniquify
		(remove-if (lambda (x)
			     (and (fourth x)
				  (not (string= (fourth x) mod))))
			   (mapcan (lambda (x) (xref-defs x mod))
				   (xref-apropos "" (concat "^" mod "$")))))
	       #'sepia-tree-def-order)))
    (sepia-build-tree-buffer #'xref-callees defs (format "*%s subs*" mod))))

(provide 'sepia-tree)
;;; sepia.el ends here
