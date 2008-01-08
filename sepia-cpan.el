(require 'button)

(define-button-type 'sepia-cpan
  'follow-link nil
  'action 'sepia-cpan-button
  'help-echo "[r]eadme, [d]ocumentation, [i]nstall, [b]rowse"
  'keymap sepia-cpan-keymap)

(defvar sepia-cpan-actions
  '(("r" . sepia-cpan-readme)
    ("d" . sepia-cpan-doc)
    ("i" . sepia-cpan-install)
    ("b" . sepia-cpan-browse)
    ("?" . sepia-cpan-readme)))

(defun sepia-cpan-doc (mod)
  (interactive "sModule: ")
  (browse-url (concat "http://search.cpan.org/perldoc?" mod)))

(defun sepia-cpan-readme (mod)
  (interactive "sModule: ")
  (with-current-buffer (get-buffer-create "*sepia-cpan-readme*")
    (insert (sepia-call "Sepia::CPAN::readme" 'list-context mod))
    (pop-to-buffer (current-buffer))))

(defun sepia-cpan-install (mod)
  (interactive "sModule: ")
  (when (y-or-n-p (format "install %s?" mod))
    (sepia-call "Sepia::CPAN::install" 'void-context mod)))

(defun sepia-cpan-list (pattern)
  (interactive "sPattern (regexp): ")
  (sepia-eval (format "map $_->id, Sepia::CPAN::list('/%s/')" pattern)
              'list-context))

(defun sepia-cpan-button (button)
  (funcall (cdr (assoc sepia-cpan-button sepia-cpan-actions))
           (button-label button)))

(defvar sepia-cpan-button)

(defun sepia-cpan-button-press ()
  (interactive)
  (let ((sepia-cpan-button (this-command-keys)))
    (push-button)))

(defvar sepia-cpan-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km button-map)
    (define-key km "q" 'bury-buffer)
    (dolist (k (mapcar #'car sepia-cpan-actions))
      (define-key km k 'sepia-cpan-button-press))
    km))

(defun sepia-cpan-buffer (pat)
  (interactive  "sPattern (regexp): ")
  (switch-to-buffer "*sepia-cpan*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (insert (format "\
CPAN modules matching /%s/
    [r]eadme, [d]ocumentation, [i]nstall, [b]rowse

" pat))
  (dolist (mod (sepia-cpan-list pat))
    (let ((beg (point)))
      (insert mod)
      (make-button beg (point) :type 'sepia-cpan))
      (insert "\n"))
  (use-local-map sepia-cpan-keymap))

(provide 'sepia-cpan)
