(require 'gud)

(defvar gud-sepia-acc)

(defun sepia-gud-common-init ()
  (let* ((existing-buffer (get-buffer "*sepia-repl*")))
    (pop-to-buffer existing-buffer)

    ;; Since comint clobbered the mode, we don't set it until now.
    ;; (gud-mode)
    (set (make-local-variable 'gud-last-frame) nil)
    (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
    (make-local-variable 'comint-prompt-regexp)
    ;; Don't put repeated commands in command history many times.
    (set (make-local-variable 'comint-input-ignoredups) t)
    (make-local-variable 'paragraph-start)
    (set (make-local-variable 'gud-delete-prompt-marker) (make-marker))

    (set (make-local-variable 'gud-target-name)
	 (and file-word (file-name-nondirectory file)))
    (set (make-local-variable 'gud-marker-filter) 'sepia-gud-marker-filter)
    (setq gud-running t)
    (setq gud-last-last-frame nil)

    ;; NOTE: gud-filter calls comint-output-filter, so we're okay
    (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          'gud-sentinel)
    (gud-set-buffer)))

(defun sepia-gud-disable ()
  (with-current-buffer (get-buffer "*sepia-repl*")
    (set-process-filter (get-buffer-process (current-buffer))
                        'comint-output-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) nil)
    (setq gud-running nil)
    (setq tool-bar-map nil)))

(defun sepia-gud (&optional cmd)
  (interactive)
  ;; Set up our commands
  (sepia-gud-common-init)
  (set (make-local-variable 'gud-minor-mode) 'sepia)
  (gud-def gud-break ",break %l %f" "\C-b" "Set breakpoint at current line.")
  (gud-def gud-step ",step %p" "\C-s" "Step one line.")
  (gud-def gud-next ",next %p" "\C-n" "Step one line, skipping calls.")
  (gud-def gud-cont ",continue" "\C-r" "Continue.")
  (gud-def gud-print "%e" "\C-p" "Evaluate something.")
  (gud-def gud-remove ",delete %l %f" "\C-d" "Delete current breakpoint.")
  (setq gud-sepia-acc nil)
  (run-hooks 'sepia-gud-mode-hook))

(defun gud-sepia-massage-args (&rest blah)
  (error "don't do this")
  )

(defun gud-sepia-marker-filter (str)
  (setq gud-sepia-acc
        (if gud-sepia-acc
            (concat gud-sepia-acc str)
            str))
  (while (string-match "^_<\\([^:>]+\\):\\([0-9]+\\)>\n\\(.*\\)" gud-sepia-acc)
    (setq gud-sepia-acc (match-string 3 gud-sepia-acc)
          gud-last-last-frame gud-last-frame
          gud-last-frame (cons
                          (string-to-number (match-string 2 gud-sepia-acc))
                          (match-string 1 gud-sepia-acc)))))

;; (defun gud-sepia-find-file (file)
;;   )
