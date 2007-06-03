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

    (set (make-local-variable 'gud-target-name) "sepia")
    (set (make-local-variable 'gud-marker-filter) 'sepia-gud-marker-filter)
    (setq gud-running t)
    (setq gud-last-last-frame nil)

    ;; NOTE: gud-filter calls comint-output-filter, so we're okay
    (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          'gud-sentinel)
    (setq gud-comint-buffer existing-buffer)))

(defun sepia-gud-running ()
  (eq (process-filter sepia-process)
      'gud-filter))

(defun sepia-gud (&optional value)
  (interactive "P")
  ;; Set up our commands
  (unless value
    (setq value (if (sepia-gud-running) -1 1)))
  (with-current-buffer (get-buffer "*sepia-repl*")
    (cond
      ;; Turn off?
      ((and (sepia-gud-running) (= value -1))
       (comint-send-string sepia-process ",debug 0\n")
       (accept-process-output sepia-process)
       (set-process-filter (get-buffer-process (current-buffer))
                           'comint-output-filter)
       (set-process-sentinel (get-buffer-process (current-buffer)) nil)
       (setq gud-running nil)
       (setq tool-bar-map nil))
      ;; Turn on?
      ((and (not (sepia-gud-running)) (= value 1))
       (setq sepia-gud-running t)
       (comint-send-string sepia-process ",debug 1\n")
       (accept-process-output sepia-process)
       (sepia-gud-common-init)
       (set (make-local-variable 'gud-minor-mode) 'sepia)
       (gud-def gud-break ",break %l %f" "\C-b" "Set breakpoint at current line.")
       (gud-def gud-step ",step %p" "\C-s" "Step one line.")
       (gud-def gud-next ",next %p" "\C-n" "Step one line, skipping calls.")
       (gud-def gud-cont ",continue" "\C-c" "Continue.")
       (gud-def gud-print "%e" "\C-p" "Evaluate something.")
       (gud-def gud-remove ",delete %l %f" "\C-d" "Delete current breakpoint.")
       (setq gud-sepia-acc nil)
       (run-hooks 'sepia-gud-mode-hook)))))

(defun gud-sepia-massage-args (&rest blah)
  (error "don't do this")
  )

(defun sepia-gud-marker-filter (str)
  (setq gud-sepia-acc
        (if gud-sepia-acc
            (concat gud-sepia-acc str)
            str))
  (while (string-match "_<\\([^:>]+\\):\\([0-9]+\\)>\\(.*\\)" gud-sepia-acc)
    (setq gud-last-last-frame gud-last-frame
          gud-last-frame (cons
                          (match-string 1 gud-sepia-acc)
                          (string-to-number (match-string 2 gud-sepia-acc)))
          gud-sepia-acc (match-string 3 gud-sepia-acc)))
  (setq gud-sepia-acc
        (if (string-match "_<\\(.*\\)" gud-sepia-acc)
            (match-string 1 gud-sepia-acc)
            nil))
  str)

;; (defun gud-sepia-find-file (file)
;;   )
