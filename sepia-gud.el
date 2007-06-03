(define-derived-mode sepia-repl-mode gud-mode "Sepia REPL"
  "Major mode for the Sepia REPL."
    (set (make-local-variable 'comint-dynamic-complete-functions)
         '(sepia-complete-symbol comint-dynamic-complete-filename))
    (set (make-local-variable 'comint-preoutput-filter-functions)
         '(sepia-watch-for-eval))
    ;; (set (make-local-variable 'comint-use-prompt-regexp) t)
    (modify-syntax-entry ?: "_")
    (modify-syntax-entry ?> ".")
    (use-local-map (copy-keymap (current-local-map)))
    (sepia-install-keys)
    (local-set-key (kbd "TAB") 'comint-dynamic-complete)
    (local-set-key "\C-a" 'comint-bol)
    (set (make-local-variable 'comint-prompt-regexp) "^[^>\n]*> *")
    (set (make-local-variable 'gud-target-name) "sepia")
    (set (make-local-variable 'gud-marker-filter) 'sepia-gud-marker-filter)
    (set (make-local-variable 'gud-minor-mode) 'sepia)

    (setq gud-comint-buffer (current-buffer))
    (setq gud-running t)
    (setq gud-last-last-frame nil)
    (setq gud-sepia-acc nil)

    (gud-def gud-break ",break %l %f" "\C-b" "Set breakpoint at current line.")
    (gud-def gud-step ",step %p" "\C-s" "Step one line.")
    (gud-def gud-next ",next %p" "\C-n" "Step one line, skipping calls.")
    (gud-def gud-cont ",continue" "\C-c" "Continue.")
    (gud-def gud-print "%e" "\C-p" "Evaluate something.")
    (gud-def gud-remove ",delete %l %f" "\C-d" "Delete current breakpoint.")
    (run-hooks 'sepia-repl-mode-hook))

(defvar gud-sepia-acc nil
  "Accumulator for `sepia-gud-marker-filter'.")

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
