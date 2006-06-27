;;; sepia-w3m.el --- The add-on program to view Perl documents.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Modified 2004 by Sean O'Rourke to work with Sepia and operate on
;; buffer.

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, perldoc

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; w3m-perldoc.el is the add-on program of emacs-w3m to view Perl
;; documents.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;;; Code:
(require 'w3m-perldoc)

;;;###autoload
(defun w3m-about-perldoc-buffer (url &optional no-decode no-cache &rest args)
  (when (string-match "\\`about://perldoc-buffer/" url)
    (let ((buf (get-buffer (w3m-url-decode-string
			    (substring url (match-end 0)))))
	  (default-directory w3m-profile-directory)
	  (process-environment (copy-sequence process-environment)))
      ;; To specify the place in which pod2html generates its cache files.
      (setenv "HOME" (expand-file-name w3m-profile-directory))
      (insert-buffer buf)
      (when (zerop (apply #'call-process-region
			  (point-min) (point-max)
			  w3m-perldoc-pod2html-command
			  t '(t nil) nil
			  (append w3m-perldoc-pod2html-arguments
				  '("--htmlroot=about://perldoc-buffer"))))
	(let ((case-fold-search t))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "<a href=\"about://perldoc\\(-buffer\\)?/\\([^\"]*\\)\\(\\.html\\)\">" nil t)
	    (delete-region (match-beginning 3) (match-end 3))
	    (save-restriction
	      (narrow-to-region (match-beginning 2) (match-end 2))
	      (while (search-backward "/" nil t)
		(delete-char 1)
		(insert "::"))
	      (goto-char (point-max))))
	  "text/html")))))

;;;###autoload
(defun sepia-w3m-view-pod (&optional buffer)
  "View POD for the current buffer."
  (interactive)
  (w3m-goto-url (concat "about://perldoc-buffer/"
			(w3m-url-encode-string (buffer-name buffer)))))

;;;###autoload
(defun sepia-w3m-perldoc-this (mod)
  "View perldoc for module at point."
  (interactive (list (sepia-interactive-arg 'module)))
  (w3m-perldoc mod))

(defun sepia-module-list ()
  (interactive)
  (let ((file "/tmp/modlist.html"))
    (unless (file-exists-p file)
      (with-temp-buffer
	(insert "use ExtUtils::Installed;

print \"<html><body><ul>\";
for (sort ExtUtils::Installed->new->modules) {
    print qq{<li><a href=\"about://perldoc/$_\">$_</a>};
}
print \"</ul></body></html>\n\";
")
	(shell-command-on-region (point-min) (point-max)
				 (concat "perl > " file))))
    (w3m-find-file file)))

(provide 'sepia-w3m)

;;; sepia-w3m.el ends here.
