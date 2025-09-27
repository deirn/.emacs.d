;;; trust.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Modify `trusted-content'
;;; Code:

(defconst +trusted-content-file (expand-file-name "trusted-content" user-emacs-directory))

(defun +save-trusted-content ()
  "Save `trusted-content' to a file."
  (with-temp-file +trusted-content-file
    (insert
     ";; " (current-time-string) " -*- mode: lisp-data -*-\n"
     (prin1-to-string trusted-content))))

(defun +trust-content (path)
  "Modify `trusted-content' with specified PATH."
  (interactive (list (abbreviate-file-name (expand-file-name (read-file-name "Trust content: " nil nil t)))))
  (add-to-list 'trusted-content path)
  (+save-trusted-content))

(defun +remove-trusted-content(path)
  "Remove `trusted-content' PATH entry."
  (interactive (list (completing-read "Remove trusted: " trusted-content nil t)))
  (setq trusted-content (delete path trusted-content))
  (+save-trusted-content))

(late! trusted-content
  (when (file-readable-p +trusted-content-file)
    (with-temp-buffer
      (insert-file-contents +trusted-content-file)
      (goto-char (point-min))
      (setq trusted-content (read (current-buffer))))))

;;; trust.el ends here.
