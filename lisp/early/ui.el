;;; ui.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Early ui configuration
;;; Code:

;; disable builtin tool bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(set-face-attribute 'default nil
                    :background "#282c34"
                    :foreground "#bbc2cf")

(defconst +frameg (expand-file-name "frameg" user-emacs-directory))

;; https://www.reddit.com/r/emacs/comments/4ermj9/comment/d237n0i
(defun +save-frameg ()
  "Gets the current frame's geometry and saves to `frameg'."
  (let* ((frame (selected-frame)))
    (with-temp-file +frameg
      ;; Turn off backup for this file
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil)
      (insert
       ";; " (current-time-string) " -*- mode: lisp-data -*-\n"
       (prin1-to-string
        `((top        . ,(frame-parameter frame 'top))
          (left       . ,(frame-parameter frame 'left))
          (width      . ,(frame-parameter frame 'width))
          (height     . ,(frame-parameter frame 'height))
          (fullscreen . ,(frame-parameter frame 'fullscreen))))))))

(add-hook 'kill-emacs-hook #'+save-frameg)

(when (file-readable-p +frameg)
  (with-temp-buffer
    (insert-file-contents +frameg)
    (goto-char (point-min))
    (let ((parameters (read (current-buffer))))
      (setq initial-frame-alist parameters)
      (modify-frame-parameters (selected-frame) parameters))))

;;; ui.el ends here.
