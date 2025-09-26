;;; frame.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Frame
;;; Code:

(defconst +frameg (expand-file-name "frameg" user-emacs-directory))

;; Custom functions/hooks for persisting/loading frame geometry upon save/load
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

(defun +load-frameg ()
  "Load frame parametes from `frameg'."
  (when (file-readable-p +frameg)
    (with-temp-buffer
      (insert-file-contents +frameg)
      (goto-char (point-min))
      (modify-frame-parameters (selected-frame) (read (current-buffer))))))

(late! setup-frame
  (when window-system
    (+load-frameg)
    (add-hook 'kill-emacs-hook '+save-frameg)))

;;; frame.el ends here.
