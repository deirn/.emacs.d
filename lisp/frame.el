;;; frame.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Frame
;;; Code:

;; Custom functions/hooks for persisting/loading frame geometry upon save/load
;; https://www.reddit.com/r/emacs/comments/4ermj9/comment/d237n0i
(defun +save-frameg ()
  "Gets the current frame's geometry and saves to `frameg.el'."
  (let ((frameg-left (frame-parameter (selected-frame) 'left))
        (frameg-top (frame-parameter (selected-frame) 'top))
        (frameg-width (frame-parameter (selected-frame) 'width))
        (frameg-height (frame-parameter (selected-frame) 'height))
        (frameg-fullscreen (frame-parameter (selected-frame) 'fullscreen))
        (frameg-file (expand-file-name "frameg.el" user-emacs-directory)))
    (with-temp-buffer
      ;; Turn off backup for this file
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil)
      (insert
       ";;; This file stores the previous emacs frame's geometry. -*- lexical-binding: t; -*-\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       " '("
       (format " (top . %d)\n" (max frameg-top 0))
       (format " (left . %d)\n" (max frameg-left 0))
       (format " (width . %d)\n" (max frameg-width 0))
       (format " (height . %d)\n" (max frameg-height 0))
       (format " (fullscreen . %s)))\n" frameg-fullscreen))
      (when (file-writable-p frameg-file)
        (write-file frameg-file)))))

(defun +load-frameg ()
  "Loads `frameg.el' which should load the previous frame's geometry."
  (let ((frameg-file (expand-file-name "frameg.el" user-emacs-directory)))
    (when (file-readable-p frameg-file)
      (load-file frameg-file))))

;; Special work to do ONLY when there is a window system being used
(when window-system
  (add-hook 'after-init-hook '+load-frameg)
  (add-hook 'kill-emacs-hook '+save-frameg))

;;; frame.el ends here.
