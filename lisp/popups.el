;;; popups.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Side Popups
;;; Code:

(use-package transient
  :custom
  (transient-mode-line-format nil)
  :config
  (define-advice transient--show (:after () center)
    (let ((char-width (frame-char-width))
          (frame-width (frame-width))
          fringe
          max-line
          (max-length 0))
      (with-current-buffer transient--buffer
        (save-excursion
          (setq max-line (line-number-at-pos (point-max)))
          (goto-char (point-min))
          (while (< (line-number-at-pos) max-line)
            (setq max-length (max max-length (length (thing-at-point 'line))))
            (forward-line 1))))
      (setq fringe (* char-width (max 0 (round (/ (- frame-width max-length) 2)))))
      (set-window-fringes transient--window fringe fringe))))

(defun +pop (pred &optional side width)
  "Add `display-buffer-alist' side window rule for PRED with SIDE and WIDTH."
  (add-to-list 'display-buffer-alist `(,pred
                                       (display-buffer-reuse-window
                                        display-buffer-in-side-window)
                                       (post-command-select-window . t)
                                       (side . ,(or side 'right))
                                       (window-width . ,(or width 0.25)))))

(+pop '(major-mode . help-mode))
(+pop '(major-mode . Info-mode))
(+pop '(major-mode . apropos-mode))
(+pop '(major-mode . grep-mode))
(+pop '(major-mode . Custom-mode))
(+pop '(major-mode . occur-mode))
(+pop '(major-mode . xref--xref-buffer-mode))
(+pop '(major-mode . compilation-mode) 'bottom)
(+pop '(this-command . help))
(+pop '(this-command . customize))
(+pop '(this-command . man))
(+pop '(this-command . shell-command))

;;; popups.el ends here.
