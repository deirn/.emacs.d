;;; popups.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Side Popups
;;; Code:

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

;;; popups.el ends here.
