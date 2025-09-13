;;; formatting.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Formatting
;;; Code:

(setq-default truncate-lines t
              tab-width 4)

(setq standard-indent 4
      whitespace-style '(face tabs tab-mark
                              spaces space-mark
                              indentation
                              space-after-tab space-before-tab
                              trailing
                              missing-newline-at-eof))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package editorconfig
  :ensure nil
  :hook
  (+late . editorconfig-mode))

(use-package apheleia :defer t)

(defun +format-buffer ()
  "Format buffer using apheleia or lsp-bridge."
  (interactive)
  (if (+has-lsp)
      (progn
        (call-interactively #'lsp-bridge-code-format)
        (message "Formatted using lsp-bridge"))
    (progn
      (call-interactively #'apheleia-format-buffer)
      (message "Formatted using apheleia"))))

(defun +set-tab-width (num)
  "Set `tab-width' to NUM."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Tab width: " tab-width))))
  (setq tab-width num)
  (message "Set tab width to %d" num))

(map! spc
  "c f" '("format" . +format-buffer)
  "c t" '("tab width" . +set-tab-width))

;;; formatting.el ends here.
