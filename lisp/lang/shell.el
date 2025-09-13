;;; shell.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package fish-mode
  :mode "\\.fish\\'"
  :config
  (+nobreadcrumb 'fish-mode))

(use-package sh-script
  :ensure nil
  :config
  (+nobreadcrumb 'bash-ts-mode))

;;; shell.el ends here.
