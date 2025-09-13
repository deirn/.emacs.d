;;; language.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Languages
;;; Code:

(defvar +--nobreadcrumb nil)
(defun +nobreadcrumb (mode)
  "Disable breadcrumb for MODE."
  (add-to-list '+--nobreadcrumb mode))

(defun +ts (language url &rest conf)
  "Add new treesit LANGUAGE source from URL and extra CONF."
  (add-to-list 'treesit-language-source-alist (cons language (cons url conf))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(load! (beancount
        csv
        dart
        elisp
        markdown
        nix
        python
        shell
        systemd
        text
        web))

;;; language.el ends here.
