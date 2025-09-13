;;; systemd.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package systemd
  :mode ("\\.service\\'" . systemd-mode)
  :init
  (add-hook 'systemd-mode-hook #'display-line-numbers-mode))

;;; systemd.el ends here.
