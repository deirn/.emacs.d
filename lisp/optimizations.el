;;; optimizations.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package benchmark-init
  :config
  (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

;;; optimizations.el ends here.
