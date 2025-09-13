;;; web.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package js
  :ensure nil
  :mode ("\\.[mc]?js\\'" . js-ts-mode))

(use-package svelte-ts-mode
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :config
  (dolist (e svelte-ts-mode-language-source-alist) (apply '+ts e))
  (+lsp 'svelte-ts-mode "svelteserver"))

;;; web.el ends here.
