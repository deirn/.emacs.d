;;; kotlin.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package kotlin-ts-mode
  :mode "\\.kt\\'"
  :config
  (+ts 'kotlin "https://github.com/fwcd/tree-sitter-kotlin"))

;;; kotlin.el ends here.
