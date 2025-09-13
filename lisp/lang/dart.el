;;; dart.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dart-ts-mode
  :ensure (:host github :repo "50ways2sayhard/dart-ts-mode")
  :mode "\\.dart\\'"
  :config
  (+ts 'dart "https://github.com/UserNobody14/tree-sitter-dart")
  (+lsp 'dart-ts-mode "dart-analysis-server"))

;;; dart.el ends here.
