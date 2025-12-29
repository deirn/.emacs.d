;;; _index.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! lsp-bridge)

(use-package mason
  :config
  (late! (mason . 99)
    (mason-ensure)))

(map! spc
  "l m" '("mason" . mason-manager))

(map! normal
  "K" #'+show-documentation)

;;; _index.el ends here.
