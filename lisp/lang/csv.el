;;; csv.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :mode
  ("\\.csv\\'" . csv-mode)
  ("\\.tsv\\'" . tsv-mode))

(use-package rainbow-csv
  :ensure ( :host github
            :repo "emacs-vs/rainbow-csv"
            ;; FIXME: latest commit breaks csv without quotes
            ;; https://github.com/emacs-vs/rainbow-csv/commit/0865b7c0e95ca1f8ce561df55d497d2e90774efc
            :ref "46a1acc8f981bfda254c88e7390f8c94837aead2")
  :hook (csv-mode tsv-mode))

;;; csv.el ends here.
