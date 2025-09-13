;;; csv.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :mode
  ("\\.csv\\'" . csv-mode)
  ("\\.tsv\\'" . tsv-mode))

(use-package rainbow-csv
  :hook (csv-mode tsv-mode))

;;; csv.el ends here.
