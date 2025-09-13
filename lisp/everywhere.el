;;; everywhere.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Use Emacs everywhere
;;; Code:

(use-package tinee
  :ensure (:repo "https://codeberg.org/tusharhero/tinee.git")
  :custom
  (tinee-send-text-function 'tinee-paste))

;;; everywhere.el ends here.
