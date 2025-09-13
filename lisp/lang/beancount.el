;;; beancount.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode))

;;; beancount.el ends here.
