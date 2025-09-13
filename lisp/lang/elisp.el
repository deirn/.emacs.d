;;; elisp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! ../thirdparty/fuco1-indent)

(setq lisp-indent-function #'+fuco1-lisp-indent-function)

(use-package helpful
  :commands (helpful-at-point)
  :config
  (+pop '(major-mode . helpful-mode))
  :hook
  (helpful-mode . +killable-mode))

(use-package elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package elisp-def
  :hook
  (emacs-lisp-mode . elisp-def-mode))

(use-package pcre2el)

;; (load! ../thirdparty/ouroboroslisp-indent)

;;; elisp.el ends here.
