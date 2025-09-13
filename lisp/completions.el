;;; completions.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Completions
;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :hook
  (+late . minibuffer-depth-indicate-mode)
  (+late . context-menu-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode text-mode markdown-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package yasnippet
  :defer t
  :hook
  (+late . yas-global-mode))

(use-package yasnippet-snippets :after yasnippet :defer t)

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(use-package corfu
  :custom
  (corfu-auto t)
  (global-corfu-minibuffer nil)
  :hook
  (+late . global-corfu-mode))

(use-package nerd-icons-corfu
  :config
  (defun +nic-alias (alias from)
    "Add ALIAS mapping FROM `nerd-icons-corfu-mapping'."
    (add-to-list 'nerd-icons-corfu-mapping (cons alias (cdr (assoc from nerd-icons-corfu-mapping)))))

  (+nic-alias 'feature 'keyword)
  (+nic-alias (intern "special form") 'keyword)
  (+nic-alias 'custom 'variable)
  (+nic-alias 'search 'text)
  (+nic-alias 'face 'color)

  (after! corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(map! nil
  "C-c p" #'cape-prefix-map)

;;; completions.el ends here.
