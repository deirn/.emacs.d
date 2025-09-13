;;; ui.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   UI Themes
;;; Code:

(set-face-attribute 'default nil
                    :family "JetBrainsMono NF"
                    :height 105)
(set-face-attribute 'fixed-pitch nil :family "JetBrainsMono NF")

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode))

(late!
  (global-hl-line-mode)
  (line-number-mode)
  (column-number-mode))

(use-package nerd-icons :defer t
  :custom
  (nerd-icons-default-adjust 0.1))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-minor-modes t)
  :hook
  (+late . doom-modeline-mode))

(use-package minions
  :hook
  (+late . minions-mode))

(use-package hide-mode-line
  :commands (hide-mode-line-mode
             global-hide-mode-line-mode))

(use-package solaire-mode
  :hook
  (+late . solaire-global-mode))

(use-package vi-tilde-fringe
  :hook
  (+late . global-vi-tilde-fringe-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package page-break-lines
  :hook
  (+late . global-page-break-lines-mode))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("FIXME" error bold)))
  :config
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake)
  :hook
  (+late . global-hl-todo-mode))

(use-package perfect-margin
  :commands (perfect-margin-mode)
  :config
  (define-advice perfect-margin--supported-side-window-p (:around (orig-fn win) more-side-window)
    (or (+is-dirvish-side win)
        (funcall orig-fn win))))

(define-minor-mode +zen-mode
  "Toggle Zen Mode."
  :global t
  :init-value nil
  (let ((enable (if +zen-mode 1 -1)))
    (global-hide-mode-line-mode enable)
    (perfect-margin-mode enable)))

(map! spc
  "t m"   '("mode line"        . hide-mode-line-mode)
  "t M-m" '("global mode line" . global-hide-mode-line-mode)
  "t M"   '("menu bar"         . menu-bar-mode)
  "t n"   '("line numbers"     . display-line-numbers-mode)
  "t p"   '("perfect margin"   . perfect-margin-mode)
  "t w"   '("wrap"             . visual-line-mode)
  "t z"   '("zen"              . +zen-mode)
  "t SPC" '("whitespace"       . whitespace-mode))

;;; ui.el ends here.
