;;; evil.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Vim Emulation
;;; Code:

(use-package evil
  :after undo-fu
  :custom
  (evil-want-minibuffer t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-fu)
  (evil-respect-visual-line-mode t)
  :config
  (setq evil-insert-state-cursor `(,(face-foreground 'ansi-color-blue)    bar)
        evil-normal-state-cursor `(,(face-foreground 'ansi-color-green)   box)
        evil-emacs-state-cursor  `(,(face-foreground 'ansi-color-magenta) box)
        evil-visual-state-cursor `(,(face-foreground 'ansi-color-yellow)  box))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "g s"))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :init
  (evilnc-default-hotkeys))

(use-package evil-anzu
  :after (evil anzu))

(map! visual
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

;;; evil.el ends here.
