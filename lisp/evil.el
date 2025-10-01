;;; evil.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Vim Emulation
;;; Code:

(use-package evil
  :after undo-fu
  :custom
  (evil-want-minibuffer t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-fu)
  (evil-respect-visual-line-mode t))

(use-package evil-collection)
(use-package evil-surround)
(use-package evil-snipe)
(use-package evil-mc)

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "g s"))

(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys))

(use-package evil-anzu
  :after (evil anzu))

(late! evil
  (evil-mode)
  (evil-collection-init)
  (global-evil-surround-mode)
  (evil-snipe-mode)
  (evil-snipe-override-mode)
  (global-evil-mc-mode))

(map! normal
  "g . j" #'evil-mc-make-cursor-move-next-line
  "g . k" #'evil-mc-make-cursor-move-prev-line)

(map! visual
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

;;; evil.el ends here.
