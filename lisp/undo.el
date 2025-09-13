;;; undo.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Undo/History
;;; Code:

(use-package savehist
  :ensure nil
  :custom
  (history-length 1000)
  (savehist-autosave-interval 60)
  (savehist-additional-variables '(search-ring
                                   regexp-search-ring
                                   extended-command-history))
  :hook
  (+late . save-place-mode)
  (+late . savehist-mode))

(use-package undo-fu
  :custom
  (undo-limit 256000)
  (undo-strong-limit 2000000)
  (undo-outer-limit 36000000))

(use-package undo-fu-session
  :after undo-fu
  :custom
  (undo-fu-session-compression 'zst)
  :hook
  (+late . global-undo-fu-session-mode))

;;; undo.el ends here.
