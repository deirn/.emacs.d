;;; discord.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Discord
;;; Code:

(use-package elcord
  :custom
  (elcord-use-major-mode-as-main-icon t)
  :hook
  (+late . elcord-mode))

(map! spc
  "t d" '("discord rich presence" . elcord-mode))

;;; discord.el ends here.
