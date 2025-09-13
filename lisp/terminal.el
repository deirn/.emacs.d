;;; terminal.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Terminal
;;; Code:

(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  (+pop '(this-command . vterm-other-window)))

(map! spc
  "o t" '("terminal"      . vterm-other-window)
  "o T" '("terminal here" . vterm))

;;; terminal.el ends here.
