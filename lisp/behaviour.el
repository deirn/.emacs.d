;;; behaviour.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Behaviour
;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (use-dialog-box nil)
  (confirm-kill-emacs #'yes-or-no-p)

  (custom-file (expand-file-name "custom.el" user-emacs-directory))

  ;; disable annoying bell sound
  (ring-bell-function 'ignore)

  ;; less jumpy scroll
  (scroll-step 1)                     ; scroll one line at a time
  (scroll-margin 2)                   ; start scrolling when 2 lines from bottom
  (scroll-conservatively 100000)      ; never recenter automatically
  (scroll-preserve-screen-position t) ; keep point position when scrolling
  (auto-window-vscroll nil)

  ;; disable backup `.#' and lockfiles `~'
  (create-lockfiles nil)
  (make-backup-files nil)

  :config
  (fset #'yes-or-no-p #'y-or-n-p)
  (setq-default indent-tabs-mode nil))

(defun +open-config ()
  "Open Emacs configuration."
  (interactive)
  (find-file (read-file-name "Find file in config: " user-emacs-directory)))

(defun +reload-init ()
  "Reload Emacs init.el."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "init.el reloaded!"))

(map! spc
  "e c" '("open config" . +open-config)
  "e r" '("restart"     . restart-emacs)
  "e q" '("quit"        . save-buffers-kill-emacs))

(map! nil
  "M-<f4>" #'save-buffers-kill-emacs)

;;; behaviour.el ends here.
