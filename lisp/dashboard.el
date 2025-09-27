;;; dashboard.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Dashboard
;;; Code:

(use-package dashboard
  :custom
  (inhibit-startup-screen t)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents  . 10)
                     (projects . 10)))

  :init
  (late! (open-dashboard . 90)
    (when (length< command-line-args 2)
      (dashboard-open)
      (when (get-buffer "*scratch*")
        (kill-buffer "*scratch*"))
      ;; refresh it immediately to fix image background
      (run-at-time 0 nil #'dashboard-open)))

  :config
  (defun +dashboard-inhibit-kill ()
    "Disallow killing dashboard buffer when it is visible."
    (if (not (get-buffer-window (current-buffer) 'visible)) t
      (message "Cannot kill *dasboard* when it is open.")
      nil))

  (defun +dashboard-setup ()
    (add-hook 'kill-buffer-query-functions #'+dashboard-inhibit-kill nil t)
    (keymap-local-set "<f5>" #'dashboard-open))
  :hook
  (dashboard-mode . +dashboard-setup))

(map! spc
  "o d" '("dashboard" . dashboard-open))

;;; dashboard.el ends here.
