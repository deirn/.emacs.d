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
  (defun +maybe-open-dashboard ()
    (when (length< command-line-args 2)
      (dashboard-open)
      (when (get-buffer "*scratch*")
        (kill-buffer "*scratch*"))))
  (late! (+maybe-open-dashboard))

  :config
  (defun +dashboard-refresh ()
    (with-current-buffer "*dashboard*"
      (let ((inhibit-read-only t))
        (dashboard-insert-startupify-lists t))))

  (defvar +dashboard-resize-timer nil
    "Idle timer for debounced dashboard refresh.")

  (defun +schedule-dashboard-refresh (&optional _)
    "Schedule dashboard refresh after resize."
    (when +dashboard-resize-timer
      (cancel-timer +dashboard-resize-timer))
    (setq +dashboard-resize-timer
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (when (get-buffer "*dashboard*")
               (+dashboard-refresh))))))

  (defun +dont-kill-dashboard-buffer ()
    (add-hook 'kill-buffer-query-functions
              (lambda ()
                (if (get-buffer-window (current-buffer) 'visible)
                    (progn (message "Cannot kill *dasboard* when it is open.")
                           nil)
                  t))
              nil t))

  :hook
  (window-size-change-functions . +schedule-dashboard-refresh)
  (dashboard-mode . +dont-kill-dashboard-buffer))

(map! spc
  "o d" '("dashboard" . dashboard-open))

;;; dashboard.el ends here.
