;;; debug.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Debugging
;;; Code:

(use-package flymake
  :ensure nil
  :config
  (set-face-italic 'flymake-warning-echo nil)

  (defvar +flymake-buffers nil
    "Saved flymake buffers.")

  (define-advice flymake-show-buffer-diagnostics (:before (&rest _) quit-other)
    "Quit other flymake windows before opeing new one."
    (dolist (name +flymake-buffers)
      (when-let* ((buf (get-buffer name))
                  (win (get-buffer-window buf)))
        (quit-window t win)))
    (setq +flymake-buffers nil))

  (define-advice flymake-show-buffer-diagnostics (:after (&rest _) save-buffer)
    "Save flymake buffers."
    (add-to-list '+flymake-buffers (flymake--diagnostics-buffer-name)))

  (+pop '(major-mode flymake-diagnostics-buffer-mode) 'bottom)
  :hook
  (prog-mode . flymake-mode)
  (flymake-diagnostics-buffer-mode . +killable-mode))

(use-package flymake-popon
  :custom
  (flymake-popon-width 100)
  :config
  (set-face-foreground 'flymake-popon-posframe-border "#323232")
  (setq flymake-popon-posframe-extra-arguments (plist-put flymake-popon-posframe-extra-arguments :background-color "#202329"))

  (define-advice flymake-goto-next-error (:around (orig-fn &rest args) no-message)
    "Disable message when jumping to errors."
    (define-advice message (:override (&rest _) noop))
    (apply orig-fn args)
    (advice-remove 'message #'message@noop))

  (define-advice flymake-eldoc-function (:override (&rest _) disable)
    "Disable eldoc function for flymake."
    nil)
  :hook
  (flymake-mode . flymake-popon-mode))

(use-package dape
  :commands (dape)
  :custom
  (dape-buffer-window-arrangement 'left)
  :hook
  (kill-emacs . dape-breakpoint-save)
  (+late . dape-breakpoint-load)
  :config
  (dape-breakpoint-global-mode 1))

(use-package ansi-color
  :ensure nil
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(map! spc
  "c c" '("compile" . compile)
  "d"   '(:keymap dape-global-map :package dape :which-key "dape")
  "o e" '("error"   . flymake-show-buffer-diagnostics)
  "p c" '("compile" . project-compile))

(map! normal
  "] e" #'flymake-goto-next-error
  "[ e" #'flymake-goto-prev-error)

;;; debug.el ends here.
