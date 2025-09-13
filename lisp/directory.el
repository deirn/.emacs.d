;;; directory.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Directory
;;; Code:

(use-package dirvish
  :custom
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-alh --group-directories-first")
  (dirvish-subtree-state-style 'nerd)
  (dirvish-attributes '(nerd-icons vc-state file-size file-time))
  (dirvish-side-attributes '(nerd-icons collapse vc-state file-size))
  (dirvish-side-root-window-fn)
  (dirvish-side-display-alist '((side . left)))
  (dirvish-side-window-parameters '((+dirvish-side . t)
                                    (no-delete-other-windows . t)))
  (dirvish-header-line-height 24)
  (dirvish-mode-line-height 24)
  (dirvish-collapse-separator "/")
  (dirvish-subtree-skip-intermediate-folders t)
  :config
  (dirvish-side-follow-mode 1)
  (put 'dired-find-alternate-file 'disabled nil)

  (defun +dirvish-side-revert ()
    "Revert dirvish side window."
    (interactive)
    (when-let* ((window (dirvish-side--session-visible-p)))
      (with-current-buffer (window-buffer window)
        (revert-buffer))))
  :hook
  (+late . dirvish-override-dired-mode)
  (dired-mode . dired-omit-mode)
  (after-save . +dirvish-side-revert))

(use-package diredfl
  :after dirvish
  :hook
  (dired-mode . diredfl-mode)
  (dirvish-directory-view-mode . diredfl-mode))

(defun +is-dirvish-side (&optional window)
  "Returns t if WINDOW is `dirvish-side'."
  (window-parameter window '+dirvish-side))

(defun +dirvish-quit ()
  "Dirvish q key."
  (interactive)
  (if (+is-dirvish-side)
      (+last-focused-window)
    (quit-window)))

(defun +dirvish-open-file ()
  "Open file in main window, or open directory in current window."
  (interactive)
  (if (not (+is-dirvish-side))
      (call-interactively #'dired-find-alternate-file)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (call-interactively #'dired-find-alternate-file)
        (progn
          (select-window (get-mru-window nil t t))
          (find-file file))))))

(map! spc
  "f f" '("find"         . find-file)
  "f s" '("save"         . save-buffer)
  "f r" '("recent files" . consult-recent-file)

  "o o" '("dired"        . dired)
  "o p" '("project view" . dirvish-side)

  "p f" '("find file"    . project-find-file)
  "p p" '("switch"       . project-switch-project))

(map! nil
  :keymaps 'dirvish-mode-map
  :states '(normal motion)
  "q"   #'+dirvish-quit
  "TAB" #'dirvish-subtree-toggle
  "H"   #'dirvish-subtree-up
  "h"   #'dired-up-directory
  "l"   #'+dirvish-open-file
  "RET" #'+dirvish-open-file)

(map! nil
  :keymaps 'dired-mode-map
  [remap dired-do-symlink] #'dirvish-symlink
  [remap dired-do-copy] #'dirvish-yank)

;;; directory.el ends here.
