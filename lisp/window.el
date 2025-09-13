;;; window.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Window
;;; Code:

(use-package frame
  :ensure nil
  :custom
  (window-divider-default-bottom-width 5)
  (window-divider-default-right-width 5)
  :hook
  (+late . window-divider-mode))

(use-package ace-window
  :commands (ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(defun +last-focused-window ()
  "Go to the last focused window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun +split-window-left ()
  "Split window to the left, focus the left window."
  (interactive)
  (call-interactively #'evil-window-vsplit))

(defun +split-window-right ()
  "Split window to the right, focus the right window."
  (interactive)
  (call-interactively #'evil-window-vsplit)
  (call-interactively #'evil-window-right))

(defun +split-window-up ()
  "Split window to the up, focus the up window."
  (interactive)
  (call-interactively #'evil-window-split))

(defun +split-window-down ()
  "Split window to the down, focus the down window."
  (interactive)
  (call-interactively #'evil-window-split)
  (call-interactively #'evil-window-down))

(defun +window-increase-width ()
  "Increase window width."
  (interactive)
  (if (+is-dirvish-side)
      (call-interactively #'dirvish-side-increase-width)
    (call-interactively #'evil-window-increase-width)))

(defun +window-decrease-width ()
  "Decrease window width."
  (interactive)
  (if (+is-dirvish-side)
      (call-interactively #'dirvish-side-decrease-width)
    (call-interactively #'evil-window-decrease-width)))

(map! spc
  "w h" '("left"            . evil-window-left)
  "w H" '("split left"      . +split-window-left)
  "w j" '("down"            . evil-window-down)
  "w J" '("split down"      . +split-window-down)
  "w k" '("up"              . evil-window-up)
  "w K" '("split up"        . +split-window-up)
  "w l" '("right"           . evil-window-right)
  "w L" '("split right"     . +split-window-right)
  "w m" '("maximize"        . delete-other-windows)
  "w q" '("kill window"     . delete-window)
  "w w" '("switch window"   . ace-window)
  "w =" '("increase width"  . +window-increase-width)
  "w -" '("decrease width"  . +window-decrease-width)
  "w +" '("increase height" . +window-increase-height)
  "w _" '("decrease height" . +window-decrease-height))

;;; window.el ends here.
