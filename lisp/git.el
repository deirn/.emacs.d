;;; git.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Git
;;; Code:

(setq vc-follow-symlinks t)

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package magit
  :commands (magit-status)
  :hook
  (magit-mode . +killable-mode)
  (magit-process-mode . +killable-mode))

(use-package diff-hl
  :config
  ;; https://github.com/dgutov/diff-hl/issues/116#issuecomment-1573253134
  (let* ((width 2)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap '+diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) '+diff-hl-bitmap))

  (set-face-background 'diff-hl-insert nil)
  (set-face-background 'diff-hl-change nil)
  (set-face-background 'diff-hl-delete nil)
  :hook
  (+late . global-diff-hl-mode))

(use-package git-modes
  :mode
  (("\\.gitignore\\'"     . gitignore-mode)
   ("\\.gitconfig\\'"     . gitconfig-mode)
   ("\\.gitmodules\\'"    . gitconfig-mode)
   ("\\.gitattributes\\'" . gitattributes-mode)))

(map! spc
  "g b" '("blame" . magit-blame)
  "g d" '("diff"  . magit-diff)
  "g g" '("git"   . magit-status)
  "g h" '("hunk"  . diff-hl-show-hunk))

;;; git.el ends here.
