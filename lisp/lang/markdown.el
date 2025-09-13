;;; markdown.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-display-remote-images t)
  (markdown-max-image-size '(200 . nil))
  :config
  (+nobreadcrumb 'gfm-mode)
  :hook
  (gfm-view-mode . markdown-display-inline-images))

;;; markdown.el ends here.
