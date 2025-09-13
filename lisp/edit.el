;;; edit-shortcuts.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Editing Shortcut
;;; Code:

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :hook
  (prog-mode . combobulate-mode))

(use-package embark
  :config
  (+pop '(major-mode . embark-collect-mode))

  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package anzu
  :hook
  (+late . global-anzu-mode))

(map! spc
  "a"   '("act" . embark-act)
  "b c" 'combobulate)

(map! nil
  "M-e" #'embark-act
  [remap query-replace] #'anzu-query-replace
  [remap query-replace-regexp] #'anzu-query-replace-regexp)

(map! nil
  :keymaps 'isearch-mode-map
  [remap isearch-query-replace] #'anzu-isearch-query-replace
  [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;;; edit-shortcuts.el ends here.
