;;; keybind-setup.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Key Bindings Setup
;;; Code:

(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-dont-use-unicode nil)
  (which-key-min-display-lines 5)
  :config
  (define-advice which-key--side-window-max-dimensions (:override () center)
    (cons 10 200))

  (define-advice which-key--show-buffer-side-window (:around (orig-fn dim) center)
    (let* ((window (funcall orig-fn dim))
           (char-width (frame-char-width))
           (frame-width (frame-width))
           (wk-width (cdr dim))
           (fringe (round (/ (- frame-width wk-width) 2)))
           (fringe (* char-width (max 0 fringe))))
      (set-window-fringes window fringe fringe)))
  :hook
  (+late . which-key-mode))

(use-package general
  :config
  (global-unset-key (kbd "C-SPC"))
  (general-create-definer +key-spc
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer +key-normal
    :states 'normal
    :keymaps 'override)

  (general-create-definer +key-visual
    :states 'visual
    :keymaps 'override))

(defvar +map-n nil)
(defmacro map! (definer &rest rules)
  "Map RULES for DEFINER."
  (declare (indent defun))
  (let* ((file (or load-file-name buffer-file-name))
         (file (file-truename file))
         (file (file-relative-name file +deirnmacs-lisp-dir))
         (file (file-name-sans-extension file))
         (key (intern file))
         (n (1+ (or (plist-get +map-n key) 0)))
         (name (intern (format "map-%s-%s" file n)))
         (spec (cons name 99)))
    (setq +map-n (plist-put +map-n key n))
    (if (null definer)
        `(late! ,spec (general-def ,@rules))
      (let ((definer (intern (concat "+key-" (symbol-name definer)))))
        `(late! ,spec (,definer ,@rules))))))

(map! spc
  "b" '(:ignore t :which-key "buffer")
  "c" '(:ignore t :which-key "code")
  "e" '(:ignore t :which-key "emacs")
  "f" '(:ignore t :which-key "file")
  "g" '(:ignore t :which-key "git")
  "o" '(:ignore t :which-key "open")
  "p" '(:ignore t :which-key "project")
  "s" '(:ignore t :which-key "search")
  "t" '(:ignore t :which-key "toggle")
  "w" '(:ignore t :which-key "window"))

(map! spc
  "e k" '("keybinds" . which-key-show-top-level))

;;; keybind-setup.el ends here.
