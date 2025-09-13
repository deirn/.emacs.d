;;; keybind-setup.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Key Bindings Setup
;;; Code:

(use-package which-key
  :ensure nil
  :custom
  (which-key-show-prefix 'top)
  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-dont-use-unicode nil)
  (which-key-min-display-lines 5)
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

(defmacro map! (definer &rest rules)
  "Map RULES for DEFINER."
  (declare (indent defun))
  (if (eq definer nil)
      `(late! (general-def ,@rules))
    (let ((definer (intern (concat "+key-" (symbol-name definer)))))
      `(late! (,definer ,@rules)))))

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
