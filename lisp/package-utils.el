;;; package-utils.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Package Utilities
;;; Code:

(defun +elpaca-name (&optional interactive)
  "Prompt for package, copy its name to kill ring if INTERACTIVE."
  (interactive (list t))
  (when-let* ((name-symbol (car (elpaca-menu-item)))
              (name (symbol-name name-symbol)))
    (when interactive
      (kill-new name))
    name))

(defun +elpaca-insert-name ()
  "Prompt for package, insert its name at point."
  (interactive)
  (insert (+elpaca-name)))

(defvar +late-hook-ran nil)
(defcustom +late-hook nil
  "Hook that runs after startup."
  :type 'hook)

(defmacro late! (&rest body)
  "Delay running BODY until after startup."
  (declare (indent defun))
  (if (bound-and-true-p +late-hook-ran)
      `(progn ,@body)
    `(add-hook '+late-hook #'(lambda () ,@body))))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (run-with-idle-timer
             0.2 nil
             (lambda ()
               (load! custom user-emacs-directory t)
               (run-hooks '+late-hook)
               (setq +late-hook-ran t)
               (load! init-private user-emacs-directory t)))))

(defmacro after! (package &rest body)
  "Delay running BODY until PACKAGE(s) loaded.
Usage:
  (after! pkg (message \"pkg loaded\"))
  (after! (pkg1 pkg2) (message \"pkg1 and pkg2 loaded\"))"
  (declare (indent defun))
  (if (not (listp package))
      `(with-eval-after-load ',package ,@body)
    (let ((pkg (car package)))
      (dolist (next (reverse (cdr package)))
        (setq body `((after! ,next ,@body))))
      `(after! ,pkg ,@body))))

(use-package el-patch)

;;; package-utils.el ends here.
