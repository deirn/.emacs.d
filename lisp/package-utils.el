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

(defmacro late! (spec &rest body)
  "Assign function NAME with BODY to `+late-hook'.
SPEC can be symbol NAME or cons (NAME . DEPTH).
NAME can be nil, in which the hook will be anonymous.
See `add-hook' for DEPTH."
  (declare (indent defun))
  (let ((name spec) depth
        (ran (bound-and-true-p +late-hook-ran)))
    (when (consp spec)
      (setq name (car spec)
            depth (cdr spec)))
    (setq name (when name (intern (concat "+late-" (symbol-name name)))))
    (if name `(progn
                (defun ,name ()
                  ,@body)
                ,(if ran `(,name)
                   `(add-hook '+late-hook #',name ,depth)))
      ;; anonymous
      (if ran `(progn ,@body)
        `(add-hook '+late-hook #'(lambda () ,@body))))))

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
(use-package package-lint)

;;; package-utils.el ends here.
