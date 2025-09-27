;;; patch.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Package patch using `el-patch'
;;; Code:

(use-package el-patch)

(defconst +patch-dir (expand-file-name "patches" user-emacs-directory))
(make-directory +patch-dir t)

(defmacro patch! (type name &rest templates)
  "Call `el-patch-define-template' with TYPE, NAME and TEMPLATES.
Save to a cache file, and load it if it exists."
  (declare (indent defun))
  (let* ((type-s (symbol-name type))
         (name-s (symbol-name name))
         (cache (expand-file-name (concat type-s "-" name-s ".el") +patch-dir))
         (elc-cache (file-name-with-extension cache "elc")))
    `(late! ,(intern (concat "patch-" type-s "-" name-s))
       (if (file-readable-p ,elc-cache) (load ,elc-cache)
         (el-patch-define-template ,(list type name) ,@templates)
         (cond
          ((el-patch-validate-template ',name ',type)
           (with-temp-file ,cache
             (insert ";; " (current-time-string) " -*- lexical-binding: t; -*-\n")
             (el-patch-insert-template ',name ',type))
           (el-patch-eval-template ',name ',type)
           (byte-compile-file ,cache))
          (t (error "Invalid template")))))))

(defun +delete-patch-cache ()
  "Delete patch cache folder, regenerate next reload."
  (interactive)
  (delete-directory +patch-dir t nil)
  (make-directory +patch-dir t)
  (message "Deleted %s" +patch-dir))

;;; patch.el ends here.
