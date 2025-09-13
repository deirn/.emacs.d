;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "30") (error "This requires Emacs 30 and above!"))

(defun -load-abs (file no-error)
  (when (file-directory-p (file-name-as-directory file))
    (setq file (expand-file-name "_index" file)))
  (load file no-error))

(defun -load (file dir no-error)
  (if (file-name-absolute-p file)
      (-load-abs file no-error)
    (when (not dir)
      (setq dir (file-name-directory load-file-name)
	        file (expand-file-name file dir)))
    (-load-abs file no-error)))

(defmacro load! (file &optional dir no-error)
  "Load FILE(s) relative to DIR.
FILE can also be absolute, in which it loaded directly.

If DIR is nil and FILE is not absolute, it will be expanded from
the calling file directory.

If NO-ERROR is not nil, report no error."
  (if (not (listp file))
      `(-load ,(symbol-name file) ,dir ,no-error)
    `(progn ,@(mapcar (lambda (f) `(load! ,f ,dir ,no-error))
                      file))))

(defgroup +deirn nil
  "Deirn's custom group."
  :group 'emacs)

(load! lisp)

;;; init.el ends here.
(put 'list-threads 'disabled nil)
