;;; package-lock.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst +packages-lock-file (expand-file-name "packages.lock" +deirnmacs-directory))
(defvar +packages nil)

(defun +package-lock-elpaca-function (order)
  "Add package lock for ORDER.  To be added to `elpaca-order-functions'."
  (when order
    (let ((package (if (symbolp order) order (car order))))
      (gethash package +packages))))

(defun +packages-lock-read (pin)
  "Read packages lock file.
PIN means pin the package or not"
  (let ((ht (make-hash-table))
        (alist (with-temp-buffer
                 (insert-file-contents +packages-lock-file)
                 (read (buffer-string)))))
    (dolist (e alist)
      (let ((recipe (cdr e)))
        (unless pin
          (setq recipe (plist-put recipe :ref nil)))
        (puthash (car e) recipe ht)))
    (setq +packages ht)))

(defun +packages-lock-write ()
  "Write packages lock file."
  (interactive)
  (let* ((temp (make-temp-file "elpaca-lock-"))
         (_ (elpaca-write-lock-file temp))
         (pkgs (with-temp-buffer
                 (insert-file-contents temp)
                 (read (buffer-string))))
         recipes recipes-from-url)
    (dolist (pkg pkgs)
      (let* ((name (car pkg))
             (recipe (plist-get (cdr pkg) :recipe))
             (ref (plist-get recipe :ref))
             (source (plist-get recipe :source))
             (host (or (plist-get recipe :host) (plist-get recipe :fetcher)))
             (repo (plist-get recipe :repo))
             (files (plist-get recipe :files))
             (build (plist-get recipe :build))
             (old-recipe (gethash name +packages))
             (old-unlock (plist-get old-recipe :unlock))
             (old-ref (plist-get old-recipe :ref)))
        (if source (push `(,name :ref ,ref) recipes)
          (push `(,name
                  :source nil
                  ,@(when host `(:host ,host))
                  :repo ,repo
                  ,@(if old-unlock
                        `(,@(when old-ref `(:ref ,old-ref))
                          :unlock t)
                      `(:ref ,ref))
                  ,@(when files `(:files ,files))
                  ,@(when build `(:build ,build)))
                recipes))))
    (with-temp-file (expand-file-name "packages.lock" +deirnmacs-directory)
      (insert "( ;; -*- mode: lisp-data; -*-\n")
      (dolist (recipe (nreverse recipes))
        (insert (prin1-to-string recipe) "\n"))
      (insert ")\n"))
    (delete-file temp)))

(+packages-lock-read t)
;; (+packages-lock-read nil)

;;; package-lock.el ends here.
