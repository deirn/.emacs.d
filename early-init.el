;;; early-init.el --- -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Suppress warning for missing `lexical-binding'.
;; https://github.com/doomemacs/doomemacs/blob/ed9190ef005829c7a2331e12fb36207794c5ad75/lisp/doom.el#L671
(setq warning-inhibit-types '((files missing-lexbind-cookie)))

;; Suppress compiler warnings.
;; https://github.com/doomemacs/doomemacs/blob/751ac6134b6abe204d9c514d300343b07b26da3c/lisp/doom.el#L609
(setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)


;; https://github.com/doomemacs/doomemacs/blob/e6c755305358412a71a990fc2cf592c629edde1e/early-init.el#L36
(if noninteractive
    (setq gc-cons-threshold 134217728  ; 128mb
          gc-cons-percentage 1.0)
  (setq gc-cons-threshold most-positive-fixnum))

;; disable builtin tool bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(set-face-attribute 'default nil
                    :background "#282c34"
                    :foreground "#bbc2cf")

;;; early-init.el ends here
