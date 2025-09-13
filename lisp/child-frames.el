;;; child-frames.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Child Frames
;;; Code:

(use-package transient
  :custom
  (transient-mode-line-format nil))

(use-package posframe)
(defvar +posframe-y-offset 100)
(defvar +posframe-border-color "#bbc2cf")
(defvar +posframe-params '((left-fringe 10)
                           (right-fringe 10)))

(when (display-graphic-p)
  (defun +posframe-poshandler (info)
    (let ((top-center (posframe-poshandler-frame-top-center info)))
      (cons (car top-center) +posframe-y-offset)))

  (use-package which-key-posframe
    :custom
    (which-key-posframe-parameters `(,@+posframe-params
                                     (z-group . above)))
    :config
    (defun +which-key-posframe-poshandler (info)
      (if-let* ((og-pos (+posframe-poshandler info))
                (vertico-buffer (bound-and-true-p vertico-posframe--buffer))
                (vertico-posframe (posframe--find-existing-posframe vertico-buffer))
                (_ (eq (window-buffer (frame-root-window vertico-posframe)) vertico-buffer))
                (_ (frame-visible-p vertico-posframe)))
          (cons (car og-pos) (+ +posframe-y-offset (frame-pixel-height vertico-posframe) -1))
        og-pos))
    (setq which-key-posframe-poshandler #'+which-key-posframe-poshandler)

    (define-advice which-key-posframe--show-buffer (:around (orig-fn dim) fix-dim)
      (setf (car dim)
            (with-current-buffer which-key--buffer
              (count-lines (point-min) (point-max))))
      (setf (cdr dim)
            (max (cdr dim)
                 (with-current-buffer which-key--buffer
                   (save-excursion
                     (goto-char (point-min))
                     (1- (length (thing-at-point 'line t)))))))
      (funcall orig-fn dim))

    (set-face-background 'which-key-posframe-border +posframe-border-color)
    :hook
    (+late . which-key-posframe-mode))

  ;; https://github.com/emacsorphanage/transient-posframe/wiki
  (after! (transient posframe)
    (setq transient-display-buffer-action
          (list
           (lambda (buffer _)
             (posframe-show
              buffer
              :poshandler #'+posframe-poshandler
              :min-width transient-minimal-frame-width
              :lines-truncate t
              :internal-border-color +posframe-border-color
              :internal-border-width 1
              :override-parameters +posframe-params)
             (get-buffer-window transient--buffer t)))))

  (use-package vertico-posframe
    :after vertico
    :custom
    (vertico-posframe-border-width 1)
    (vertico-posframe-poshandler #'+posframe-poshandler)
    (vertico-posframe-parameters +posframe-params)
    (vertico-multiform-commands '((t posframe)))
    :config
    (define-advice vertico-posframe--get-border-color (:override () all-same-color)
      +posframe-border-color)

    (defun +vertico-posframe-get-size (buf)
      (let ((h (1+ vertico-count))
            (w (round (* (frame-width) 0.62))))
      (list :min-height h :min-width w
            :max-height h :max-width w)))
    (setq vertico-posframe-size-function #'+vertico-posframe-get-size)
    :hook
    (+late . vertico-multiform-mode))

  (use-package mini-frame
    :custom
    (mini-frame-detach-on-hide nil)
    (mini-frame-show-parameters `((left . 0.5)
                                  (top . ,+posframe-y-offset)
                                  (width . 0.6)
                                  (no-accept-focus . t)
                                  (child-frame-border-width . 1)
                                  (background-color . "#21242b")))
    :config
    (add-to-list 'mini-frame-advice-functions 'map-y-or-n-p)
    (add-to-list 'mini-frame-ignore-functions 'completing-read)
    (add-to-list 'mini-frame-ignore-commands 'elpaca-ui-search)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex)
    (add-to-list 'mini-frame-ignore-commands 'shell-command)
    (set-face-background 'child-frame-border +posframe-border-color)
    :hook
    (+late . mini-frame-mode)))

;;; child-frames.el ends here.
