;;; minibuffer.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun +minibuffer-fringe-update (window)
  "Upadate fringe for minibuffer WINDOW."
  (let* ((char-width (frame-char-width))
         (frame-width (frame-width))
         (minibuffer-width (min frame-width 200))
         (fringe (round (/ (- frame-width minibuffer-width) 2)))
         (fringe (* char-width (max 0 fringe))))
    (set-window-fringes window fringe fringe)))

(defun +minibuffer-fringe-setup ()
  "Adjust minibuffer WINDOW fringe."
  (let* ((window (selected-window))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (+minibuffer-fringe-update window)
      (add-hook 'window-size-change-functions #'+minibuffer-fringe-update 0 t))))
(add-hook 'minibuffer-setup-hook #'+minibuffer-fringe-setup)

(defun +center-message-function (msg)
  "Center echo area MSG."
  (when (not (minibufferp (window-buffer)))
    (setq msg (string-trim msg))
    (let* ((frame-width (frame-width))
           (msg-width (string-width msg))
           (spc-width (round (/ (- frame-width msg-width) 2)))
           (spc-width (max 0 spc-width))
           (spc (make-string spc-width ?\s)))
      (setq msg (concat spc msg))))
  (set-message-functions msg))

(setq set-message-function #'+center-message-function)

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  (xref-prompt-for-identifier nil)
  :config
  (define-advice xref--show-xrefs (:before (&rest _) evil-jump-list)
    "Add to evil jump list before showing xrefs."
    (evil-set-jump)))

(use-package consult
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(defun +consult-fd-with-dir ()
  "Run `consult-fd` with universal argument to prompt for directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; simulate C-u
    (call-interactively #'consult-fd)))

(defun +consult-ripgrep-with-dir ()
  "Run `consult-ripgrep` with universal argument to prompt for directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; simulate C-u
    (call-interactively #'consult-ripgrep)))

(use-package embark-consult :after embark)

(use-package vertico
  :after embark
  :config
  (vertico-mode 1))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(use-package nerd-icons-completion
  :hook
  (+late . nerd-icons-completion-mode)
  (marginalia-mode . #'nerd-icons-completion-marginalia-setup))

(use-package imenu-list
  :custom
  (imenu-list-auto-update nil))

(use-package wgrep)

(map! spc
  "s b" '("buffer"       . consult-buffer)
  "s e" '("error"        . consult-flymake)
  "s f" '("fd project"   . consult-fd)
  "s F" '("fd directory" . +consult-fd-with-dir)
  "s i" '("imenu"        . consult-imenu)
  "s j" '("jump"         . evil-collection-consult-jump-list)
  "s l" '("line"         . consult-line)
  "s L" '("line multi"   . consult-line-multi)
  "s r" '("rg project"   . consult-ripgrep)
  "s R" '("rg directory" . +consult-ripgrep-with-dir))

(map! normal
  "g d" #'xref-find-definitions
  "g r" #'xref-find-references)

(map! nil
  :keymaps 'minibuffer-local-map
  "M-a" #'marginalia-cycle)

;;; minibuffer.el ends here.
