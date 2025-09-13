;;; lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   LSP
;;; Code:

(defun +lsp (mode command)
  "Set lsp-bridge server COMMAND for MODE."
  (after! lsp-bridge
    (add-to-list 'lsp-bridge-single-lang-server-mode-list (cons mode command))))

(use-package mason
  :ensure (:host github :repo "deirn/mason.el")
  :hook
  (+late . mason-ensure))

(use-package lsp-bridge
  :after (yasnippet markdown-mode orderless nerd-icons-corfu el-patch)
  :ensure (:host github :repo "manateelazycat/lsp-bridge"
           :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not elpaca--byte-compile))
  :custom
  ;; use uv, so it has consistent package version
  (lsp-bridge-python-command "uv")

  (lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge/langserver" user-emacs-directory))
  (lsp-bridge-user-multiserver-dir (expand-file-name "lsp-bridge/multiserver" user-emacs-directory))

  (lsp-bridge-enable-hover-diagnostic t)

  ;; use flymake-bridge, see below
  (lsp-bridge-diagnostic-enable-overlays nil)

  ;; Use minibuffer instead of popup for code action
  (lsp-bridge-code-action-preview-delay nil)
  (lsp-bridge-code-action-enable-popup-menu nil)

  ;; manually enabled below
  (lsp-bridge-enable-mode-line nil)

  (lsp-bridge-enable-completion-in-minibuffer t)
  (lsp-bridge-enable-completion-in-string t)
  (lsp-bridge-symbols-enable-which-func t)

  ;; open reference match on current main window
  (lsp-bridge-ref-open-file-in-request-window t)
  ;; don't delete other main windows
  (lsp-bridge-ref-delete-other-windows nil)
  ;; don't kill match buffers when quiting reference search
  (lsp-bridge-ref-kill-temp-buffer-p nil)

  (lsp-bridge-imenu-function 'consult-imenu)

  (acm-enable-capf t)
  (acm-enable-icon t)
  (acm-enable-tabnine nil)
  (acm-candidate-match-function #'orderless-flex)

  (acm-enable-quick-access t)
  :config
  ;; Sort snippets first, then other candidates
  ;; (delete "template-first-part-candidates" acm-completion-backend-merge-order)
  ;; (setq acm-completion-backend-merge-order (cons "template-first-part-candidates" acm-completion-backend-merge-order))

  ;; use nerd icons
  (defvar +acm-nerd-icon-mapper
    '(("material" :fn nerd-icons-mdicon :prefix "nf-md-")
      ("octicons" :fn nerd-icons-octicon :prefix "nf-oct-")
      ("codicons" :fn nerd-icons-codicon :prefix "nf-cod-")
      ("cod" :fn nerd-icons-codicon :prefix "nf-cod-")))

  (define-advice acm-icon-build (:around (orig-fn collection name fg-color) use-nerd-icons)
    (if-let* ((_ acm-enable-icon)
              (map (cdr (assoc collection +acm-nerd-icon-mapper)))
              (nf-fn (plist-get map :fn))
              (prefix (plist-get map :prefix))
              (nf-name (concat prefix name))
              (face `(:foreground ,fg-color)))
        (concat " " (funcall nf-fn nf-name :face face) nerd-icons-corfu--space)
      (funcall orig-fn collection name fg-color)))

  (define-advice lsp-bridge-breadcrumb--icon (:around (orig-fn kind active) use-nerd-icons)
    (if-let* ((_ lsp-bridge-breadcrumb-show-icon)
              (icon (cdr (assoc (downcase kind) acm-icon-alist)))
              (icon (or icon (cdr (assoc t acm-icon-alist))))
              (collection (nth 0 icon))
              (_ (assoc collection +acm-nerd-icon-mapper))
              (name (nth 1 icon))
              (fg-color (if active (nth 2 icon) (face-foreground 'mode-line-inactive))))
        (acm-icon-build collection name fg-color)
      (funcall orig-fn kind active)))

  ;; Use the same icons as nerd-icons-corfu
  (dolist (l nerd-icons-corfu-mapping)
    (when-let* ((key (symbol-name (car l)))
                (_ (assoc key acm-icon-alist))
                (map (cdr l))
                (style (plist-get map :style))
                (icon (plist-get map :icon))
                (face (plist-get map :face))
                (color (face-foreground face)))
      (setf (cdr (assoc key acm-icon-alist)) (list style icon color))))

  ;; Replace - with _ as that's what nerd icons uses
  (dolist (map acm-icon-alist)
    (when (assoc (nth 1 map) +acm-nerd-icon-mapper)
      (setf (nth 2 map) (replace-regexp-in-string "-" "_" (nth 2 map)))))

  ;; Show quick access key without dot
  (el-patch-define-template
   (defun acm-menu-render-items)
   (el-patch-swap (concat quick-access-key ". ")
                  (concat (propertize quick-access-key 'face annotation-face) nerd-icons-corfu--space)))
  (late! (el-patch-eval-template 'acm-menu-render-items 'defun))

  (evil-set-initial-state 'lsp-bridge-ref-mode 'insert)
  (+pop "*lsp-bridge-doc*")
  (+pop '(major-mode . lsp-bridge-ref-mode) 'bottom)

  (defcustom +lsp-bridge-root-config '()
    "LSP-Bridge root directories."
    :type '(repeat directory)
    :group '+deirn)

  ;; Get root folder from custom alist
  (setq lsp-bridge-get-project-path-by-filepath
        (lambda (file-path)
          (cl-find-if (lambda (e) (string-prefix-p e file-path))
                      +lsp-bridge-root-config)))

  ;; Add to jump list before going to definition, impl, etc
  (advice-add 'lsp-bridge--record-mark-ring :before #'evil-set-jump)

  (define-advice lsp-bridge-ref-open-file (:around (orig-fn &rest args) set-jump)
    "Add to jump list before opening result file."
    (define-advice find-file (:before (&rest _) set-jump-inner) (evil-set-jump))
    (apply orig-fn args)
    (advice-remove 'find-file #'find-file@set-jump-inner))

  ;; (define-advice lsp-bridge--mode-line-format (:filter-return (ret) rocket)
  ;;   "Replace `lsp-bridge' mode line string with a rocket icon."
  ;;   (when ret
  ;;     (let ((face (doom-modeline-face (get-text-property 0 'face ret))))
  ;;       (propertize (nerd-icons-mdicon "nf-md-rocket") 'face face))))

  (define-advice lsp-bridge--enable (:after () extra)
    (corfu-mode -1)
    (setq-local completion-in-region-function (lambda (&rest _) (call-interactively #'lsp-bridge-popup-complete-menu)))
    (unless (member major-mode +--nobreadcrumb) (lsp-bridge-breadcrumb-mode 1)))

  (define-advice lsp-bridge--disable (:after () extra)
    (corfu-mode 1)
    (lsp-bridge-breadcrumb-mode -1)
    (lsp-bridge-kill-process))

  (defvar +lsp-bridge-doc-mode-map (make-sparse-keymap))
  (define-minor-mode +lsp-bridge-doc-mode
    "Minor mode for *lsp-bridge-doc* buffer.")

  (defun +setup-lsp-bridge-doc-buffer ()
    "Setup *lsp-bridge-doc* buffer."
    (when-let* ((buf (get-buffer "*lsp-bridge-doc*")))
      (with-current-buffer buf
        (unless +lsp-bridge-doc-mode
          (+lsp-bridge-doc-mode 1))
        (display-line-numbers-mode -1)
        (general-define-key
         :keymaps 'local
         :states '(normal motion)
         "q" #'quit-window))))
  (add-hook 'buffer-list-update-hook #'+setup-lsp-bridge-doc-buffer)

  :hook
  (+late . lsp-bridge-xref-override-mode)
  (prog-mode . lsp-bridge-mode)
  (conf-mode . lsp-bridge-mode)
  (text-mode . lsp-bridge-mode)
  (lsp-bridge-mode . flymake-bridge-setup))

(use-package flymake-bridge
  :after flymake
  :ensure (:host github :repo "eki3z/flymake-bridge" :main nil))

(defun +has-lsp ()
  "Return whether the current buffer has LSP server."
  (and (bound-and-true-p lsp-bridge-mode)
       (lsp-bridge-has-lsp-server-p)))

(defun +show-documentation ()
  "Show documentation at point."
  (interactive)
  (if (+has-lsp)
      (call-interactively #'lsp-bridge-show-documentation)
    (call-interactively #'helpful-at-point)))

(defun +show-symbols ()
  "Show symbols in buffer."
  (interactive)
  (if (+has-lsp)
      (call-interactively #'lsp-bridge-imenu)
    (call-interactively #'consult-imenu)))

(map! spc
  "c a" '("action"        . lsp-bridge-code-action)
  "c r" '("rename symbol" . lsp-bridge-rename)
  "c s" '("symbol list"   . consult-imenu)
  "t l" '("lsp"           . lsp-bridge-mode)
  "t L" '("restart lsp"   . lsp-bridge-restart-process))

(map! normal
  "K" #'+show-documentation)

(map! nil
  :keymaps 'acm-mode-map
  "M-n" #'acm-select-next-page
  "M-p" #'acm-select-prev-page)

;;; lsp.el ends here.
