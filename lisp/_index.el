;;; _index.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst +deirnmacs-lisp-dir (expand-file-name "lisp" +deirnmacs-directory))

(load! (package-lock
        elpaca-init
        package-utils
        patch
        optimizations
        keybind-setup
        behaviour
        minibuffer
        ui
        window
        popups
        buffer
        dashboard
        directory
        git
        terminal
        discord
        undo
        evil
        completions
        lsp
        lang
        edit
        debug
        formatting
        trust))

;;; _index.el ends here.
