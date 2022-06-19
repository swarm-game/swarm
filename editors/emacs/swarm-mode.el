;;; swarm-mode.el --- Swarm Lang mode -*- lexical-binding: t -*-
;;
;; Author: swarm contributors
;; URL: https://github.com/swarm-game/swarm
;; Version: 0.1
;; Package-Requires: ((emacs-lsp))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'lsp-mode)

(defvar swarm-mode-syntax-table nil "Syntax table for `swarm-mode'.")

(setq swarm-mode-syntax-table
      (let ( (synTable (make-syntax-table)))

        ;; C++ style comments ("// ..." and "/* ... */")
        (modify-syntax-entry ?\/ ". 124" synTable)
        (modify-syntax-entry ?* ". 23b" synTable)
        (modify-syntax-entry ?\n "> " synTable)

        synTable))

(setq swarm-font-lock-keywords
      (let* (
             ;; We should figure out how to autogenerate these, so we don't have
             ;; to edit the emacs mode every time we add new commands.
             (x-keywords '("def" "end"))
             (x-builtins '("if" "run" "return" "try" "raise" "force" "fst" "snd"))
             (x-commands
              '("noop" "wait" "selfdestruct" "move" "turn" "grab" "place" "give"
                "install" "make" "build" "salvage" "reprogram" "knows"
                "say" "log" "view" "appear" "create" "getx" "gety"
                "blocked" "scan" "upload" "ishere" "whoami"
                "random" "not"
                "left" "right" "back" "forward" "north" "south" "east" "west" "down"
                ))
             (x-types '("int" "string" "dir" "bool" "cmd"))

             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-builtins-regexp (regexp-opt x-builtins 'words))
             (x-commands-regexp (regexp-opt x-commands 'words))
             (x-types-regexp (regexp-opt x-types 'words)))

        `(
          (,x-keywords-regexp . 'font-lock-keyword-face)
          (,x-builtins-regexp . 'font-lock-builtin-face)
          (,x-types-regexp . 'font-lock-type-face)
          (,x-commands-regexp . 'font-lock-constant-face)
          ("[a-z_][a-z0-9_]*" . 'font-lock-function-name-face)
          )))

(define-derived-mode swarm-mode prog-mode "Swarm Lang Mode"
  (setq font-lock-defaults '((swarm-font-lock-keywords) nil t nil))
  (set-syntax-table swarm-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.sw\\'" . swarm-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(swarm-mode . "swarm"))

  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection (list "swarm" "lsp"))
                     :activation-fn (lsp-activate-on "swarm")
                     :server-id 'swarm-check)))

(provide 'swarm-mode)
;;; swarm-mode.el ends here
