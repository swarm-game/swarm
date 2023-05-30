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
             ;; Generate the current keywords with:
             ;; cabal run swarm:swarm -- generate editors --emacs
             (x-keywords '("def" "end" "let" "in" "require"))
             (x-builtins '(
               "self"
               "parent"
               "base"
               "if"
               "inl"
               "inr"
               "case"
               "fst"
               "snd"
               "force"
               "undefined"
               "fail"
               "not"
               "format"
               "chars"
               "split"
               "charat"
               "tochar"
               "key"
             ))
             (x-commands '(
               "noop"
               "wait"
               "selfdestruct"
               "move"
               "push"
               "stride"
               "turn"
               "grab"
               "harvest"
               "place"
               "give"
               "equip"
               "unequip"
               "make"
               "has"
               "equipped"
               "count"
               "act"
               "drill"
               "use"
               "build"
               "salvage"
               "reprogram"
               "say"
               "listen"
               "log"
               "view"
               "appear"
               "create"
               "halt"
               "time"
               "scout"
               "whereami"
               "detect"
               "resonate"
               "sniff"
               "chirp"
               "watch"
               "surveil"
               "heading"
               "blocked"
               "scan"
               "upload"
               "ishere"
               "isempty"
               "meet"
               "meetall"
               "whoami"
               "setname"
               "random"
               "run"
               "return"
               "try"
               "swap"
               "atomic"
               "instant"
               "installkeyhandler"
               "teleport"
               "as"
               "robotnamed"
               "robotnumbered"
               "knows"
               "east"
               "north"
               "west"
               "south"
               "left"
               "right"
               "back"
               "forward"
               "down"
             ))
             (x-types '("int" "text" "dir" "bool" "cmd" "void" "unit" "actor"))

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
