;;; swarm-mode.el --- Swarm Lang mode -*- lexical-binding: t -*-
;;
;; Author: swarm contributors
;; URL: https://github.com/byorgey/swarm
;; Version: 0.1
;; Package-Requires: ((emacs-lsp))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'lsp-mode)

(define-derived-mode swarm-mode prog-mode "Swarm Lang Mode"
  (font-lock-fontify-buffer))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(swarm-mode . "swarm"))

  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection (list "swarm" "--lsp"))
                     :activation-fn (lsp-activate-on "swarm")
                     :server-id 'swarm-check)))
;;; swarm-mode.el ends here
