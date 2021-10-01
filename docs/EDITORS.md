# Text Editor Configuration

Validate a swarm-lang file using: `swarm format ./file.sw`

Swarm comes with a language server protocol (LSP) server.
Make sure the `swarm` program is present in your PATH, then
configure your editor:

* [Emacs](#EMACS)

The current LSP implementation features the following extensions:

* error diagnostic on load/save

## EMACS

Using [lsp-mode](https://github.com/emacs-lsp/lsp-mode):

Load the [swarm-mode.el](../contribs/swarm-mode.el) and start
the `M-x lsp` service in a *swarm-mode* buffer.
