# Text Editor Configuration

Validate a swarm-lang file using: `swarm format ./file.sw`

Swarm comes with a language server protocol (LSP) server.
Make sure the `swarm` program is present in your PATH, then
configure your editor.

## EMACS

The current LSP implementation features the following extensions:

* error diagnostic on load/save

Using [lsp-mode](https://github.com/emacs-lsp/lsp-mode):

Load the [swarm-mode.el](../contribs/swarm-mode.el) and start
the `M-x lsp` service in a *swarm-mode* buffer.

## VS Code and VS Codium

The [swarm-lang](./vscode) extension provides highlighting and an
LSP client. That is if you have `swarm` executable in PATH, then
the executable will be used as LSP server to show errors as you type.

You can get it by:
- installing from MS marketplace ([link](https://marketplace.visualstudio.com/items?itemName=xsebek.swarm-language))
- building from source in the [vscode folder](./vscode/DEVELOPING.md)
- **TBD** get the VSIX from GitHub releases
- **TBD** installing from the VS codium free marketplace

## Vim and Neovim

Currently there is neither highlighting nor LSP support for Vim,
but we would be happy to [accept a contribution](../CONTRIBUTING.md).