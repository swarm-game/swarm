# Text Editor Configuration

Validate a swarm-lang file using: `swarm format ./file.sw`

Swarm comes with a language server protocol (LSP) server.
Make sure the `swarm` program is present in your PATH, then
configure your editor.

## EMACS

The current LSP implementation features the following extensions:

* error diagnostic on load/save

Using [lsp-mode](https://github.com/emacs-lsp/lsp-mode):

Load the [swarm-mode.el](emacs/swarm-mode.el) and start
the `M-x lsp` service in a *swarm-mode* buffer.

## VS Code and VS Codium

The [swarm-lang](./vscode) extension provides highlighting and an
LSP client. That is if you have `swarm` executable in PATH, then
the executable will be used as LSP server to show errors as you type.

You can get it by:
- installing from the MS marketplace ([link](https://marketplace.visualstudio.com/items?itemName=swarm-game.swarm-language))
- installing from the Open VSX Registry ([link](https://open-vsx.org/extension/swarm-game/swarm-language))
- building from source in the [vscode folder](./vscode/DEVELOPING.md)

### YAML schema validation

To configure YAML editor tabs for schema validation, install the [YAML plugin](https://marketplace.visualstudio.com/items?itemName=redhat.vscode-yaml).  The appropriate settings are already included in `.vscode/settings.json` under the workspace root.

## Vim and Neovim

Add the following lines to your Vim/Neovim configuration file for files with the `.sw` extension to be recognized as `swarm` programs:


`init.vim`:

`au BufRead,BufNewFile *.sw setfiletype swarm`


`init.lua`:

`vim.cmd[[au BufRead,BufNewFile *.sw setfiletype swarm]]`


Basic syntax highlighting is available for both Vim and Neovim. To make use of this capability, copy [swarm.vim](vim/swarm.vim) to the `syntax` directory in your Vim or Neovim configuration directory.


An LSP configuration leveraging Neovim's native LSP client is also available. It only works with Neovim. To enable it, copy [swarm.lua](vim/swarm.lua) to `after/ftplugin` in your Neovim configuration directory.
