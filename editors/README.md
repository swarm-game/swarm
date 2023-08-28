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

To configure YAML editor tabs for schema validation, install the [YAML plugin](https://marketplace.visualstudio.com/items?itemName=redhat.vscode-yaml) and add the following to `.vscode/settings.json` under the workspace root:

```json
{
    "yaml.schemas": {
        "data/schema/scenario.json": [
            "data/scenarios/**/*.yaml"
        ],
        "data/schema/entities.json": [
            "data/entities.yaml"
        ],
        "data/schema/recipes.json": [
            "data/recipes.yaml"
        ],
    }
}
```

## Vim and Neovim

Currently there is neither highlighting nor LSP support for Vim,
but we would be happy to [accept a contribution](../CONTRIBUTING.md).