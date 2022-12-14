## Developing

> The extension was created using [`yo code`](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#developing-a-new-grammar-extension)
> and further extended to support LSP by including modified
> [Microsoft lsp-sample](https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample).
>
> As a note, this is deliberately a separate file from `README.md` which shows in the VSCode extension view.

### What's in the folder

* `package.json` - this is the manifest file in which you declare your language support and define the location of the grammar file that has been copied into your extension.
* `syntaxes/swarm.tmLanguage.json` - this is the Text mate grammar file that is used for tokenization.
  * here is an overview of the available scopes: [SO answer](https://stackoverflow.com/a/21914803/11105559)
* `language-configuration.json` - this is the language configuration, defining the tokens that are used for comments and brackets.
* `client/src/extension.ts` - this is the LSP client, that will connect to `swarm` executable which can serve as LSP server.

### Get up and running straight away

* Make sure the language configuration settings in `language-configuration.json` are accurate.
* Press `F5` to open a new window with your extension loaded.
* Create a new file with a file name suffix matching your language.
* Verify that syntax highlighting works and that the language configuration settings are working.

### Make changes

* You can relaunch the extension from the debug toolbar after making changes to the files listed above.
* You can also reload (`Ctrl+R` or `Cmd+R` on Mac) the VS Code window with your extension to load your changes.

### Updating the syntax highlighting

Whenever swarm language adds new features, the highlighing needs to be updated.

To save some time, get the current reserved words by running `swarm generate`:
```bash
cabal run swarm:swarm -- generate editors
```

You still have to add for example types manually.


### Add more language features

To add features such as intellisense, hovers and validators check out the VS Code extenders documentation
at [VSCode docs](https://code.visualstudio.com/docs).

## Install your extension

First install the dependencies:
```bash
npm update
npm install
```

If you want to include the LSP client then after the previous install do:
```sh
cd client
tsc --build
```

To build the VSIX package do:
```sh
vsce package --baseImagesUrl "https://raw.githubusercontent.com/swarm-game/swarm/editors/vscode"
```

To share this extension with the world, read on https://code.visualstudio.com/docs about publishing an extension or ask @xsebek to do it.
