# Welcome to your VS Code Extension

## What's in the folder

* This folder contains all of the files necessary for your extension.
* `package.json` - this is the manifest file in which you declare your language support and define the location of the grammar file that has been copied into your extension.
* `syntaxes/swarm.tmLanguage.json` - this is the Text mate grammar file that is used for tokenization.
* `language-configuration.json` - this is the language configuration, defining the tokens that are used for comments and brackets.
* `client/src/extension.ts` - this is the LSP client, that will connect to `swarm` executable which can serve as LSP server.

## Get up and running straight away

* Make sure the language configuration settings in `language-configuration.json` are accurate.
* Press `F5` to open a new window with your extension loaded.
* Create a new file with a file name suffix matching your language.
* Verify that syntax highlighting works and that the language configuration settings are working.

## Make changes

* You can relaunch the extension from the debug toolbar after making changes to the files listed above.
* You can also reload (`Ctrl+R` or `Cmd+R` on Mac) the VS Code window with your extension to load your changes.

### Updating the syntax highlighting

Whenever swarm language adds new features, the highlighing needs to be updated.

To save some time, get the current reserved words by running `cabal repl`:
```haskell
import Swarm.Language.Syntax 
import qualified Data.Text as T
:set -XOverloadedStrings

-- get basic functions/commands
T.intercalate  "|" $ map (syntax . constInfo) (filter isUserFunc allConst)

-- get list of directions
T.intercalate "|" $  map (dirSyntax . dirInfo) allDirs
```

You still have to add for example types manually.


### Add more language features

* To add features such as intellisense, hovers and validators check out the VS Code extenders documentation at https://code.visualstudio.com/docs

## Install your extension

If you want to include the LSP client, first do:
```sh
cd client
npm update
tsc --build
```

To build the VSIX package do:
```sh
vsce package --baseImagesUrl "https://raw.githubusercontent.com/byorgey/swarm/editors/vscode"
```

To share this extension with the world, read on https://code.visualstudio.com/docs about publishing an extension or ask @xsebek to do it.
