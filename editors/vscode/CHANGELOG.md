# Change Log

All notable changes to the "swarm-language" extension will be documented in this file.

## WIP
- [Highlighter] added `infinity` unary function

## version 0.0.7
- [Highlighter] added `swap` command
- [Highlighter] added `split` and `chars` functions

## version 0.0.6
- [Highlighter] **FIX:** the number pattern now matches properly, previously it broke everything!
- [Highlighter] added `listen` command
- added a snapshot test

## version 0.0.5
- [Highlighter] added the `atomic`, `installed` and `time` constants
- [Highlighter] added `require` syntax (checking what follows it is left to typechecker)
- [Highlighter] changed the syntax for numbers to allow octal, binary and hexadecimal
- hopefully fixed the image link, so that it can render in editor and on the extension webpage

## version 0.0.4
- [Highlighter] Automatically generated the list of keywords (many new ones and few renamed)

## version 0.0.3

- [Highlighter] Update reserved word list (include `drill`, `has`, etc.)
- [Highlighter] Recognize sum types (`a + b`)
- [Highlighter] Recognize explicit `forall a b.`
- moved the package to `swarm-game/swarm` repository
- added a logo (featuring base and four robots)

## version 0.0.2

- Add LSP integration (requires `swarm` executable).

## version 0.0.1

- Initial release with support for highlighting
