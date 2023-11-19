# Change Log

All notable changes to the "swarm-language" extension will be documented in this file.

## version 0.1.1
- Move the extension under "swarm-game" org

## version 0.0.9
- [Highlighter] add `unit` and `actor` types
- [Highlighter] add many new commands:
  - backup, charat, chirp, density, detect, equip, equipped, halt, heading,
    installkeyhandler, instant, isempty, key, meet, meetall, push, resonate,
    scout, sniff, stride, surveil, tochar, unequip, use, watch, waypoint
- [Highlighter] remove `install` and `installed` commands

## version 0.0.8
- [Highlighter] update regex to recognize `void` and `text` types
- [Highlighter] improve the coloring of types and lambda parameters

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
