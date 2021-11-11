# swarm-language README

This VSCode extension provides a basic highlighting and LSP client for the swarm programming language.

![VSCode screenshot](images/editor_debug.png)

## Extension Settings

There are no customizations yet, sorry.

## Known Issues

- The highlighter expects type and `=` sign on the same line as `def`/`let`, i.e.
  ```haskell
  def missionImpossible : cmd a -> a = ...
  ```
  This regexp limitation is unlikely to impact many users, but is still a departure from real swarm.
