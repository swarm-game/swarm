# Revision history for swarm

## **0.1.0.1** - 2022-10-06

A bugfix release for a few minor bugs that plagued the first release:

- Fall back to to the swarm data directory when a `run` file is not
  found ([#730](https://github.com/swarm-game/swarm/pull/730))
  - This bug caused the move tutorial to be unplayable because it tried to execute
    `run "data/scenarios/Tutorials/move_system.sw"`
- Fix version check when there is no GitInfo
  ([#729](https://github.com/swarm-game/swarm/pull/729))
  - This bug caused the game to always report that there was a new
    version available even when you already had the latest (and only!) version.

## **0.1.0.0** - 2022-10-06

First Swarm release! Swarm already has:

- a programming language based on the polymorphic
  lambda calculus + recursion, with a command monad for describing
  first-class imperative actions
- scenarios which can be loaded from YAML files
  - the release comes with official challenges and an in-game tutorial
  - the default Classic and Creative modes use the same YAML syntax
  - we include JSON schemas for editor support when writing scenarios 
- procedural 2D world generation
- LSP server built into the Swarm executable
- Terminal UI interface
  - running the executable opens the Main menu by default
  - game screen with a world view, inventory and REPL
    - popup windows for messages, challenge descriptions, etc.
