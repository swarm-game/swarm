# Revision history for swarm

## **0.2.0.0** - 2022-11-01

A bunch of small fixes and improvements; special thanks to new
contributors @0xcefaedfe, @kostmo, @ussgarci, and @valyagolev. Notable changes include:

- New UI features:
    - REPL improvements:
        - Expose the last evaluated result as `it`, and previous results
          as `itN` ([#734](https://github.com/swarm-game/swarm/pull/734))
        - Allow clicking in the REPL input to move the cursor
          ([#750](https://github.com/swarm-game/swarm/pull/750))
        - Autocomplete entity names in the repl ([#798](https://github.com/swarm-game/swarm/pull/798))
        - REPL cursor no longer blinks when REPL panel is not selected ([#801](https://github.com/swarm-game/swarm/pull/801))
    - Improve user experience around quitting & moving between
      tutorial challenges ([#754](https://github.com/swarm-game/swarm/pull/754))
        - Add a button to the Quit dialog to restart a
          scenario. ([#767](https://github.com/swarm-game/swarm/pull/767))
        - Use scenario name as Goal dialog title ([#774](https://github.com/swarm-game/swarm/pull/774))
    - `autoplay` flag for automatically demonstrating scenario
      solutions ([#792](https://github.com/swarm-game/swarm/pull/792))
    - Improved inventory sorting and user-controllable sort criteria ([#793](https://github.com/swarm-game/swarm/pull/793))
    - Ability to temporarily hide robots so you can see what's under
      them ([#802](https://github.com/swarm-game/swarm/pull/802))
- New language features:
    - New `void` type ([#735](https://github.com/swarm-game/swarm/pull/735))
- Bug fixes:
    - Fix bug in the first tutorial challenge that froze the game and
      ate all memory if the user said anything other than expected
      ([#762](https://github.com/swarm-game/swarm/pull/762), [#810](https://github.com/swarm-game/swarm/pull/810))
- Documentation:
    - Generate all wiki "cheat sheets" automatically ([#769](https://github.com/swarm-game/swarm/pull/769))
- Support for building on GHC 9.4 ([#752](https://github.com/swarm-game/swarm/pull/752))

There were several other small fixes and improvements; see the [full
changelog
here](https://github.com/swarm-game/swarm/compare/0.1.1.0...0.2.0.0).

## **0.1.1.0** - 2022-10-14

A couple new features and an important bugfix for the Hackage release.

- Update to `hsnoise-0.0.3`, fixing some world generation bugs that
  only showed up in the Hackage
  release. ([#746](https://github.com/swarm-game/swarm/pull/746))
- New "blank" creative scenario
  ([#741](https://github.com/swarm-game/swarm/pull/741))
- REPL improvements
    - `Ctrl-D` at an empty REPL prompt now triggers a quit
      ([#743](https://github.com/swarm-game/swarm/pull/743))
    - The REPL panel now persists in showing the type of the most
      recently evaluated expression ([#733](https://github.com/swarm-game/swarm/pull/733))

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
