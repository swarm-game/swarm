Swarm
=====

[![Build Status][build-status]][actions]
[![GitHub release (latest by date)][release-img]][releases]
[![Swarm release on Hackage][hackage-img]][hackage]
[![Contributor Covenant][badge-covenant]](CODE_OF_CONDUCT.md)
[![ircchat][badge-ircchat]][ircchat]
![GitHub Contributors][contribs]

[build-status]: https://github.com/swarm-game/swarm/actions/workflows/haskell-ci.yml/badge.svg
[release-img]: https://img.shields.io/github/v/release/swarm-game/swarm?logo=github
[releases]: https://github.com/swarm-game/swarm/releases
[hackage-img]: https://img.shields.io/hackage/v/swarm.svg?logo=haskell
[hackage]: https://hackage.haskell.org/package/swarm
[actions]: https://github.com/swarm-game/swarm/actions
[badge-covenant]: https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg
[badge-ircchat]: https://img.shields.io/badge/chat-on%20libera-brightgreen.svg
[ircchat]: https://web.libera.chat/?channels=#swarm
[contribs]: https://contrib.rocks/image?repo=swarm-game/swarm

Swarm is a 2D programming and resource gathering game. Program your
robots to explore the world and collect resources, which in turn
allows you to build upgraded robots that can run more interesting and
complex programs.  More info can be found on the [Swarm
website](https://swarm-game.github.io).

[![World 0 after scanning a tree and making a log.](images/tutorial/log.png)](https://swarm-game.github.io)

Contributing
------------

See [CONTRIBUTING.md](CONTRIBUTING.md) for information about various
ways you can contribute to Swarm development!

Building
--------

If you just want to play the game, [head over to the Swarm website for
installation instructions](https://swarm-game.github.io/installing/).
If you want to build Swarm from source (*e.g.* in order to
[contribute](CONTRIBUTING.md), or to test out the latest bleeding-edge
unreleased features), read on.

1. Clone the Swarm repository, e.g.

       git clone https://github.com/swarm-game/swarm.git

1. If you don't already have the `cabal` tool:
    1. Get the [`ghcup` tool](https://www.haskell.org/ghcup/), a handy
       one-stop utility for managing all the different pieces of a
       Haskell toolchain.
    1. Use `ghcup` to install a supported version of GHC:

           ghcup install ghc 9.8.2 --set
    1. Use `ghcup` to install `cabal`:

           ghcup install cabal

1. Now use `cabal` to build and run Swarm:

       cd /path/to/the/swarm/repo
       cabal run -O0 swarm:exe:swarm

   (Note that we recommend turning off optimizations with `-O0` since
   they don't seem to make much difference to the speed of the
   resulting executable, but they make a big difference in compilation
   time.)

1. Go get a snack while `cabal` downloads and builds all of Swarm's
   dependencies.

1. You might also want to check out the `scripts` directory, which
   contains an assortment of useful scripts for developers.
