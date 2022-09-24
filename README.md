Swarm
=====

[![Build Status](https://github.com/swarm-game/swarm/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/swarm-game/swarm/actions)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)
![GitHub Contributors](https://contrib.rocks/image?repo=swarm-game/swarm)

Swarm is a 2D programming and resource gathering game. Program your
robots to explore the world and collect resources, which in turn
allows you to build upgraded robots that can run more interesting and
complex programs.  Check out the [installation
instructions](https://github.com/swarm-game/swarm#installing) below,
join the [IRC channel](COMMUNITY.md), take a look at the
[wiki](https://github.com/swarm-game/swarm/wiki), or [see how you can
contribute](CONTRIBUTING.md)!

![World 0 after scanning a tree and making a log.](images/tutorial/log.png)

Features include:

* Practically infinite 2D procedurally generated worlds
* Simple yet powerful programming language based on the polymorphic
  lambda calculus + recursion, with a command monad for describing
  first-class imperative actions
* Editor support with LSP and highlighting
* In-game tutorial
* Multiple game modes:
    - In Classic mode, you start with the ability to produce only very
      basic, limited robots; collecting resources allows you to
      bootstrap your way into programming more sophisticated robots
      that can explore more of the world, collect more resources, etc.
    - Creative mode places no restrictions: program robots to your
      heart's content using whatever language features you want,
      without worrying about collecting resources.
    - There are also challenge scenarios where you attempt to program
      robots in order to solve pre-designed puzzles or challenges.

Installing
==========

There will _soon_ be a release of Swarm on Hackage and in a binary form!
If you are interested in Haskell or functional programming, you can install
it from source (see below) or, once it is released, with the Cabal tool.
For instructions on how to install the binary directly, check the releases.

Installing from Source
----------------------

**NOTE**: Swarm requires a POSIX-style environment that supports
`terminfo`.  On Linux and OSX, the below instructions should work
natively.  On Windows, see [the comments on this GitHub
issue](https://github.com/swarm-game/swarm/issues/53) for instructions
getting it to work under the Windows Subsystem for Linux.

To install Swarm from source:

1. Clone the Swarm repository, e.g.

       git clone https://github.com/swarm-game/swarm.git

1. If you don't already have the `stack` tool:
    1. Get the [`ghcup` tool](https://www.haskell.org/ghcup/), a handy
       one-stop utility for managing all the different pieces of a
       Haskell toolchain.
    1. Use `ghcup` to install `stack`:

           ghcup install stack

1. It is recommended that you use a relatively large terminal window (*e.g.*
   132 columns x 43 rows or larger).
    * To find out the size of your terminal, you can type `stty size`.

1. Now use `stack` to build and run Swarm:

       cd /path/to/the/swarm/repo
       stack run

1. Go get a snack while `stack` downloads a Haskell compiler and
   all of Swarm's dependencies.
1. Have fun!


Configuring your editor
=======================

Although you can write commands and definitions directly in the Swarm
REPL, once you get beyond the basics you'll probably want to use an
external editor for writing Swarm programs.  Swarm has support for
external editors with highlighting and LSP integration:

![Editor with problem popup](images/editor.png)

See the `editors` folder for details on how to configure your editor.
Currently, Emacs and VS Code are officially supported, but more can be
added.

Community
=========

Check out the [COMMUNITY](COMMUNITY.md) page for ways to connect with
others in the community.

If you want to contribute, you're most welcome!  There are *lots* of
ways to contribute, regardless of your Haskell background.  For
example, even someone with no Haskell experience whatsoever could
still help with *e.g.* game design, playtesting, and creating
challenges and scenarios.  Check out the [CONTRIBUTING](CONTRIBUTING.md)
file for more specific information about how to contribute.
