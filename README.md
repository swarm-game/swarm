Swarm
=====

[![Build Status](https://github.com/byorgey/swarm/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/byorgey/swarm/actions)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)
![GitHub Contributors](https://contrib.rocks/image?repo=byorgey/swarm)
--
Other langs: [ru](other_langs/README_RU.md)
--
Swarm is a 2D programming and resource gathering game. Program your
robots to explore the world and collect resources, which in turn
allows you to build upgraded robots that can run more interesting and
complex programs.

![World 0 after scanning a tree and making a log.](images/tutorial/log.png)

The implementation is still in an early stage, but these are some of the (planned) features:

* Practically infinite 2D procedurally generated worlds
* Simple yet powerful programming language based on the polymorphic
  lambda calculus + recursion, with a command monad for describing
  first-class imperative actions
* Editor support with LSP and highlighting
* (**TBD**) In-game tutorial
* Multiple game modes:
    - In Classic mode, you start with the ability to produce only very
      basic, limited robots; collecting resources allows you to
      bootstrap your way into programming more sophisticated robots
      that can explore more of the world, collect more resources, etc.
    - Creatrive mode places no restrictions: program robots to your
      heart's content using whatever language features you want,
      without worrying about collecting resources.
    - (**TBD**) Hardcore mode like Classic mode, but you start
      with only a limited number of robots.  If they get stuck or
      you run out of resources, it's game over!
    - (**TBD**) In Challenge mode, where you attempt to program robots
      in order to solve pre-designed puzzles or challenges.
    - (**TBD**) Future versions might also have multiplayer modes,
      with co-op or PvP play over a network...?

Installing and Playing
======================

Warning: Swarm is still in an early stage; it's missing many of the
planned features, there is no winning condition, and the gameplay is
not very deep yet.  But you can program cute little robots, and hey,
that's all that really matters, right?

**NOTE**: Swarm requires a POSIX-style environment that supports
`terminfo`.  On Linux and OSX, the below instructions should work
natively.  On Windows, see [the comments on this GitHub
issue](https://github.com/byorgey/swarm/issues/53) for instructions
getting it to work under the Windows Subsystem for Linux.

The recommended way to install Swarm at the moment is as follows:

1. Clone the Swarm repository, e.g.

       git clone https://github.com/byorgey/swarm.git

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
1. Have fun! At the moment, you probably want to [take a look at the
   tutorial](TUTORIAL.md) to help get you started.  Eventually there
   will be an in-game tutorial.


Programming swarm
=================

Your base has a dictionary to store definitions, like this one:

```
def moveUntil : cmd bool -> cmd () = \predicate.
  res <- predicate;
  if res {
    noop
  } {
    moveUntil predicate
  }
end
```

<sup>The indentation is not required but `;` is, as it is similar
to Haskell `>>` - that is the command monad, which imperative
programmers can ignore. :wink:
</sup>

This allows you to program robots to perform complicated tasks.

While you can write commands and definitions like the one above
in the REPL, swarm also has a editor support with highlighting
and LSP integration:

![Editor with problem popup](images/editor.png)

See the `editors` folder for details on how to configure your editor.

Community
=========

Check out the [COMMUNITY](COMMUNITY.md) page for ways to connect with
others in the community.

If you want to contribute, you're most welcome!  There are *lots* of
ways to contribute, regardless of your Haskell background.  For
example, even someone with no Haskell experience whatsoever could
still help with *e.g.* game design, playtesting, and level design for
challenge mode.  Check out the [CONTRIBUTING](CONTRIBUTING.md) file
for more specific information about how to contribute.
