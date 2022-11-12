---
title: "Swarm alpha release!"
---

    [BLOpts]
    profile = wp
    postid = 2474
    publish = true
    tags = Swarm, game, robot, programming, resource
    categories = Haskell, projects

The [Swarm](https://github.com/swarm-game/swarm/) development team is
very proud to announce the very first alpha release of the game.
There are still many missing features (for example, [saving games is
not yet possible](https://github.com/swarm-game/swarm/issues/50)) and
[known
bugs](https://github.com/swarm-game/swarm/issues?q=is%3Aissue+is%3Aopen+label%3ABug+),
but at this point it's quite playable (and, dare we say, fun!) and
ready for some intrepid souls to try it out and give us some feedback.

![](../../images/tutorial/log.png)

![](../../images/tree_harvest.png)

What is it?
-----------

Swarm is a 2D, open-world programming and resource gathering game with
a strongly-typed, functional programming language and a unique upgrade
system. Unlocking language features is tied to collecting resources,
making it an interesting challenge to bootstrap your way into the use
of the full language.

Notable changes since the [last progress
update](https://byorgey.wordpress.com/2022/06/20/swarm-status-report/)
include:

- An all-new in-game tutorial consisting of a sequence of guided challenges
- Several new challenge scenarios (mazes! towers of hanoi!), and
  documentation on how to make your own
- Lots more in-game help and info, including help on currently available
  commands + recipes, and a dialog showing all live robots
- Many more entities, recipes, and language features to explore and collect
- Better mouse support
- Backwards incremental search and tab completion in the REPL
- Many, many small bug fixes and improvements!

Give it a try!
--------------

To install, check out the [installation instructions][install]: you
can download a [binary release][release] (for now, Linux only, but
MacOS binaries should be on the horizon), or [install from
Hackage][hackage].  Give it a try and send us your feedback, either
[via a github issue][issue] or [via IRC][irc]!

[install]: https://github.com/swarm-game/swarm#installing
[release]: https://github.com/swarm-game/swarm/releases
[hackage]: https://hackage.haskell.org/package/swarm
[issue]: https://github.com/swarm-game/swarm/issues/new/choose

Future plans & getting involved
-------------------------------

We're still hard at work on the game, and will next turn our attention
to some big features, such as:

- [Saving and loading games][saving]
- New world features like aliens and [cities][cities]
- New language features like [recursive types][rectypes],
  [arrays][arrays], [inter-robot communication][robot-comm], and [a
  proper `import` construct][import]

[cities]: https://github.com/swarm-game/swarm/issues/112
[saving]: https://github.com/swarm-game/swarm/issues/50
[rectypes]: https://github.com/swarm-game/swarm/issues/154
[arrays]: https://github.com/swarm-game/swarm/issues/98
[robot-comm]: https://github.com/swarm-game/swarm/issues/94
[import]: https://github.com/swarm-game/swarm/issues/495

Of course, there are also [tons of small things that need fixing and
polishing][low-hanging] too!  If you're interested in getting
involved, check out our [contribution guide][contrib], come [join us
on IRC][irc] (`#swarm` on Libera.Chat), or take a look at the list of
[issues marked "low-hanging fruit"][low-hanging].

[contrib]: https://github.com/swarm-game/swarm/blob/main/CONTRIBUTING.md
[low-hanging]: https://github.com/swarm-game/swarm/issues?q=is%3Aissue+is%3Aopen+label%3A%22C-Low+Hanging+Fruit%22
[irc]: https://web.libera.chat/?channels=#swarm

Brought to you by the Swarm development team:

- Brent Yorgey
- Ondřej Šebek
- Tristan de Cacqueray

With contributions from:

- Alexander Block
- Daniel Díaz Carrete
- Huw Campbell
- Ishan Bhanuka
- Jacob
- Jens Petersen
- José Rafael Vieira
- Joshua Price
- lsmor
- Noah Yorgey
- Norbert Dzikowski
- Paul Brauner
- Ryan Yates
- Sam Tay

...not to mention many others who gave valuable suggestions and
feedback.  Want to see your name listed here in the next release?
[See how you can
contribute!](https://github.com/swarm-game/swarm/blob/main/CONTRIBUTING.md)
