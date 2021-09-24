---
title: "Swarm: preview and call for collaboration"
---

    [BLOpts]
    profile = wp
    postid = 2411
    publish = true
    tags = Swarm, game, robot, programming, resource
    categories = Haskell, projects

For about a month now I have been working on building a game^[Can you
tell I am on sabbatical?], tentatively titled
[Swarm](https://github.com/byorgey/swarm).  It's nowhere near
finished, but it has at least reached a point where I'm not
embarrassed to show it off.  I would love to hear feedback, and I
would especially love to have others contribute!  Read on for more
details.

[![](../../images/log.png)](https://github.com/byorgey/swarm)

Swarm is a 2D tile-based resource gathering game, but with a twist:
the only way you can interact with the world is by building and
programming robots.  And there's another twist: the kinds of commands
your robots can execute, and the kinds of programming language
features they can interpret, depends on what devices they have
installed; and you can create new devices only by gathering resources.
So you start out with only very basic capabilities and have to
bootstrap your way into more sophisticated forms of exploration and
resource collection.

I guess you could say it's kind of like a cross between Minecraft,
Factorio, and [Karel the
Robot](https://en.wikipedia.org/wiki/Karel_(programming_language)),
but with a much cooler programming language (lambda calculus +
polymorphism + recursion + exceptions + a command monad for
first-class imperative programs + a bunch of other stuff).

The game is far from complete, and especially needs a lot more depth
in terms of the kinds of devices and levels of abstraction you can
build.  But for me at least, it has already crossed the line into
something that is actually somewhat fun to play.

If it sounds interesting to you, give it a spin!  Take a look at the
[README](https://github.com/byorgey/swarm/blob/main/README.md) and the
[tutorial](https://github.com/byorgey/swarm/blob/main/TUTORIAL.md).
If you're interested in contributing to development, check out the
[CONTRIBUTING](https://github.com/byorgey/swarm/blob/main/CONTRIBUTING.md)
file and the [GitHub issue
tracker](https://github.com/byorgey/swarm/issues), which I have
populated with a plethora of tasks of varying difficulty.  This could
be a great project to contribute to especially if you're relatively
new to Haskell; I try to keep everything well-organized and
well-commented, and am happy to help guide new contributors.
