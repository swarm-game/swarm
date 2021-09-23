# Contributing to Swarm

Thanks so much for taking the time to contribute to Swarm! All
contributions are welcomed and appreciated, whether reporting
a bug or fixing a single punctuation mark, contributing to
game design, or implementing a complex new language feature.

The following is a set of guidelines for contributing to Swarm, which
is hosted in the [swarm repository](https://github.com/byorgey/swarm)
on GitHub. These are mostly guidelines, not rules. In particular, you
don't need to worry that your contribution will be ignored or rejected
in some way if you don't follow the guidelines. Use your best
judgment, and also feel free to propose changes to this document in a
pull request.

## Table Of Contents

[Code of Conduct](#code-of-conduct)

[Quick Links](#quick-links)

[How can I contribute?](#how-can-i-contribute)

- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Contributing to Game Design](#contributing-to-game-design)
- [Making a Code Contribution](#making-a-code-contribution)
- [Development Workflow](#development-workflow)

[I have push access to the Swarm repository, now what?](#i-have-push-access-to-the-swarm-repository-now-what)

## Code of Conduct

The Swarm project and everyone participating in it is governed by the
[Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). By participating,
you are expected to uphold this code. Please report unacceptable behavior to
[byorgey@gmail.com](mailto:byorgey@gmail.com).

## Quick Links

- Bug to report or feature to request? Try the
  [GitHub issue tracker](https://github.com/byorgey/swarm/swarm/issues).
<!-- - Questions? Join the -->
<!--   [`#swarm-lang` IRC channel on `freenode.net`](https://webchat.freenode.net/#swarm-lang). -->

## How can I contribute?

There are lots of ways to contribute to Swarm. Regardless of your
background knowledge or level of programming skill, there is probably
a way you can help. For general advice on contributing to open source
projects like Swarm, see [How to Contribute to Open
Source](https://opensource.guide/how-to-contribute/), or [Your first
open source contribution: a step-by-step technical
guide](https://medium.com/@jenweber/your-first-open-source-contribution-a-step-by-step-technical-guide-d3aca55cc5a6).
The rest of this section walks through specific ways you can
contribute and instructions for how to do so.

### Reporting Bugs

Even if you're not at the point where you would feel comfortable
contributing code or documentation, simply playing the game and
reporting any problems that you find or suggestions you have along the
way is an immensely valuable way to help make the game better.

Feel free to [submit a bug
report](https://github.com/byorgey/swarm/issues) for anything that
seems like it's not the way it should be, such as a typo or
inconsistency in some in-game text, a robot program that yields an
error even though you think it should be accepted, or a robot program
that seems to behave incorrectly. Even if the error turns out to be in
your understanding rather than in the game, it's still a valuable
contribution, since it may point out a way that the documentation or
interface could be improved to help reduce similar confusion in the
future.

Bugs are tracked as [GitHub issues](https://github.com/byorgey/swarm/issues).
Before creating a new issue, you can
[do a quick search](https://github.com/byorgey/swarm/issues?q=is%3Aissue+is%3Aopen)
to see if the problem has already been reported. If it has and the issue is
still open, feel free add a comment to the existing issue instead of opening a
new one.

When creating a new issue, explain the problem and include additional details to
help the maintainers reproduce the problem, for example:

- **Use a clear and descriptive title** for the issue to identify the problem,
  if you can.
- **Provide specific examples to demonstrate the problem**. In many cases this
  may be in the form of some specific Swarm code which causes the problem to
  happen. To include some Swarm code in the issue, you can use
  [Markdown code blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
- **Describe the behavior you observed after following the steps** and point out
  what exactly is the problem with that behavior.
- **Explain which behavior you expected to see instead and why.**

Not all of these are necessarily required for every single bug report;
use your judgment, but try to err on the side of including more
information.

### Suggesting Enhancements

If you have an idea for a way Swarm could be better, or a super-cool new feature
you'd like to see, you can
[submit it on the GitHub issue tracker](https://github.com/byorgey/swarm/issues),
the same as a bug report. Just describe your idea in as much detail as you can.

### Contributing to Game Design

You are very much encouraged to help think about game design: how new
language features, entities, devices, and world features should be
added and changed, and how they all fit together to deepen the game
and create possibilities of gameplay.  This aspect of the game is very
much unfinished and will need a lot of work.  Take a look at the
[DESIGN](DESIGN.md) document in this repository for some overarching
thoughts and principles about the design of the game.

If you have a concrete idea that you think will work well, you can try
implementing it and opening a pull request.  However, if you have an
idea you are unsure about, or if you just want to help think through
some ideas but lack the skill, time, or desire to implement them, feel
free to just [open an
issue](https://github.com/byorgey/swarm/issues/new/choose) with a
description of your ideas to create a space for discussion.

### Making a Code Contribution

Swarm is written in [Haskell](http://haskell.org). The level of
Haskell skill needed to understand and contribute to the Swarm
codebase varies widely depending on which part of the code you look
at, but generally speaking you will probably need to be comfortable
with standard monads (`Except`, `Reader`, `State`, `Maybe`) and with
standard container types like `Data.Set` and `Data.Map`. Being
familiar with using `lens` will probably also help. If you'd like to
learn enough Haskell to contribute to Swarm, we recommend starting by
[working through these introductory Haskell course
materials](https://www.cis.upenn.edu/~cis194/spring13/).

If you'd like to contribute some code but are unsure where to begin, you can
start by looking through
[issues tagged "Low-Hanging Fruit"](https://github.com/byorgey/swarm/issues?q=is%3Aissue+is%3Aopen+label%3A%22C-Low+Hanging+Fruit%22)
in the issue tracker. These are bugs and features which should be appropriate
for someone just getting started to tackle. If you want help understanding or
getting started on a particular issue, feel free to leave a comment on the issue.
<!-- or ask in the -->
<!-- [`#swarm` IRC channel](https://webchat.freenode.net/#swarm). -->

### Development Workflow

Never made an open source contribution before? Wondering how making a
contribution actually works? Here's a quick rundown!

1. Find an issue that you are interested in addressing or a feature that you
   would like to add.
1. _Fork_ [the `swarm` repository](https://github.com/byorgey/swarm) (by
   clicking the "Fork" button in the upper-right corner). This will make a copy
   of the repository in your personal GitHub account, that is, you will have
   your own personal copy of the repository under
   `<your-GitHub-username>/swarm`.
1. _Clone_ the repository to your local machine by opening a terminal,
   navigating to the directory where you would like to store the `swarm`
   repository, and typing
   `git clone https://github.com/your-GitHub-username/swarm.git`. (If you don't
   already have the `git` tool, you will have to
   [install it first](https://git-scm.com/downloads).) You should now have a
   subfolder named `swarm` containing an up-to-date copy of the repository.
1. Create a new _branch_ for your fix using `git checkout -b BRANCH-NAME`
   (replace `BRANCH-NAME` by some appropriate name based on the feature or fix
   you plan to make).
1. Make the appropriate changes for the issue you are trying to address or the
   feature that you want to add.
1. Use `git add` to add the file contents of the changed files to the "snapshot"
   git uses to manage the state of the project, also known as the index.
1. Use `git commit -m "Insert a short message of the changes made here"` to
   store the contents of the index with a descriptive message.
1. Push the changes to your fork on GitHub using `git push origin BRANCH-NAME`.
1. [Submit a pull request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork)
   from your fork to the `byorgey/swarm` repository.
1. Title the pull request with a short description of the changes made and the
   issue or bug number associated with your change (if any). For example, you
   can title an issue like so "Added more log output to resolve #4352".
1. In the description of the pull request, explain the changes that you made,
   any issues you think exist with the pull request you made, and any questions
   you have for the maintainer. It's OK if your pull request is not perfect (no
   pull request is), the reviewer will be able to help you fix any problems and
   improve it!
1. Wait for the pull request to be reviewed by a maintainer.
1. Make changes to the pull request if the reviewing maintainer recommends them.
   You can simply make additional changes, `add` and `commit` them, and then
   `push` to the same branch. Any additional commits added to the same branch
   will automatically be included in the pull request.
1. Celebrate your success after your pull request is merged!
1. After your first pull request is merged, it is very likely that you will be
   granted push access to the `byorgey/swarm` repository. This means that
   from now on you can create branches directly within the `swarm` repository
   rather than working in your own fork. For more information, see
   [I have push access to the Swarm repository, now what?](#i-have-push-access-to-the-swarm-repository-now-what).

## I have push access to the Swarm repository, now what?

Inspired by
[Edward Kmett](https://www.reddit.com/r/haskell/comments/5n61uh/an_interview_with_haskell_developer_edward_kmett/dc92hcc/?context=8&depth=9),
Swarm uses
[The Pull Request Hack](https://felixge.de/2013/03/11/the-pull-request-hack.html):
anyone who submits a pull request to Swarm is likely to be granted push access
in short order.

One benefit is that when working on a new feature, you can create a branch in
the `swarm` repository itself (instead of in your own fork) and then open a pull
request from the feature branch to `main`. This is actually _preferred_ since
it makes several things smoother (for example, the `restyled.io` bot works much
better on pulls from local branches than from forks).

Although being given push access does not imply any particular responsibility,
you are welcome to do things such as help review and merge other pull requests,
and help triage, label, and update issues in the issue tracker.

Having push access also means, of course, that you can push directly to
`main`. You are welcome to do so for typos, small fixes, documentation
improvements, and the like; for larger fixes, new features, _etc._ opening a
pull request from a feature branch is still preferred, to give a chance for
others to offer suggestions for improvement.
