# Scenario Authoring Guide

A *scenario* is a description of a starting world, robots, and
entities, along with (optionally) specific winning conditions and
description text. All the various game modes in Swarm (classic mode,
creative mode, tutorial, challenges, *etc.*) are described via
scenarios. Scenarios are stored in human-friendly `.yaml` files, so it
is very easy---and encouraged!---for you to create your own scenarios
representing puzzles, challenges, works of art, or whatever you want.
This page formally documents the `.yaml` format and also provides some
tips for authoring your own scenarios.

If you're impatient to get started, note that a much quicker route is
to look at the scenario files stored in
https://github.com/swarm-game/swarm/tree/main/data/scenarios , copy
one, and make whatever changes you want, using the other files as
examples.

XXX Things to add to scenarios:
  - Version field
  - Author field

## Loading scenarios

The "blessed" scenarios that come with Swarm are stored in
`data/scenarios` and can be accessed via the "New game" menu.
However, other scenarios can be loaded directly from a file: simply
run swarm with the `--scenario` flag (`-c` for short) and point it to
a specific `.yaml` file containing a scenario.  For example:

```
swarm --scenario myscenarios/fun.yaml
```

## The scenario file format

Scenarios are stored in [YAML files](https://yaml.org/). If you want
to learn about YAML, see the link above; this is not a YAML tutorial.
However, Swarm tends to use a fairly simple subset of YAML, and should
be easy to pick up even if you have never seen YAML before.  Swarm
uses the Haskell [yaml
library](https://hackage.haskell.org/package/yaml) for parsing `.yaml`
files, and hence we use YAML 1.1.

### Top level

At the top level, a scenario file contains a key-value mapping,
described by the following table.  Note that a blank Default? column
means the key is required; other keys are optional and take on the
indicated default value when they are not present.

| Key            | Default? | Type    | Description                                                                                                                                                                                                                                                                                                     |
|----------------|----------|---------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`         |          | string  | The name of the scenario.  Blah blah blah.                                                                                                                                                                                                                                                                      |
| `description`  | `""`     | string  | A short description of the scenario.                                                                                                                                                                                                                                                                            |
| `creative`     | `False`  | boolean | Whether the scenario should start out in creative mode.                                                                                                                                                                                                                                                         |
| `seed`         | `null`   | int     | The seed that will be used to seed the random number generator.  If a procedurally generated world is used, the seed hence determines the world.  If omitted, a random seed will be used.                                                                                                                       |
| `entities`     | `[]`     | list    | A list of custom entities.  See [Entities](#entities).                                                                                                                                                                                                                                                          |
| `recipes`      | `[]`     | list    | A list of custom recipes.  See [Recipes](#recipes).                                                                                                                                                                                                                                                             |
| `world`        |          | map     | A description of the world.  See [World](#world).                                                                                                                                                                                                                                                               |
| `robots`       |          | list    | A list of robots that will inhabit the world.  See [Robots](#robots).                                                                                                                                                                                                                                           |
| `objectives`   |          | list    | A list of objectives.  See [Objectives](#objectives).                                                                                                                                                                                                                                                           |
| `solution`     | `null`   | string  | The (optional) text of a Swarm program that, when run on the base robot, completes all the objectives.  For scenarios which are officially part of the Swarm repository, such a solution will be tested as part of our CI.  For scenarios loaded directly from a file, any provided solution is simply ignored. |
| `stepsPerTick` | `null`   | int     | When present, this specifies the maximum number of CESK machine steps each robot is allowed to take per game tick.  It is rather technical and only used in a few automated tests; most scenario authors should not need this.                                                                                  |
|                |          |         |                                                                                                                                                                                                                                                                                                                 |

### Entities

Foo bar, all about entities!

### Recipies

All about recipes.

### World

The world.

### Robots

Robots.

### Objectives

Objectives.
