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

As with all YAML, note that the order of keys in a key-value mapping
does not matter.

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
| `objectives`   | `[]`     | list    | A list of objectives.  See [Objectives](#objectives).                                                                                                                                                                                                                                                           |
| `solution`     | `null`   | string  | The (optional) text of a Swarm program that, when run on the base robot, completes all the objectives.  For scenarios which are officially part of the Swarm repository, such a solution will be tested as part of our CI.  For scenarios loaded directly from a file, any provided solution is simply ignored. |
| `stepsPerTick` | `null`   | int     | When present, this specifies the maximum number of CESK machine steps each robot is allowed to take per game tick.  It is rather technical and only used in a few automated tests; most scenario authors should not need this.                                                                                  |

### Entities

The top-level `entities` field contains a list of entity
descriptions.  Each entity description is a key-value map described by
the following table.

| Key            | Default? | Type   | Description                                                                                                                                                                                                                                                                      |
|----------------|----------|--------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`         |          | string | The name of the entity.  This is what will show up in the inventory and how the entity can be referred to.                                                                                                                                                                       |
| `display`      |          | map    | [Display](#display) information for the entity.                                                                                                                                                                                                                                  |
| `plural`       | `null`   | string | An explicit plural form of the name of the entity.  If omitted, standard heuristics will be used for forming the English plural of its name.                                                                                                                                     |
| `description`  |          | list   | A description of the entity, as a list of paragraphs.                                                                                                                                                                                                                            |
| `orientation`  | `null`   | list   | A 2-tuple of integers specifying an orientation vector for the entity. Currently unused.                                                                                                                                                                                         |
| `growth`       | `null`   | list   | For growable entities, a 2-tuple of integers specifying the minimum and maximum amount of time taken for one growth stage.  The actual time for one growth stage will be chosen uniformly at random from this range; it takes two growth stages for an entity to be fully grown. |
| `yields`       | `null`   | string | The name of the entity which will be added to a robot's inventory when it executes `grab` or `harvest` on this entity.  If omitted, the entity will simply yield itself.                                                                                                         |
| `properties`   | `[]`     | list   | A list of properties of this entity.  See [Entity properties](#entity-properties).                                                                                                                                                                                               |
| `capabilities` | `[]`     | list   | A list of capabilities provided by entity, when it is installed as a device.  See [Capabilities](#capabilities).                                                                                                                                                                 |

#### Entity properties

The properties an entity may possess are listed below.  Each entity
may possess any number of properties.

- `unwalkable`: robots cannot `move` into a cell containing this
  entity.  If they try, the `move` command will throw an exception.

- `portable`: robots can pick this up using `grab` or `harvest`.
  Trying to execute `grab` or `harvest` on an entity that is not
  `portable` will throw an exception.

- `growable`: when `harvest`ed, the entity will regrow from a seed.

- `infinite`: when `grab`bed or `harvest`ed, the entity will
  immediately respawn.

- `known`: robots know what this is without having to `scan` it first,
  hence it does not show up as a question mark.

#### Capabilities

Each capability enables the evaluation of execution of one or more
commands or language constructs. Rather than listing all possible
capabilities here, which would be annoying to keep up-to-date, see the
(automatically generated) [Commands cheat
sheet](https://github.com/swarm-game/swarm/wiki/Commands-Cheat-Sheet)
on the Swarm wiki.

### Recipes

The top-level `recipes` field contains a list of recipe descriptions.
Each recipe is a key-value map describing a process that takes some
inputs and produces some outputs, which robots can access using `make`
and `drill`.

| Key        | Default? | Type | Description                                                                                                                                                                                                                                                                                                                                                                                                             |
|------------|----------|------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `in`       |          | list | A list of ingredients consumed by the recipe.  Each ingredient is a tuple consisting of an integer and an entity name, indicating the number of copies of the given entity that are needed.                                                                                                                                                                                                                             |
| `out`      |          | list | A list of outputs produced by the recipe.  It is a list of [integer, entity name] tuples just like `in`.                                                                                                                                                                                                                                                                                                                |
| `required` | `[]`     | list | A list of catalysts required by the recipe.  They are neither consumed nor produced, but must be present in order for the recipe to be carried out.  IT is a list of [integer, entity name] tuples just like `in` and `out`.                                                                                                                                                                                            |
| `time`     | 1        | int  | The number of ticks the recipe takes to perform. For recipes which take more than 1 tick, the robot will `wait` for a number of ticks until the recipe is complete.  For example, this is used for many drilling recipes.                                                                                                                                                                                               |
| `weight`   | 1        | int  | Whenever there are multiple recipes that match the relevant criteria, one of them will be chosen at random, with probability proportional to their weights.  For example, suppose there are two recipes that both output a `widget`, one with weight `1` and the other with weight `9`.  When a robot executes `make "widget"`, the first recipe will be chosen 10% of the time, and the second recipe 90% of the time. |


### World

The world.

### Robots

Robots.

### Objectives

Objectives.

## Example

XXX example here
