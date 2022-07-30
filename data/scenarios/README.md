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
files.

### Top level

At the top level, a scenario file contains a key-value mapping,
described by the following table.  Note that a blank Default? column
means the key is required; other keys are optional and take on the
indicated default value when they are not present.

As with all YAML, note that the order of keys in a key-value mapping
does not matter.

| Key            | Default? | Type       | Description                                                                                                                                                                                                                                                                                                                                                   |
|----------------|----------|------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`         |          | `string`   | The name of the scenario. For official scenarios, this is what shows up in the new game menu.                                                                                                                                                                                                                                                                 |
| `description`  | `""`     | `string`   | A short description of the scenario. This shows up next to the new game menu when the scenario is selected.                                                                                                                                                                                                                                                   |
| `creative`     | `False`  | `boolean`  | Whether the scenario should start out in creative mode.                                                                                                                                                                                                                                                                                                       |
| `seed`         | `null`   | `int`      | An optional seed that will be used to seed the random number generator.  If a procedurally generated world is used, the seed hence determines the world.  Hence, if the seed is specified, the procedurally generated world will be exactly the same every time, for every player.  If omitted, a random seed will be used every time the scenario is loaded. |
| `entities`     | `[]`     | `map list` | An optional list of custom entities, to be used in addition to the built-in entities.  See [Entities](#entities).                                                                                                                                                                                                                                             |
| `recipes`      | `[]`     | `map list` | An optional list of custom recipes, to be used in addition to the built-in recipes. They can refer to built-in entities as well as custom entities. See [Recipes](#recipes).                                                                                                                                                                                  |
| `world`        |          | `map`      | A description of the world.  See [World](#world).                                                                                                                                                                                                                                                                                                             |
| `robots`       |          | `map list` | A list of robots that will inhabit the world.  See [Robots](#robots).                                                                                                                                                                                                                                                                                         |
| `objectives`   | `[]`     | `map list` | An optional list of objectives, aka winning conditions.  The player has to complete the objectives in sequence to win.  See [Objectives](#objectives).                                                                                                                                                                                                        |
| `solution`     | `null`   | `string`   | The (optional) text of a Swarm program that, when run on the base robot, completes all the objectives.  For scenarios which are officially part of the Swarm repository, such a solution will be tested as part of CI testing.  For scenarios loaded directly from a file, any provided solution is simply ignored.                                           |
| `stepsPerTick` | `null`   | `int`      | When present, this specifies the maximum number of CESK machine steps each robot is allowed to take per game tick.  It is rather obscure and technical and only used in a few automated tests; most scenario authors should not need this.                                                                                                                    |

### Entities

The top-level `entities` field contains a list of entity descriptions.
Each entity description is a key-value map described by the following
table.

| Key            | Default? | Type          | Description                                                                                                                                                                                                                                                                      |
|----------------|----------|---------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`         |          | `string`      | The name of the entity.  This is what will show up in the inventory and how the entity can be referred to.                                                                                                                                                                       |
| `display`      |          | `map`         | [Display](#display) information for the entity.                                                                                                                                                                                                                                  |
| `plural`       | `null`   | `string`      | An explicit plural form of the name of the entity.  If omitted, standard heuristics will be used for forming the English plural of its name.                                                                                                                                     |
| `description`  |          | `string list` | A description of the entity, as a list of paragraphs.                                                                                                                                                                                                                            |
| `orientation`  | `null`   | `int × int`   | A 2-tuple of integers specifying an orientation vector for the entity. Currently unused.                                                                                                                                                                                         |
| `growth`       | `null`   | `int × int`   | For growable entities, a 2-tuple of integers specifying the minimum and maximum amount of time taken for one growth stage.  The actual time for one growth stage will be chosen uniformly at random from this range; it takes two growth stages for an entity to be fully grown. |
| `yields`       | `null`   | `string`      | The name of the entity which will be added to a robot's inventory when it executes `grab` or `harvest` on this entity.  If omitted, the entity will simply yield itself.                                                                                                         |
| `properties`   | `[]`     | `string list` | A list of properties of this entity.  See [Entity properties](#entity-properties).                                                                                                                                                                                               |
| `capabilities` | `[]`     | `string list` | A list of capabilities provided by entity, when it is installed as a device.  See [Capabilities](#capabilities).                                                                                                                                                                 |

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

### Display

XXX display for both entities and robots (robots are special entities).

### Recipes

The top-level `recipes` field contains a list of recipe descriptions.
Each recipe is a key-value map describing a process that takes some
inputs and produces some outputs, which robots can access using `make`
and `drill`.

| Key        | Default? | Type                  | Description                                                                                                                                                                                                                                                                                                                                                                                                             |
|------------|----------|-----------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `in`       |          | `(int × string) list` | A list of ingredients consumed by the recipe.  Each ingredient is a tuple consisting of an integer and an entity name, indicating the number of copies of the given entity that are needed.                                                                                                                                                                                                                             |
| `out`      |          | `(int × string) list` | A list of outputs produced by the recipe.  It is a list of [count, entity name] tuples just like `in`.                                                                                                                                                                                                                                                                                                                  |
| `required` | `[]`     | `(int × string) list` | A list of catalysts required by the recipe.  They are neither consumed nor produced, but must be present in order for the recipe to be carried out.  It is a list of [count, entity name] tuples just like `in` and `out`.                                                                                                                                                                                              |
| `time`     | 1        | `int`                 | The number of ticks the recipe takes to perform. For recipes which take more than 1 tick, the robot will `wait` for a number of ticks until the recipe is complete.  For example, this is used for many drilling recipes.                                                                                                                                                                                               |
| `weight`   | 1        | `int`                 | Whenever there are multiple recipes that match the relevant criteria, one of them will be chosen at random, with probability proportional to their weights.  For example, suppose there are two recipes that both output a `widget`, one with weight `1` and the other with weight `9`.  When a robot executes `make "widget"`, the first recipe will be chosen 10% of the time, and the second recipe 90% of the time. |
|            |          |                       |                                                                                                                                                                                                                                                                                                                                                                                                                         |

### World

The top-level `world` field contains a key-value map describing the
world, that is, a description of the terrain and entities that exist
at various locations.

| Key         | Default? | Type          | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|-------------|----------|---------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `default`   | `null`   | `string list` | A tuple representing the contents of a default cell (see [Cells](#cells), except that the default cell may not contain a robot).  If this key is present, it means that the whole world besides the part specified with the `map` will be filled with this default cell.  If omitted, the world besides the part specified with the `map` will be procedurally generated.                                                                                                                                        |
| `offset`    | `False`  | `boolean`     | Whether the `base` robot's position should be moved to the nearest "good" location, currently defined as a location near a tree, in a 16x16 patch which contains at least one each of `tree`, `copper ore`, `bit (0)`, `bit (1)`, `rock`, `lambda`, `water`, and `sand`. The `classic` scenario uses `offset: True` to make sure that the it is not unreasonably difficult to obtain necessary resources in the early game.  See https://github.com/swarm-game/swarm/blob/main/src/Swarm/Game/WorldGen.hs#L204 . |
| `palette`   | `{}`     | `map`         | The `palette` maps single character keys to tuples representing contents of cells in the world, so that a world containing entities and robots can be drawn graphically.  See [Cells](#cells) for the contents of the tuples representing a cell.                                                                                                                                                                                                                                                                |
| `map`       | `""`     | `string`      | A rectangular string, using characters from the `palette`, exactly specifying the contents of a rectangular portion of the world.  Leading spaces are ignored.  The rest of the world is either filled by the `default` cell, or by procedural generation otherwise. Note that this is optional; if omitted, the world will simply be filled with the `default` cell or procedurally generated.                                                                                                                  |
| `upperleft` | `[0,0]`  | `int × int`   | A 2-tuple of `int` values specifying the (x,y) coordinates of the upper left corner of the `map`.                                                                                                                                                                                                                                                                                                                                                                                                                |

#### Cells

Each cell of the world is specified by a 1-, 2-, or 3-tuple, for
example, `[grass]`, `[grass, tree]`, or `[grass, null, base]`.

- The first (required) item specifies the terrain.  Currently, valid
  terrain values are `stone`, `dirt`, `grass`, `ice`, or `blank`.
- The second item (if present) specifies the name of an entity which
  should be present in the cell.  This may be a built-in entity, or a
  custom entity specified in the `entities` section.  `null` may be
  used to explicitly specify no entity in the cell.
- The third item (if present) specifies the name of a robot which
  should be present in the cell.  This must be the name of a robot
  specified in the `robots` section.  A copy of the robot will be
  created at each location in the `map` where it is drawn.

  Although multiple robots may be in a single location in general,
  there is currently no way to specify more than one robot for a
  cell in the world description.

If a 1-tuple is used, it specifies a terrain value with no entity or
robot.  A 2-tuple specifies a terrain value and entity, but no robot.

### Robots

The top-level `robots` field contains a list of robot descriptions.
Each robot description is a key-value map described by the following
table.

| Key           | Default? | Type                  | Description                                                                                                                                                                                                                                                                                                                                                                                                                    |
|---------------|----------|-----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`        |          | `string`              | The name of the robot.  This shows up in the list of robots in the game (F2), and is also how the robot will be referred to in the [world](#world) `palette`.                                                                                                                                                                                                                                                                  |
| `description` | `[]`     | `[string]`            | A description of the robot, given as a list of paragraphs.  This is currently not used for much (perhaps not at all?).                                                                                                                                                                                                                                                                                                         |
| `loc`         | `null`   | `int × int`           | An optional (x,y) starting location for the robot.  If the `loc` field is specified, then a concrete robot will be created at the given location.  If this field is omitted, then this robot record exists only as a *template* which can be referenced from a [cell](#cells) in the [world](#world) `palette`.  Concrete robots will then be created wherever the corresponding palette character is used in the world `map`. |
| `dir`         |          | `int × int`           | The starting orientation of the robot, expressed as a vector.  Every time the robot executes a `move` command, this vector will be added to its position.  Typically, this is a unit vector in one of the four cardinal directions, although there is no particular reason that it has to be.                                                                                                                                  |
| `display`     | default  | `map`                 | [Display](#display) information for the robot. If this field is omitted, the [default robot display](#display) will be used.                                                                                                                                                                                                                                                                                                   |
| `program`     | `null`   | `string`              | This is the text of a Swarm program which the robot should initially run, and must be syntax- and type-error-free.  If omitted, the robot will simply be idle.                                                                                                                                                                                                                                                                 |
| `devices`     | `[]`     | `string list`         | A list of entity names which should be *installed* as the robot's devices, i.e. entities providing capabilities to run commands and interpret language constructs.                                                                                                                                                                                                                                                             |
| `inventory`   | `[]`     | `(int × string) list` | A list of [count, entity name] pairs, specifying the entities in the robot's starting inventory, and the number of each.                                                                                                                                                                                                                                                                                                       |
| `system`      | `False`  | `boolean`             | Whether the robot is a "system" robot.  System robots can do anything, without regard for devices and capabilities.                                                                                                                                                                                                                                                                                                            |
| `heavy`       | `False`  | `boolean`             | Whether the robot is heavy.  Heavy robots require `tank treads` to `move` (rather than just `treads` for other robots).                                                                                                                                                                                                                                                                                                        |


### Objectives

Objectives.

## Example

XXX example here
