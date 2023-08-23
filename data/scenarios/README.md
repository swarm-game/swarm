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
 [`data/scenarios`](https://github.com/swarm-game/swarm/tree/main/data/scenarios) of the swarm repository, copy
one, and make whatever changes you want, using the other files as
examples.

If you notice any errors or have any questions or suggestions, feel
free to join the `#swarm` channel on
[Libera.Chat](https://libera.chat/), or [open a pull
request](https://github.com/swarm-game/swarm/blob/main/CONTRIBUTING.md)!

## Loading scenarios

The "blessed" scenarios that come with Swarm are stored in
`data/scenarios` and can be accessed via the "New game" menu.
However, other scenarios can be loaded directly from a file: simply
run swarm with the `--scenario` flag (`-i` for short) and point it to
a specific `.yaml` file containing a scenario.  For example:

```
swarm --scenario myscenarios/fun.yaml
```

## Examples

Many examples can be found in
https://github.com/swarm-game/swarm/tree/main/data/scenarios .

## The scenario file format

Scenarios are stored in [YAML files](https://yaml.org/). If you want
to learn about YAML, see the link above; this is not a YAML tutorial.
However, Swarm tends to use a fairly simple subset of YAML, and should
be easy to pick up even if you have never seen YAML before.  Swarm
uses the Haskell [yaml
library](https://hackage.haskell.org/package/yaml) for parsing `.yaml`
files.

### Scenario schema

To ease writing Scenario YAML files, you can use a JSON schema that includes
the information below in a machine readable format. This will allow
your editor to highlight the errors as you are writing.

#### VS Code

If you are using Visual Studio Code or VSCodium, you need to have
the [YAML extension](https://open-vsx.org/extension/redhat/vscode-yaml)
installed.

To point the editor to the right schema for scenarios in this repository,
you can use this `settings.json`:
```JSON
{
  "yaml.schemas": {
    "https://raw.githubusercontent.com/swarm-game/swarm/main/data/schema/scenario.json": [
      "data/scenarios/*.yaml",
      "data/scenarios/**/*.yaml"
    ],
    "https://raw.githubusercontent.com/swarm-game/swarm/main/data/schema/entities.json": [
      "data/entities.yaml"
    ],
    "https://raw.githubusercontent.com/swarm-game/swarm/main/data/schema/recipes.json": [
      "data/recipes.yaml"
    ]
  }
}
```

#### CLI

You can also check the files from the command line:
```Bash
# install latest jsonschema executable version (tested with 4.9.1)
pip install jsonschema
# try it on provided scenarios
yq eval scenarios/creative.yaml -o json | jsonschema data/schema/scenario.json
# try that it works on empty JSON
echo {} | jsonschema data/schema/scenario.json
# {}: 'name' is a required property
# {}: 'world' is a required property
# {}: 'robots' is a required property
```

### YAML conventions

Objects (key-value mappings) are described below using tables.  Note
that a blank "Default?" column means the key is required; other keys
are optional and take on the indicated default value when they are not
present. The order of keys in a key-value mapping does not matter.

YAML is untyped, but we try to give a more precise idea of the
expected types in the tables below.
- `foo list` means a list where all the elements are of type `foo`.
- A type like `int × int` means a pair of int values.  YAML does not actually have
  tuples, only lists, so in practice, an `int × int` value is written
  as a list with exactly two elements.  Likewise, `int × string`
  denotes a list with exactly two elements, the first being an `int`
  and the second being a `string`.
- `object` denotes a generic key-value mapping.  Whenever `object` is
  used, you will find a link to a more specific description of the
  keys and values expected.

### Top level

At the top level, a scenario file contains a key-value mapping,
described by the following table.

| Key            | Default? | Type                            | Description                                                                                                                                                                                                                                                                                                                                                   |
|----------------|----------|---------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `version`      |          | `int`                           | The version number of the scenario schema.  Currently, this should always be 1.                                                                                                                                                                                                                                                                               |
| `name`         |          | `string`                        | The name of the scenario. For official scenarios, this is what shows up in the new game menu.                                                                                                                                                                                                                                                                 |
| `description`  | `""`     | `string`                        | A short description of the scenario. This shows up next to the new game menu when the scenario is selected.                                                                                                                                                                                                                                                   |
| `author`       | `null`   | `string`                        | The author of the scenario (optional).  Typically this is a person's name, but it can be any string. It is displayed under the scenario description in the new game menu.                                                                                                                                                                                     |
| `creative`     | `False`  | `boolean`                       | Whether the scenario should start out in creative mode.                                                                                                                                                                                                                                                                                                       |
| `seed`         | `null`   | `int`                           | An optional seed that will be used to seed the random number generator.  If a procedurally generated world is used, the seed hence determines the world.  Hence, if the seed is specified, the procedurally generated world will be exactly the same every time, for every player.  If omitted, a random seed will be used every time the scenario is loaded. |
| `entities`     | `[]`     | [`entity`](#entities) list      | An optional list of custom entities, to be used in addition to the built-in entities.  See [Entities](#entities).                                                                                                                                                                                                                                             |
| `recipes`      | `[]`     | [`recipe`](#recipes) list       | An optional list of custom recipes, to be used in addition to the built-in recipes. They can refer to built-in entities as well as custom entities. See [Recipes](#recipes).                                                                                                                                                                                  |
| `known`        | `[]`     | `string list`                   | A list of names of standard or custom entities which should have the `Known` property added to them; that is, robots should know what they are without having to scan them.                                                                                                                                                                                   |
| `world`        |          | `object`                        | A description of the world.  See [World](#world).                                                                                                                                                                                                                                                                                                             |
| `robots`       |          | [`robot`](#robots) list         | A list of robots that will inhabit the world.  See [Robots](#robots).                                                                                                                                                                                                                                                                                         |
| `objectives`   | `[]`     | [`objective`](#objectives) list | An optional list of objectives, aka winning conditions.  The player has to complete the objectives in sequence to win.  See [Objectives](#objectives).                                                                                                                                                                                                        |
| `solution`     | `null`   | `string`                        | The (optional) text of a Swarm program that, when run on the base robot, completes all the objectives.  For scenarios which are officially part of the Swarm repository, such a solution will be tested as part of CI testing.  For scenarios loaded directly from a file, any provided solution is simply ignored.                                           |
| `stepsPerTick` | `null`   | `int`                           | When present, this specifies the maximum number of CESK machine steps each robot is allowed to take per game tick.  It is rather obscure and technical and only used in a few automated tests; most scenario authors should not need this.                                                                                                                    |

### Entities

The top-level `entities` field contains a list of entity descriptions.
Each entity description is a key-value mapping described by the following
table.

| Key            | Default? | Type          | Description                                                                                                                                                                                                                                                                      |
|----------------|----------|---------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`         |          | `string`      | The name of the entity.  This is what will show up in the inventory and how the entity can be referred to.                                                                                                                                                                       |
| `display`      |          | `object`      | [Display](#display) information for the entity.                                                                                                                                                                                                                                  |
| `plural`       | `null`   | `string`      | An explicit plural form of the name of the entity.  If omitted, standard heuristics will be used for forming the English plural of its name.                                                                                                                                     |
| `description`  |          | `string list` | A description of the entity, as a list of paragraphs.                                                                                                                                                                                                                            |
| `orientation`  | `null`   | `int × int`   | A 2-tuple of integers specifying an orientation vector for the entity. Currently unused.                                                                                                                                                                                         |
| `growth`       | `null`   | `int × int`   | For growable entities, a 2-tuple of integers specifying the minimum and maximum amount of time taken for one growth stage.  The actual time for one growth stage will be chosen uniformly at random from this range; it takes two growth stages for an entity to be fully grown. |
| `combustion`      |          | `object`      | [Combustion](#combustion) information for the entity.                                                                                                                                                                                                                                  |
| `yields`       | `null`   | `string`      | The name of the entity which will be added to a robot's inventory when it executes `grab` or `harvest` on this entity.  If omitted, the entity will simply yield itself.                                                                                                         |
| `properties`   | `[]`     | `string list` | A list of properties of this entity.  See [Entity properties](#entity-properties).                                                                                                                                                                                               |
| `capabilities` | `[]`     | `string list` | A list of capabilities provided by entity, when it is equipped as a device.  See [Capabilities](#capabilities).                                                                                                                                                                 |

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

### Combustion

The *combustion* property specifies whether and how an entity may combust, described by the following table.

| Key              | Default? | Type      | Description                                                                                                                                                                                            |
|------------------|----------|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ignition`           | `0.5`    | `number`  | The rate of ignition by a neighbor, per tick.                                                                                                                                 |
| `duration`       | `null`   | `int × int`   | For combustible entities, a 2-tuple of integers specifying the minimum and maximum amount of time taken for combustion. |
| `product`           | `ash`    | `string`  | What entity, if any, is left over after combustion                                                                                                                                 |

### Display

A *display* specifies how an entity or a robot (robots are essentially
special kinds of entities) is displayed in the world.  It consists of
a key-value mapping described by the following table.

| Key              | Default? | Type      | Description                                                                                                                                                                                            |
|------------------|----------|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `char`           | `' '`    | `string`  | The default character that should be used to draw the robot or entity.                                                                                                                                 |
| `orientationMap` | `{}`     |           | A map to override display character for any of the (lowercase) cardinal directions                                                                                                                                                                                 |
| `curOrientation` | `null`   |           | TODO currently unused                                                                                                                                                                                  |
| `attr`           | `entity` | `string`  | The name of the attribute that should be used to style the robot or entity.  A list of currently valid attributes can be found at https://github.com/swarm-game/swarm/blob/main/src/Swarm/TUI/Attr.hs. |
| `priority`       | `1`      | `int`     | When multiple entities and robots occupy the same cell, the one with the highest priority is drawn.  By default, entities have priority `1`, and robots have priority `10`.                            |
| `invisible`      | `False`  | `boolean` | Whether the entity or robot should be invisible.  Invisible entities and robots are not drawn, but can still be interacted with in otherwise normal ways. System robots are invisible by default. |


### Recipes

The top-level `recipes` field contains a list of recipe descriptions.
Each recipe is a key-value mapping describing a process that takes some
inputs and produces some outputs, which robots can access using `make`
and `drill`.

| Key        | Default? | Type                  | Description                                                                                                                                                                                                                                                                                                                                                                                                             |
|------------|----------|-----------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `in`       |          | `(int × string) list` | A list of ingredients consumed by the recipe.  Each ingredient is a tuple consisting of an integer and an entity name, indicating the number of copies of the given entity that are needed.                                                                                                                                                                                                                             |
| `out`      |          | `(int × string) list` | A list of outputs produced by the recipe.  It is a list of [count, entity name] tuples just like `in`.                                                                                                                                                                                                                                                                                                                  |
| `required` | `[]`     | `(int × string) list` | A list of catalysts required by the recipe.  They are neither consumed nor produced, but must be present in order for the recipe to be carried out.  It is a list of [count, entity name] tuples just like `in` and `out`.                                                                                                                                                                                              |
| `time`     | 1        | `int`                 | The number of ticks the recipe takes to perform. For recipes which take more than 1 tick, the robot will `wait` for a number of ticks until the recipe is complete.  For example, this is used for many drilling recipes.                                                                                                                                                                                               |
| `weight`   | 1        | `int`                 | Whenever there are multiple recipes that match the relevant criteria, one of them will be chosen at random, with probability proportional to their weights.  For example, suppose there are two recipes that both output a `widget`, one with weight `1` and the other with weight `9`.  When a robot executes `make "widget"`, the first recipe will be chosen 10% of the time, and the second recipe 90% of the time. |

### World

The top-level `world` field contains a key-value mapping describing the
world, that is, a description of the terrain and entities that exist
at various locations.

| Key          | Default? | Type        | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|--------------|----------|-------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `dsl`        | `null`   | `string`    | An expression of the [Swarm world description DSL](../worlds/README.md).  If specified, this will be used as the base layer for the world.                                                                                                                                                                                                                                                                                                                                                                       |
| `offset`     | `False`  | `boolean`   | Whether the `base` robot's position should be moved to the nearest "good" location, currently defined as a location near a tree, in a 16x16 patch which contains at least one each of `tree`, `copper ore`, `bit (0)`, `bit (1)`, `rock`, `lambda`, `water`, and `sand`. The `classic` scenario uses `offset: True` to make sure that the it is not unreasonably difficult to obtain necessary resources in the early game.  See https://github.com/swarm-game/swarm/blob/main/src/Swarm/Game/WorldGen.hs#L204 . |
| `scrollable` | `True`   | `boolean`   | Whether players are allowed to scroll the world map.                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| `palette`    | `{}`     | `object`    | The `palette` maps single character keys to tuples representing contents of cells in the world, so that a world containing entities and robots can be drawn graphically.  See [Cells](#cells) for the contents of the tuples representing a cell.                                                                                                                                                                                                                                                                |
| `map`        | `""`     | `string`    | A rectangular string, using characters from the `palette`, exactly specifying the contents of a rectangular portion of the world.  Leading spaces are ignored.  The rest of the world is either filled by the `default` cell, or by procedural generation otherwise. Note that this is optional; if omitted, the world will simply be filled with the `default` cell or procedurally generated.                                                                                                                  |
| `upperleft`  | `[0,0]`  | `int × int` | A 2-tuple of `int` values specifying the (x,y) coordinates of the upper left corner of the `map`.                                                                                                                                                                                                                                                                                                                                                                                                                |

#### Cells

Each cell of the world is specified by a list of terrain, optional entity
and robots present (if any). For example, `[grass]`, `[grass, tree]`,
or `[grass, null, base]`.

- The first (required) item specifies the terrain.  Currently, valid
  terrain values are `stone`, `dirt`, `grass`, `ice`, or `blank`.
- The second item (if present) specifies the name of an entity which
  should be present in the cell.  This may be a built-in entity, or a
  custom entity specified in the `entities` section.  `null` may be
  used to explicitly specify no entity in the cell.
- The third item and later (if present) specifies the names of the robots
  which should be present in the cell.  These must be names of robots
  specified in the `robots` section.  A copy of each robot will be
  created at each location in the `map` where it is drawn.

  Although multiple robots may be in a single location in general,
  there is currently no way to specify more than one robot for a
  cell in the world description.

If a 1-tuple is used, it specifies a terrain value with no entity or
robot.  A 2-tuple specifies a terrain value and entity, but no robot.

### Robots

The top-level `robots` field contains a list of robot descriptions.
Each robot description is a key-value mapping described by the following
table.

| Key           | Default? | Type                  | Description                                                                                                                                                                                                                                                                                                                                                                                                                    |
|---------------|----------|-----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`        |          | `string`              | The name of the robot.  This shows up in the list of robots in the game (F2), and is also how the robot will be referred to in the [world](#world) `palette`.                                                                                                                                                                                                                                                                  |
| `description` | `[]`     | `string list`         | A description of the robot, given as a list of paragraphs.  This is currently not used for much (perhaps not at all?).                                                                                                                                                                                                                                                                                                         |
| `loc`         | `null`   | `int × int`           | An optional (x,y) starting location for the robot.  If the `loc` field is specified, then a concrete robot will be created at the given location.  If this field is omitted, then this robot record exists only as a *template* which can be referenced from a [cell](#cells) in the [world](#world) `palette`.  Concrete robots will then be created wherever the corresponding palette character is used in the world `map`. |
| `dir`         | `[0,0]`  | `int × int`           | An optional starting orientation of the robot, expressed as a vector.  Every time the robot executes a `move` command, this vector will be added to its position.  Typically, this is a unit vector in one of the four cardinal directions, although there is no particular reason that it has to be. When omitted, the robot's direction will be the zero vector.                                                                                                 |
| `display`     | default  | `map`                 | [Display](#display) information for the robot. If this field is omitted, the [default robot display](#display) will be used.                                                                                                                                                                                                                                                                                                   |
| `program`     | `null`   | `string`              | This is the text of a Swarm program which the robot should initially run, and must be syntax- and type-error-free.  If omitted, the robot will simply be idle.                                                                                                                                                                                                                                                                 |
| `devices`     | `[]`     | `string list`         | A list of entity names which should be *equipped* as the robot's devices, i.e. entities providing capabilities to run commands and interpret language constructs.                                                                                                                                                                                                                                                             |
| `inventory`   | `[]`     | `(int × string) list` | A list of [count, entity name] pairs, specifying the entities in the robot's starting inventory, and the number of each.                                                                                                                                                                                                                                                                                                       |
| `system`      | `False`  | `boolean`             | Whether the robot is a "system" robot.  System robots can do anything, without regard for devices and capabilities. System robots are invisible by default.                                                                                                                                                                                                                                                                                                            |
| `heavy`       | `False`  | `boolean`             | Whether the robot is heavy.  Heavy robots require `tank treads` to `move` (rather than just `treads` for other robots).                                                                                                                                                                                                                                                                                                        |

#### Base robot

There must be at most one **base** robot in the world. Since concrete robots can be created
either via the `loc` attribute or via the map and palette, use the following guide to
ensure the base robot is the one you intended:

1. Always list the intended **base** as the first robot definition in your scenario.
2. The first robot with a `loc` attribute will become the base, even if other robots are defined earlier.
3. Without any located robots, if multiple robots are instantiated on the map from
   the first robot definition, the first robot in
   [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order)
   shall be the base.

### Objectives

The top-level `objectives` field contains a list of objectives that
must be completed in sequence.  Each objective is a key-value mapping
described by the following table.

| Key         | Default? | Type          | Description                                                                                                                                                                                                                                                                                                                                       |
|-------------|----------|---------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `goal`      | `[]`     | `string list` | A list of paragraphs describing the objective.  This text is shown to the player in a popup dialog box as soon as the scenario starts, or the previous objective is completed, and the player can recall the popup at any time with `Ctrl-G`.                                                                                                     |
| `condition` |          | `string`      | The condition is the text of a Swarm program of type `cmd bool`, which will be run once per game tick on a freshly generated system robot.  It is run hypothetically, that is, it is run in a copy of the current game state which is thrown away once the program has run to completion.  The condition is met when this program returns `true`. |
