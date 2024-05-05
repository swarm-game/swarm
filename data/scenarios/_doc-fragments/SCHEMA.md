## Swarm YAML schema

### YAML conventions

Objects (key-value mappings) are described below using tables.  Note
that a blank "**Default?**" column means the key is required; other keys
are optional and take on the indicated default value when they are not
present. The order of keys in a key-value mapping does not matter.

YAML is untyped, but we try to give a more precise idea of the
expected types in the tables below.
- `foo list` means a list where all the elements are of type `foo`.
- Some values are tuples. The types and meaning of such tuple element
  are presented in tables with an "**Index**" column.

### Top level

At the top level, a scenario file contains a key-value mapping described
by the following table.

| Key            | Default? | Type                                                                 | Description                                                                                                                                                                                                                                                                                                                                                |
|----------------|----------|----------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `attrs`        |          | [attribute](#attributes "Link to object properties") list            | A list of local attribute definitions                                                                                                                                                                                                                                                                                                                      |
| `author`       |          | `string`                                                             | The author of the scenario (optional). Typically this is a person's name, but it can be any string. It is displayed under the scenario description in the new game menu.                                                                                                                                                                                   |
| `creative`     | `False`  | `boolean`                                                            | Whether the scenario should start out in creative mode.                                                                                                                                                                                                                                                                                                    |
| `description`  |          | `string`                                                             | A short description of the scenario. This shows up next to the new game menu when the scenario is selected.                                                                                                                                                                                                                                                |
| `entities`     | `[]`     | [entity](#entity "Link to object properties") list                   | An optional list of custom entities, to be used in addition to the built-in entities.                                                                                                                                                                                                                                                                      |
| `known`        | `[]`     | `string` list                                                        | A list of names of standard or custom entities which should have the Known property added to them; that is, robots should know what they are without having to scan them.                                                                                                                                                                                  |
| `name`         |          | `string`                                                             | The name of the scenario. For official scenarios, this is what shows up in the new game menu.                                                                                                                                                                                                                                                              |
| `objectives`   | `[]`     | [objective](#objective "Link to object properties") list             | An optional list of objectives, aka winning conditions. The player has to complete the objectives in sequence to win.                                                                                                                                                                                                                                      |
| `recipes`      | `[]`     | [recipe](#recipe "Link to object properties") list                   | An optional list of custom recipes, to be used in addition to the built-in recipes. They can refer to built-in entities as well as custom entities.                                                                                                                                                                                                        |
| `robots`       |          | [robot](#robot "Link to object properties") list                     | A list of robots that will inhabit the world.                                                                                                                                                                                                                                                                                                              |
| `seed`         |          | `number`                                                             | An optional seed that will be used to seed the random number generator. If a procedurally generated world is used, the seed hence determines the world. Hence, if the seed is specified, the procedurally generated world will be exactly the same every time, for every player. If omitted, a random seed will be used every time the scenario is loaded. |
| `solution`     |          | `string`                                                             | The (optional) text of a Swarm program that, when run on the base robot, completes all the objectives. For scenarios which are officially part of the Swarm repository, such a solution will be tested as part of CI testing.                                                                                                                              |
| `stepsPerTick` |          | `number`                                                             | When present, this specifies the maximum number of CESK machine steps each robot is allowed to take per game tick. It is rather obscure and technical and only used in a few automated tests; most scenario authors should not need this.                                                                                                                  |
| `structures`   |          | [named-structure](#named-structure "Link to object properties") list | Structure definitions                                                                                                                                                                                                                                                                                                                                      |
| `subworlds`    |          | [world](#world "Link to object properties") list                     | A list of subworld definitions                                                                                                                                                                                                                                                                                                                             |
| `terrains`     | `[]`     | [terrain](#terrain "Link to object properties") list                 | An optional list of custom terrain, to be used in addition to the built-in terrain.                                                                                                                                                                                                                                                                        |
| `version`      |          | `number`                                                             | The version number of the scenario schema. Currently, this should always be `1`.                                                                                                                                                                                                                                                                           |
| `world`        |          | [world](#world "Link to object properties")                          |                                                                                                                                                                                                                                                                                                                                                            |

### Attributes

Scenario-local attribute definitions

| Key     | Default? | Type          | Description           |
|---------|----------|---------------|-----------------------|
| `bg`    |          | `string`      | Background color      |
| `fg`    |          | `string`      | Foreground color      |
| `name`  |          | `string`      | Name of attribute     |
| `style` |          | `string` list | Style properties list |

### Entity

Description of an entity in the Swarm game

| Key            | Default? | Type                                                  | Description                                                                                                                                                                                                                                                                     |
|----------------|----------|-------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `biomes`       | `[]`     | `string` list                                         | A list of terrains that support growth by this entity. Empty list means no growth restrictions.                                                                                                                                                                                 |
| `capabilities` | `[]`     | `string` or `object` list                             | A list of capabilities provided by entity, when it is equipped as a device. See [Capabilities](https://github.com/swarm-game/swarm/wiki/Capabilities-cheat-sheet).                                                                                                              |
| `combustion`   |          | [combustion](#combustion "Link to object properties") | Properties of combustion.                                                                                                                                                                                                                                                       |
| `description`  |          | `string` list                                         | A description of the entity, as a list of paragraphs.                                                                                                                                                                                                                           |
| `display`      |          | [display](#display "Link to object properties")       | Display information for the entity.                                                                                                                                                                                                                                             |
| `growth`       |          | `array`                                               | For growable entities, a 2-tuple of integers specifying the minimum and maximum amount of time taken for one growth stage. The actual time for one growth stage will be chosen uniformly at random from this range; it takes two growth stages for an entity to be fully grown. |
| `name`         |          | `string`                                              | The name of the entity. This is what will show up in the inventory and how the entity can be referred to.                                                                                                                                                                       |
| `orientation`  |          | `array`                                               | A 2-tuple of integers specifying an orientation vector for the entity. Currently unused.                                                                                                                                                                                        |
| `plural`       |          | `string`                                              | An explicit plural form of the name of the entity. If omitted, standard heuristics will be used for forming the English plural of its name.                                                                                                                                     |
| `properties`   | `[]`     | `string` list                                         | A list of properties of this entity.                                                                                                                                                                                                                                            |
| `tags`         |          | `string` list                                         | A list of categories this entity belongs to.                                                                                                                                                                                                                                    |
| `yields`       |          | `string`                                              | The name of the entity which will be added to a robot's inventory when it executes grab or harvest on this entity. If omitted, the entity will simply yield itself.                                                                                                             |

#### Entity properties

The properties an entity may possess are listed below. Each entity may
possess any number of properties.

-   `unwalkable`: robots cannot `move` into a cell containing this
    entity. If they try, the `move` command will throw an exception.

-   `portable`: robots can pick this up using `grab` or `harvest`.
    Trying to execute `grab` or `harvest` on an entity that is not
    `portable` will throw an exception.

-   `growable`: when `harvest`ed, the entity will regrow from a seed.

-   `infinite`: when `grab`bed or `harvest`ed, the entity will
    immediately respawn.

-   `known`: robots know what this is without having to `scan` it first,
    hence it does not show up as a question mark.

#### Capabilities

Each capability enables the evaluation of execution of one or more
commands or language constructs. Rather than listing all possible
capabilities here, which would be annoying to keep up-to-date, see the
(automatically generated) [Commands cheat
sheet](https://github.com/swarm-game/swarm/wiki/Commands-Cheat-Sheet) on
the Swarm wiki.

### Combustion

Properties of entity combustion

| Key        | Default?     | Type                                                | Description                                                                                                                          |
|------------|--------------|-----------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| `duration` | `[100, 200]` | [range](#numeric-range "Link to object properties") | For combustible entities, a 2-tuple of integers specifying the minimum and maximum amount of time that the combustion shall persist. |
| `ignition` | `0.5`        | `number`                                            | Rate of ignition by a neighbor, per tick.                                                                                            |
| `product`  | `"ash"`      | `string` or `null`                                  | What entity, if any, is left over after combustion                                                                                   |

### Robot

Description of a robot in the Swarm game

| Key           | Default?    | Type                                                                                                                     | Description                                                                                                                                                                                                                                                                                                                                                                                                         |
|---------------|-------------|--------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `description` |             | `string`                                                                                                                 | A description of the robot. This is currently not used for much, other than scenario documentation.                                                                                                                                                                                                                                                                                                                 |
| `devices`     | `[]`        | `string` list                                                                                                            | A list of entity names which should be equipped as the robot's devices, i.e. entities providing capabilities to run commands and interpret language constructs.                                                                                                                                                                                                                                                     |
| `dir`         |             | `array` or [direction](#directions "Link to object properties")                                                          | An optional starting orientation of the robot, expressed as either (1) the name of a cardinal direction or (2) a vector. Every time the robot executes a `move` command, this vector will be added to its position. Typically, this is a unit vector in one of the four cardinal directions, although there is no particular reason that it has to be. When omitted, the robot's direction will be the zero vector. |
| `display`     | `"default"` | [display](#display "Link to object properties")                                                                          | Display information for the robot. If this field is omitted, the default robot display will be used.                                                                                                                                                                                                                                                                                                                |
| `heavy`       | `False`     | `boolean`                                                                                                                | Whether the robot is heavy. Heavy robots require `tank treads` to `move` (rather than just `treads` for other robots).                                                                                                                                                                                                                                                                                              |
| `inventory`   | `[]`        | [inventory](#inventory "Link to object properties")                                                                      | A list of `[count, entity name]` pairs, specifying the entities in the robot's starting inventory, and the number of each.                                                                                                                                                                                                                                                                                          |
| `loc`         |             | [cosmic-loc](#cosmic-location "Link to object properties") or [planar-loc](#planar-location "Link to object properties") | An optional starting location for the robot. If the `loc` field is specified, then a concrete robot will be created at the given location. If this field is omitted, then this robot record exists only as a template which can be referenced from a cell in the world palette. Concrete robots will then be created wherever the corresponding palette character is used in the world map.                         |
| `name`        |             | `string`                                                                                                                 | The name of the robot. This shows up in the list of robots in the game (`F2`), and is also how the robot will be referred to in the world palette.                                                                                                                                                                                                                                                                  |
| `program`     |             | `string`                                                                                                                 | This is the text of a Swarm program which the robot should initially run, and must be syntax- and type-error-free. If omitted, the robot will simply be idle.                                                                                                                                                                                                                                                       |
| `system`      | `False`     | `boolean`                                                                                                                | Whether the robot is a "system" robot. System robots can do anything, without regard for devices and capabilities. System robots are invisible by default.                                                                                                                                                                                                                                                          |
| `walkable`    |             | `object`                                                                                                                 | Blacklist/whitelist of walkable entities                                                                                                                                                                                                                                                                                                                                                                            |

#### Base robot

There must be at most one **base** robot in the world. Since concrete
robots can be created either via the `loc` attribute or via the map and
palette, use the following guide to ensure the base robot is the one you
intended:

1.  Always list the intended **base** as the first robot definition in
    your scenario.
2.  The first robot with a `loc` attribute will become the base, even if
    other robots are defined earlier.
3.  Without any located robots, if multiple robots are instantiated on
    the map from the first robot definition, the first robot in
    [row-major
    order](https://en.wikipedia.org/wiki/Row-_and_column-major_order)
    shall be the base.

### Cosmic location

Planar location plus subworld

| Key        | Default? | Type                                                       | Description      |
|------------|----------|------------------------------------------------------------|------------------|
| `loc`      |          | [planar-loc](#planar-location "Link to object properties") |                  |
| `subworld` |          | `string`                                                   | Name of subworld |

### World

Description of the world in the Swarm game

| Key          | Default? | Type                                                                 | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|--------------|----------|----------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `default`    |          | `string` list                                                        | Default world cell content                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| `dsl`        |          | `string`                                                             | A term in the Swarm world description DSL. The world it describes will be layered underneath the world described by the rest of the fields.                                                                                                                                                                                                                                                                                                                                                                                                               |
| `map`        | `""`     | `string`                                                             | A rectangular string, using characters from the palette, exactly specifying the contents of a rectangular portion of the world. Leading spaces are ignored. The rest of the world is either filled by the default cell, or by procedural generation otherwise. Note that this is optional; if omitted, the world will simply be filled with the default cell or procedurally generated.                                                                                                                                                                   |
| `name`       |          | `string`                                                             | Name of this subworld                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `offset`     | `False`  | `boolean`                                                            | Whether the base robot's position should be moved to the nearest "good" location, currently defined as a location near a `tree`, in a 16x16 patch which contains at least one each of `tree`, `copper ore`, `bit (0)`, `bit (1)`, `rock`, `lambda`, `water`, and `sand`. The classic scenario uses `offset: True` to make sure that the it is not unreasonably difficult to obtain necessary resources in the early game (see [code](https://github.com/swarm-game/swarm/blob/e06e04f622a3762a10e7c942c1cbd2c1e396144f/src/Swarm/Game/World/Gen.hs#L79)). |
| `palette`    |          | `object`                                                             | The palette maps single character keys to tuples representing contents of cells in the world, so that a world containing entities and robots can be drawn graphically. See [Cells](#cells) for the contents of the tuples representing a cell.                                                                                                                                                                                                                                                                                                            |
| `placements` |          | [placement](#placement "Link to object properties") list             | Structure placements. Earlier members may occlude later members of the list.                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| `portals`    |          | [portal](#portal "Link to object properties") list                   | A list of portal definitions that reference waypoints.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `scrollable` | `True`   | `boolean`                                                            | Whether players are allowed to scroll the world map.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `structures` |          | [named-structure](#named-structure "Link to object properties") list | Structure definitions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `upperleft`  | `[0, 0]` | `array`                                                              | A 2-tuple of `int` values specifying the `(x,y)` coordinates of the upper left corner of the map.                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `waypoints`  |          | [explicit-waypoint](#waypoint "Link to object properties") list      | Single-location waypoint definitions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

#### Cells

Each cell of the world is specified by a list of terrain, optional
entity and robots present (if any). For example, `[grass]`,
`[grass, tree]`, or `[grass, null, base]`.

-   The first (required) item specifies the terrain. Currently, valid
    terrain values are `stone`, `dirt`, `grass`, `ice`, or `blank`.

-   The second item (if present) specifies the name of an entity which
    should be present in the cell. This may be a built-in entity, or a
    custom entity specified in the `entities` section. `null` may be
    used to explicitly specify no entity in the cell.

-   The third item and later (if present) specifies the names of the
    robots which should be present in the cell. These must be names of
    robots specified in the `robots` section. A copy of each robot will
    be created at each location in the `map` where it is drawn.

    Although multiple robots may be in a single location in general,
    there is currently no way to specify more than one robot for a cell
    in the world description.

If a 1-tuple is used, it specifies a terrain value with no entity or
robot. A 2-tuple specifies a terrain value and entity, but no robot.

### Named structure

Structure definitions

| Key           | Default? | Type                                                      | Description                                                                                  |
|---------------|----------|-----------------------------------------------------------|----------------------------------------------------------------------------------------------|
| `description` |          | `string`                                                  | Description of this substructure                                                             |
| `name`        |          | `string`                                                  | Name of this substructure                                                                    |
| `recognize`   |          | [direction](#directions "Link to object properties") list | Orientations for which this structure participates in automatic recognition when constructed |
| `structure`   |          | [structure](#structure "Link to object properties")       |                                                                                              |

### Structure

Structure properties. Structures may opt-in to "automatic recognition"
for when they are constructed by a robot. There are certain limitations
on the shape and placement of such "recognizable" structures.

| Key          | Default? | Type                                                                 | Description                                                                    |
|--------------|----------|----------------------------------------------------------------------|--------------------------------------------------------------------------------|
| `map`        |          | `string`                                                             | Cell-based representation of the structure using palette entries               |
| `mask`       |          | `string`                                                             | A special palette character that indicates that map cell should be transparent |
| `palette`    |          | `object`                                                             | Structure properties                                                           |
| `placements` |          | [placement](#placement "Link to object properties") list             | Structure placements. Earlier members may occlude later members of the list.   |
| `structures` |          | [named-structure](#named-structure "Link to object properties") list | Nested structure definitions                                                   |
| `waypoints`  |          | [explicit-waypoint](#waypoint "Link to object properties") list      | Single-location waypoint definitions                                           |

### Placement

Structure placement

| Key      | Default? | Type                                                                   | Description                  |
|----------|----------|------------------------------------------------------------------------|------------------------------|
| `offset` |          | [planar-loc](#planar-location "Link to object properties")             |                              |
| `orient` |          | [structure-orient](#structure-orientation "Link to object properties") |                              |
| `src`    |          | `string`                                                               | Name of structure definition |

### Structure orientation

Structure orientation properties

| Key    | Default? | Type                                                 | Description |
|--------|----------|------------------------------------------------------|-------------|
| `flip` |          | `boolean`                                            |             |
| `up`   |          | [direction](#directions "Link to object properties") |             |

### Directions

| Member  |
|---------|
| `north` |
| `west`  |
| `south` |
| `east`  |

### Display

Swarm entity display. A display specifies how an entity or a robot
(robots are essentially special kinds of entities) is displayed in the
world. It consists of a key-value mapping described by the following
table.

| Key              | Default?      | Type                                                            | Description                                                                                                                                                                                                               |
|------------------|---------------|-----------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `attr`           | `"entity"`    | `string`                                                        | The name of the attribute that should be used to style the robot or entity. A list of currently valid attributes can be found [here](https://github.com/swarm-game/swarm/blob/main/src/Swarm/TUI/View/Attribute/Attr.hs). |
| `char`           | `" "`         | `string`                                                        | The default character that should be used to draw the robot or entity.                                                                                                                                                    |
| `curOrientation` |               | `array`                                                         | Currently unused                                                                                                                                                                                                          |
| `invisible`      | `False`       | `boolean`                                                       | Whether the entity or robot should be invisible. Invisible entities and robots are not drawn, but can still be interacted with in otherwise normal ways. System robots are by default invisible.                          |
| `orientationMap` | `fromList []` | [orientation-map](#orientation-map "Link to object properties") |                                                                                                                                                                                                                           |
| `priority`       | `1`           | `number`                                                        | When multiple entities and robots occupy the same cell, the one with the highest priority is drawn. By default, entities have priority `1`, and robots have priority `10`.                                                |

### Recipe

Recipe describes a process that takes some inputs and produces some
outputs, which robots can access using `make` and `drill`.

| Key        | Default? | Type                                                | Description                                                                                                                                                                                                                                                                                                                                                                                                         |
|------------|----------|-----------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `in`       |          | [inventory](#inventory "Link to object properties") | A list of ingredients consumed by the recipe. Each ingredient is a tuple consisting of an integer and an entity name, indicating the number of copies of the given entity that are needed.                                                                                                                                                                                                                          |
| `out`      |          | [inventory](#inventory "Link to object properties") | A list of outputs produced by the recipe. It is a list of `[count, entity name]` tuples just like `in`.                                                                                                                                                                                                                                                                                                             |
| `required` | `[]`     | [inventory](#inventory "Link to object properties") | A list of catalysts required by the recipe. They are neither consumed nor produced, but must be present in order for the recipe to be carried out. It is a list of \[count, entity name\] tuples just like in and out.                                                                                                                                                                                              |
| `time`     | `1`      | `number`                                            | The number of ticks the recipe takes to perform. For recipes which take more than 1 tick, the robot will wait for a number of ticks until the recipe is complete. For example, this is used for many drilling recipes.                                                                                                                                                                                              |
| `weight`   | `1`      | `number`                                            | Whenever there are multiple recipes that match the relevant criteria, one of them will be chosen at random, with probability proportional to their weights. For example, suppose there are two recipes that both output a widget, one with weight `1` and the other with weight `9`. When a robot executes `make "widget"`, the first recipe will be chosen 10% of the time, and the second recipe 90% of the time. |

### Inventory

List of [entity-count](#entity-count "Link to object properties")

### Entity count

One row in an inventory list

| Index | Type     | Description |
|-------|----------|-------------|
| `0`   | `number` | Quantity    |
| `1`   | `string` | Entity name |

### Waypoint

Explicit waypoint definition

| Key    | Default? | Type                                                       | Description   |
|--------|----------|------------------------------------------------------------|---------------|
| `loc`  |          | [planar-loc](#planar-location "Link to object properties") |               |
| `name` |          | `string`                                                   | Waypoint name |

### Objective

Scenario goals and their prerequisites. The top-level objectives field
contains a list of objectives that must be completed in sequence. Each
objective has a goal description and a condition.

| Key            | Default? | Type                                                      | Description                                                                                       |
|----------------|----------|-----------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| `condition`    |          | `string`                                                  | A swarm program that will be hypothetically run each tick to check if the condition is fulfilled. |
| `goal`         |          | `string` list                                             | The goal description as a list of paragraphs that the player can read.                            |
| `hidden`       |          | `boolean`                                                 | Whether this goal should be suppressed from the Goals dialog prior to achieving it                |
| `id`           |          | `string`                                                  | A short identifier for referencing as a prerequisite                                              |
| `optional`     |          | `boolean`                                                 | Whether completion of this objective is required to achieve a 'Win' of the scenario               |
| `prerequisite` |          | [prerequisite](#prerequisite "Link to object properties") |                                                                                                   |
| `teaser`       |          | `string`                                                  | A compact (2-3 word) summary of the goal                                                          |

### Orientation map

Mapping from cardinal directions to display characters

| Key     | Default? | Type     | Description |
|---------|----------|----------|-------------|
| `east`  |          | `string` |             |
| `north` |          | `string` |             |
| `south` |          | `string` |             |
| `west`  |          | `string` |             |

### Planar location

x and y coordinates of a location in a particular world

| Index | Type     | Description  |
|-------|----------|--------------|
| `0`   | `number` | X coordinate |
| `1`   | `number` | Y coordinate |

### Portal

Portal definition

| Key          | Default? | Type                                                    | Description                                               |
|--------------|----------|---------------------------------------------------------|-----------------------------------------------------------|
| `consistent` |          | `boolean`                                               | Whether this portal is spatially consistent across worlds |
| `entrance`   |          | `string`                                                | Name of entrance waypoint                                 |
| `exitInfo`   |          | [portal-exit](#portal-exit "Link to object properties") |                                                           |
| `reorient`   |          | `string`                                                | Passing through this portal changes a robot's orientation |

### Portal exit

Properties of a portal's exit

| Key            | Default? | Type     | Description           |
|----------------|----------|----------|-----------------------|
| `exit`         |          | `string` | Name of exit waypoint |
| `subworldName` |          | `string` | Name of exit subworld |

### Prerequisite

Prerequisite conditions for an objective.

### Numeric range

Min/max range of a value

| Index | Type     | Description |
|-------|----------|-------------|
| `0`   | `number` | minimum     |
| `1`   | `number` | maximum     |

### Terrain

Description of a terrain in the Swarm game

| Key           | Default? | Type     | Description                                                                                                                                                                                                               |
|---------------|----------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `attr`        |          | `string` | The name of the attribute that should be used to style the robot or entity. A list of currently valid attributes can be found [here](https://github.com/swarm-game/swarm/blob/main/src/Swarm/TUI/View/Attribute/Attr.hs). |
| `description` |          | `string` | A description of the terrain.                                                                                                                                                                                             |
| `name`        |          | `string` | The name of the terrain.                                                                                                                                                                                                  |
