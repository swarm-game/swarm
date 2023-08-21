### Scenario

Scenario for the swarm game

| Key            | Default? | Type                                                   | Description                                                                                                                                                                                                                                                                                                                                                |
|----------------|----------|--------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `attrs`        |          | `array`                                                | A list of local attribute definitions                                                                                                                                                                                                                                                                                                                      |
| `author`       |          | `string`                                               | The author of the scenario (optional). Typically this is a person's name, but it can be any string. It is displayed under the scenario description in the new game menu.                                                                                                                                                                                   |
| `creative`     | `False`  | `boolean`                                              | Whether the scenario should start out in creative mode.                                                                                                                                                                                                                                                                                                    |
| `description`  |          | `string`                                               | A short description of the scenario. This shows up next to the new game menu when the scenario is selected.                                                                                                                                                                                                                                                |
| `entities`     | `[]`     | [Object schema](#entities "Link to object properties") | An optional list of custom entities, to be used in addition to the built-in entities. See description of Entities.                                                                                                                                                                                                                                         |
| `known`        | `[]`     | `array`                                                | A list of names of standard or custom entities which should have the Known property added to them; that is, robots should know what they are without having to scan them.                                                                                                                                                                                  |
| `name`         |          | `string`                                               | The name of the scenario. For official scenarios, this is what shows up in the new game menu.                                                                                                                                                                                                                                                              |
| `objectives`   | `[]`     | `array`                                                | An optional list of objectives, aka winning conditions. The player has to complete the objectives in sequence to win. See the description of Objectives.                                                                                                                                                                                                   |
| `recipes`      | `[]`     | [Object schema](#recipes "Link to object properties")  | An optional list of custom recipes, to be used in addition to the built-in recipes. They can refer to built-in entities as well as custom entities. See description of Recipes.                                                                                                                                                                            |
| `robots`       |          | `array`                                                | A list of robots that will inhabit the world. See the description of Robots.                                                                                                                                                                                                                                                                               |
| `seed`         |          | `number`                                               | An optional seed that will be used to seed the random number generator. If a procedurally generated world is used, the seed hence determines the world. Hence, if the seed is specified, the procedurally generated world will be exactly the same every time, for every player. If omitted, a random seed will be used every time the scenario is loaded. |
| `solution`     |          | `string`                                               | The (optional) text of a Swarm program that, when run on the base robot, completes all the objectives. For scenarios which are officially part of the Swarm repository, such a solution will be tested as part of CI testing. For scenarios loaded directly from a file, any provided solution is simply ignored.                                          |
| `stepsPerTick` |          | `number`                                               | When present, this specifies the maximum number of CESK machine steps each robot is allowed to take per game tick. It is rather obscure and technical and only used in a few automated tests; most scenario authors should not need this.                                                                                                                  |
| `structures`   |          | `array`                                                | Structure definitions                                                                                                                                                                                                                                                                                                                                      |
| `subworlds`    |          | `array`                                                | A list of subworld definitions                                                                                                                                                                                                                                                                                                                             |
| `version`      |          | `number`                                               | The version number of the scenario schema. Currently, this should always be 1.                                                                                                                                                                                                                                                                             |
| `world`        |          | [Object schema](#world "Link to object properties")    |                                                                                                                                                                                                                                                                                                                                                            |

### Entities

Description of entities in the Swarm game

| Key | Default? | Type | Description |
|-----|----------|------|-------------|

### Explicit-waypoint

Explicit waypoint definition

| Key    | Default? | Type                                                     | Description   |
|--------|----------|----------------------------------------------------------|---------------|
| `loc`  |          | [Object schema](#planar-loc "Link to object properties") |               |
| `name` |          | `string`                                                 | Waypoint name |

### Recipes

How to make (or drill) entities in the Swarm game

| Key | Default? | Type | Description |
|-----|----------|------|-------------|

### Combustion

Properties of combustion

| Key        | Default? | Type            | Description                                                                                                                          |
|------------|----------|-----------------|--------------------------------------------------------------------------------------------------------------------------------------|
| `duration` |          | `array`         | For combustible entities, a 2-tuple of integers specifying the minimum and maximum amount of time that the combustion shall persist. |
| `ignition` | `0.5`    | `number`        | Rate of ignition by a neighbor, per tick.                                                                                            |
| `product`  | `"ash"`  | `string | null` | What entity, if any, is left over after combustion                                                                                   |

### Planar-loc

x and y coordinates of a location in a particular world

| Key | Default? | Type | Description |
|-----|----------|------|-------------|

### Cosmic-loc

Planar location plus subworld

| Key        | Default? | Type                                                     | Description      |
|------------|----------|----------------------------------------------------------|------------------|
| `loc`      |          | [Object schema](#planar-loc "Link to object properties") |                  |
| `subworld` |          | `string`                                                 | Name of subworld |

### Portal

Portal definition

| Key          | Default? | Type      | Description                                               |
|--------------|----------|-----------|-----------------------------------------------------------|
| `consistent` |          | `boolean` | Whether this portal is spatially consistent across worlds |
| `entrance`   |          | `string`  | Name of entrance waypoint                                 |
| `exitInfo`   |          | `object`  | Exit definition                                           |
| `reorient`   |          | `string`  | Passing through this portal changes a robot's orientation |

### Display

A display specifies how an entity or a robot (robots are essentially
special kinds of entities) is displayed in the world. It consists of a
key-value mapping described by the following table.

| Key              | Default?      | Type      | Description                                                                                                                                                                                                          |
|------------------|---------------|-----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `attr`           | `"entity"`    | `string`  | The name of the attribute that should be used to style the robot or entity. A list of currently valid attributes can be found at https://github.com/swarm-game/swarm/blob/main/src/Swarm/TUI/View/Attribute/Attr.hs. |
| `char`           | `" "`         | `string`  | The default character that should be used to draw the robot or entity.                                                                                                                                               |
| `curOrientation` |               | `array`   | Currently unused                                                                                                                                                                                                     |
| `invisible`      | `False`       | `boolean` | Whether the entity or robot should be invisible. Invisible entities and robots are not drawn, but can still be interacted with in otherwise normal ways. System robots are by default invisible.                     |
| `orientationMap` | `fromList []` | `object`  | Currently unused                                                                                                                                                                                                     |
| `priority`       | `1.0`         | `number`  | When multiple entities and robots occupy the same cell, the one with the highest priority is drawn. By default, entities have priority 1, and robots have priority 10.                                               |

### Objective

The top-level objectives field contains a list of objectives that must
be completed in sequence. Each objective has a goal description and a
condition.

| Key            | Default? | Type      | Description                                                                                       |
|----------------|----------|-----------|---------------------------------------------------------------------------------------------------|
| `condition`    |          | `string`  | A swarm program that will be hypothetically run each tick to check if the condition is fulfilled. |
| `goal`         |          | `array`   | The goal description as a list of paragraphs that the player can read.                            |
| `hidden`       |          | `boolean` | Whether this goal should be suppressed from the Goals dialog prior to achieving it                |
| `id`           |          | `string`  | A short identifier for referencing as a prerequisite                                              |
| `optional`     |          | `boolean` | Whether completion of this objective is required to achieve a 'Win' of the scenario               |
| `prerequisite` |          | `object`  |                                                                                                   |
| `teaser`       |          | `string`  | A compact (2-3 word) summary of the goal                                                          |

### Inventory

A list of \[count, entity name\] pairs, specifying the number of each
entity.

| Key | Default? | Type | Description |
|-----|----------|------|-------------|

### Attribute

Local attribute definitions

| Key     | Default? | Type     | Description           |
|---------|----------|----------|-----------------------|
| `bg`    |          | `string` | Background color      |
| `fg`    |          | `string` | Foreground color      |
| `name`  |          | `string` | Name of attribute     |
| `style` |          | `array`  | Style properties list |

### Entity

Description of an entity in the Swarm game

| Key            | Default? | Type                                                     | Description                                                                                                                                                                                                                                                                     |
|----------------|----------|----------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `capabilities` | `"[]"`   | `array`                                                  | A list of capabilities provided by entity, when it is equipped as a device. See Capabilities.                                                                                                                                                                                   |
| `combustion`   |          | [Object schema](#combustion "Link to object properties") | Properties of combustion.                                                                                                                                                                                                                                                       |
| `description`  |          | `array`                                                  | A description of the entity, as a list of paragraphs.                                                                                                                                                                                                                           |
| `display`      |          | [Object schema](#display "Link to object properties")    | Display information for the entity.                                                                                                                                                                                                                                             |
| `growth`       | `"null"` | `array`                                                  | For growable entities, a 2-tuple of integers specifying the minimum and maximum amount of time taken for one growth stage. The actual time for one growth stage will be chosen uniformly at random from this range; it takes two growth stages for an entity to be fully grown. |
| `name`         |          | `string`                                                 | The name of the entity. This is what will show up in the inventory and how the entity can be referred to.                                                                                                                                                                       |
| `orientation`  | `"null"` | `array`                                                  | A 2-tuple of integers specifying an orientation vector for the entity. Currently unused.                                                                                                                                                                                        |
| `plural`       | `"null"` | `string`                                                 | An explicit plural form of the name of the entity. If omitted, standard heuristics will be used for forming the English plural of its name.                                                                                                                                     |
| `properties`   | `"[]"`   | `array`                                                  | A list of properties of this entity. See Entity properties.                                                                                                                                                                                                                     |
| `yields`       | `"null"` | `string`                                                 | The name of the entity which will be added to a robot's inventory when it executes grab or harvest on this entity. If omitted, the entity will simply yield itself.                                                                                                             |

### Placement

Structure placement

| Key      | Default? | Type                                                     | Description                  |
|----------|----------|----------------------------------------------------------|------------------------------|
| `offset` |          | [Object schema](#planar-loc "Link to object properties") |                              |
| `orient` |          | `object`                                                 | Orientation of structure     |
| `src`    |          | `string`                                                 | Name of structure definition |

### World

Description of the world in the Swarm game

| Key          | Default?                  | Type      | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|--------------|---------------------------|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `default`    |                           | `array`   | Default world cell content                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| `dsl`        |                           | `string`  | A term in the Swarm world description DSL. The world it describes will be layered underneath the world described by the rest of the fields.                                                                                                                                                                                                                                                                                                                                               |
| `map`        | `""`                      | `string`  | A rectangular string, using characters from the palette, exactly specifying the contents of a rectangular portion of the world. Leading spaces are ignored. The rest of the world is either filled by the default cell, or by procedural generation otherwise. Note that this is optional; if omitted, the world will simply be filled with the default cell or procedurally generated.                                                                                                   |
| `name`       |                           | `string`  | Name of this subworld                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `offset`     | `False`                   | `boolean` | Whether the base robot's position should be moved to the nearest "good" location, currently defined as a location near a tree, in a 16x16 patch which contains at least one each of tree, copper ore, bit (0), bit (1), rock, lambda, water, and sand. The classic scenario uses offset: True to make sure that the it is not unreasonably difficult to obtain necessary resources in the early game. See https://github.com/swarm-game/swarm/blob/main/src/Swarm/Game/WorldGen.hs#L204 . |
| `palette`    | `fromList []`             | `object`  | The palette maps single character keys to tuples representing contents of cells in the world, so that a world containing entities and robots can be drawn graphically. See Cells for the contents of the tuples representing a cell.                                                                                                                                                                                                                                                      |
| `placements` |                           | `array`   | Structure placements                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `portals`    |                           | `array`   | A list of portal definitions that reference waypoints.                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `scrollable` | `True`                    | `boolean` | Whether players are allowed to scroll the world map.                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `structures` |                           | `array`   | Structure definitions                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `upperleft`  | `[Number 0.0,Number 0.0]` | `array`   | A 2-tuple of int values specifying the (x,y) coordinates of the upper left corner of the map.                                                                                                                                                                                                                                                                                                                                                                                             |
| `waypoints`  |                           | `array`   | Single-location waypoint definitions                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

### Structure

Structure definitions

| Key         | Default? | Type     | Description               |
|-------------|----------|----------|---------------------------|
| `name`      |          | `string` | Name of this substructure |
| `structure` |          | `object` | Structure properties      |

### Robot

Description of a robot in the Swarm game

| Key           | Default?                  | Type                                                                                                                | Description                                                                                                                                                                                                                                                                                                                                                                               |
|---------------|---------------------------|---------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `description` |                           | `string`                                                                                                            | A description of the robot. This is currently not used for much, other than scenario documentation.                                                                                                                                                                                                                                                                                       |
| `devices`     | `[]`                      | `array`                                                                                                             | A list of entity names which should be equipped as the robot's devices, i.e. entities providing capabilities to run commands and interpret language constructs.                                                                                                                                                                                                                           |
| `dir`         | `[Number 0.0,Number 0.0]` | `array`                                                                                                             | An optional starting orientation of the robot, expressed as a vector. Every time the robot executes a \`move\` command, this vector will be added to its position. Typically, this is a unit vector in one of the four cardinal directions, although there is no particular reason that it has to be. When omitted, the robot's direction will be the zero vector.                        |
| `display`     | `"default"`               | [Object schema](#display "Link to object properties")                                                               | Display information for the robot. If this field is omitted, the default robot display will be used.                                                                                                                                                                                                                                                                                      |
| `heavy`       | `False`                   | `boolean`                                                                                                           | Whether the robot is heavy. Heavy robots require tank treads to move (rather than just treads for other robots).                                                                                                                                                                                                                                                                          |
| `inventory`   | `[]`                      | [Object schema](#inventory "Link to object properties")                                                             | A list of \[count, entity name\] pairs, specifying the entities in the robot's starting inventory, and the number of each.                                                                                                                                                                                                                                                                |
| `loc`         |                           | `Object (fromList [("$ref",String "./cosmic-loc.json")]) | Object (fromList [("$ref",String "./planar-loc.json")])` | An optional starting location for the robot. If the loc field is specified, then a concrete robot will be created at the given location. If this field is omitted, then this robot record exists only as a template which can be referenced from a cell in the world palette. Concrete robots will then be created wherever the corresponding palette character is used in the world map. |
| `name`        |                           | `string`                                                                                                            | The name of the robot. This shows up in the list of robots in the game (F2), and is also how the robot will be referred to in the world palette.                                                                                                                                                                                                                                          |
| `program`     |                           | `string`                                                                                                            | This is the text of a Swarm program which the robot should initially run, and must be syntax- and type-error-free. If omitted, the robot will simply be idle.                                                                                                                                                                                                                             |
| `system`      | `False`                   | `boolean`                                                                                                           | Whether the robot is a "system" robot. System robots can do anything, without regard for devices and capabilities. System robots are invisible by default.                                                                                                                                                                                                                                |
| `unwalkable`  | `[]`                      | `array`                                                                                                             | A list of entities that this robot cannot walk across.                                                                                                                                                                                                                                                                                                                                    |
