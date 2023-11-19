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
