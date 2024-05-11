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
