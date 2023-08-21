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

