# World DSL guide

Swarm has a built-in, special-purpose *domain specific language* for
describing worlds (*i.e.* the terrain, entities, other robots,
etc. which the player sees on the map when a scenario is loaded).  It
is somewhat bare bones at the moment, but will continue to develop.

## Overview

The basic idea of the world DSL is ultimately to describe a *function*
which specifies a *cell* value for every coordinate.  In addition,
this is done in such a way that all randomness used for procedural
generation (if any) is ultimately derived from a single seed, so world
generation is actually 100% deterministic and reproducible by design.

## Types

- There are four base types: `bool`, `int`, `float`, and
  `cell`.
  - `bool` values are written `true` and `false`.
  - `int` values are written like `3` or `-5`.
  - `float` values are written like `3.2` or `0.0`.  Note that `0` is
    always an `int`, and `0.0` is always a `float`.   If you use `0`
    as an argument to a function expecting a `float`, it is a type
    error!  This may be slightly annoying but it keeps typechecking
    much simpler.
  - `cell` values describe the contents of a world cell (terrain,
    entity, etc.), and are explained in more detail below.

- In addition, if `b` is a base type, `World b` is a type representing a
  "world function" which specifies a value of type `b` at every
  coordinate in its domain.

- Any base type `b` is a subtype of `World b`; that is, a value of
  type `b` may be used anywhere a `World b` is expected, or, put
  another way, any `b` may be automatically "promoted" to a `World b`.
  Semantically, a single value of a base type may be promoted to an
  infinite, constant world function which returns that single value at
  every coordinate.

- In general, any function of type `b1 -> ... -> bn` where the `bi`
  are all base types may be "lifted" to have type `World b1 -> ... ->
  World bn`, which means the function will act "coordinatewise",
  i.e. like a giant 2D `zipWith`.

    - For example, the `<` operator has a type like `integer ->
      integer -> bool` but that means it could also have type `World
      integer -> World integer -> World bool`.

## Syntax

Comments are specified by `//` (single line) or `/* ... */`
(multi-line).

Identifiers consist of any non-empty sequence of letters, digits,
underscores, or single quotes, but must begin with a letter or
underscore.

The extended BNF syntax `S*,` denotes the syntax `S` repeated zero
or more times, separated by `,`.  Likewise, `S+,` denotes one or more
repetitions of `S` separated by `,`.

```
<int> ::= integer literal
<float> ::= floating-point literal
<ident> ::= any non-reserved identifier
<nonquote> ::= any character other than double quote '"'

<atom> ::=
    <int>
  | <float>
  | 'true' | 'false'
  | <cell>
  | <ident>
  | 'seed'
  | 'x' | 'y'
  | 'hash'
  | 'if' <exp> 'then' <exp> 'else' <exp>
  | 'perlin' <atom> <atom> <atom> <atom>
  | 'abs' <atom>
  | 'let' (<ident> '=' <exp>)*, 'in' <exp>
  | 'overlay' '[' <exp>+, ']'
  | 'mask' <atom> <atom>
  | '"' <nonquote>+ '"'
  | '(' <exp> ')'

<cell> ::= '{' <item>*, '}
<item> ::= <name> | <tag> ':' <name>
<tag> ::= 'terrain' | 'entity' | 'robot'
<name> ::= 'erase' | <nameChar>*
<nameChar> ::= any single character besides ',', '}', or ']'

<exp> ::=
  | <atom>
  | <prefix-op> <atom>
  | <atom> <infix-op> <atom>

<prefix-op> ::= 'not' | '-'

// Infix operators are listed below by precedence, highest precedence
// first.  Operators listed on the same line share the same precedence.

<infix-op> ::=
    '*' | '/' | '%'
  | '+' | '-' | '<>'
  | '==' | '/=' | '<' | '<=' | '>' | '>='
  | '&&'
  | '||'
```

## Cells

A *cell* specifies the contents of a specific location in the world,
or any information associated with that location.  Currently, a cell
value consists of:

- An optional terrain value
- An optional entity
- A list of robots

More may be added in the future; note also that currently the list of
robots is not accessible via the DSL.

Cells have a monoid structure:

- The empty cell has no terrain, no entity, and an empty list of
  robots, and can be written `{}`.
- To combine two cells, we:
    - Take the last non-empty terrain value
    - Take the last non-null entity
    - Concatenate the robot lists

The basic syntax for a cell value is either
- `{terrain: <name>}` which specifies a cell with terrain given by
  `<name>`, no entity, and no robots
- `{entity: <name>}` which specifies a cell with empty terrain, an
  entity given by `<name>`, and no robots

Optionally, the `terrain` or `entity` tag (and colon) may be omitted,
for example, `{dirt}` or `{tree}`.  In this case the parser will try
reading the given name first as a terrain value, then as an entity,
and the first one that works will be chosen.

Multiple (optionally tagged) names may be written separated by commas
inside the curly braces, which is syntax sugar for combining multiple
values via the monoid combining operation.  For example, `{dirt,
entity: tree}` is equivalent to `{dirt} <> {entity: tree}`, and
specifies a cell containing `dirt` terrain and a `tree` entity.

There is also a special `erase` value for entities, which acts as an
annihilator (like 0 under multiplication).  That is, for combining entities,
- `null <> e = e <> null = e`
- `erase <> e = e <> erase = erase`
- Otherwise, `e1 <> e2 = e2`

`erase` can be used when a previous layer specified an entity but in a
subsequent layer we want the cell to be empty.  For example, perhaps a
first layer specified a default entity (say, `water`) everywhere, but
we want to selectively overwrite this default with not only other
entities but also some empty cells.

## Typechecking and semantics

- Boolean, arithmetic, and comparison operators are standard.
    - Note that arithmetic and comparison operators are overloaded to
      work on either ints or floats
    - The division operator '/' denotes either floating-point or integer
      division depending on the type of its arguments.
- `if ... then ... else ...` is standard.
- `let ... in ...` is standard.
- The `<>` operator combines `cell` values according to their
  semigroup structure.
- The special `seed : int` variable contains the value of the world seed.
- The special `x : World int` and `y : World int` variables always
  contain the current coordinate's `x` or `y` value, respectively.
- The special `hash : World int` variable contains a (non-coherent)
  hash of the current coordinates.
- `overlay [e1, e2, ...]` layers `e1` on the bottom, `e2` on top of
  that, etc., using the semigroup structure for world functions.
- `perlin s o k p` creates a Perlin noise function, which associates a
   floating-point value on the interval [-1,1] to every coordinate in
   a way that is random yet continuous (i.e. nearby coordinates have
   close floating-point values).  The four parameters represent seed,
   octaves, scale, and persistence. For an explanation of how these
   parameters affect the resulting noise function, see
   https://libnoise.sourceforge.net/glossary/index.html#perlinnoise
- `mask b e` takes the value of `e` where `b` is true, and is empty
  elsewhere.
- `"foo"` imports the DSL term in `worlds/foo.world`.
