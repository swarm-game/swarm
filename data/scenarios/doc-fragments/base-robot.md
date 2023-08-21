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
