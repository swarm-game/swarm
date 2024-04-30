Design Commentary
=================

The core idea of this scenario is in programmatically defining subsets of entities
that can be iterated over. Since "lists" don't exist, we abuse recipes to do this.

A large number of "flower" entities exist to be intentionally cumbersome to hand-code.
A recipe called "flower pouch" requires all of the "flowers" to `make` it.
This would allow one to test with a (`try`) whether they possess all of the flowers
(this is exploited by the goal condition-checking robot).
Additionally, one recipe per flower is defined so that the sequence of flowers
can be iterated through (i.e. a cyclic [`succ`](https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Enum.html#v:succ) function).

After the player obtains one of every flower, they can `make` a "flower pouch" that can be equipped.
This can then be used as a "drill" that will cycle through the flower varieties that are placed on the ground.

Instead of simply cycling through items, it is possible that the "flower pouch" could be used to create multiple copies of a single item, given availability of its predecessor in the cycle.
However, since the items are all "growable" anyway, this doesn't offer any advantage to the player that they didn't already have (since they were required to obtain one of every flower to obtain the pouch).

Code-generation is used to obtain a (pseudo-)constant-time lookup function from numeric index to flower name.
For system robots, this is used to iterate through the flower collection by number.
For a player, one could construct a O(N)-time lookup function from numeric index to flower name by drilling through
the sequence of successors, starting at some reference flower.

Although not necessitated by the scenario, a player could construct a constant-time lookup from "flower name" to numeric index by pre-populating their inventory with the desired number of each flower.

A potentially interesting mechanic for a future scenario may be representing sparse subsets in binary.
Given consecutive numeric indices associated with each of the N flowers, one could use N-bit numbers
to define various subsets of the flowers. Swarm supports arbitrarily large integers that facilitate this
technique for more than 32 or even 64 members.