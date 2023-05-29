# Design

The objective is to arrange the tiles in ascending (row-major) order.

We want to generate a random game upon starting the scenario.
However, [not all](https://en.wikipedia.org/wiki/15_puzzle#Solvability) permutations of tiles avail themselves to a solution.

Assuming that the empty tile is initially in the lower-right position, the criteria for a solvable puzzle is that the number of "inversions" is even.

So, to guarantee solvability we can perform a random shuffle of tiles, count the inversions, and then make a single swap of adjacent tiles if necessary. Swapping adjacent tiles will either
increase the inversions by one or decrease by one, both of which invert the parity.