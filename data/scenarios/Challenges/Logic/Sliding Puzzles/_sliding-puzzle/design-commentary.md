# Design

There are 15 sequential tiles and one blank space on a 4x4 grid.  The game's objective is to arrange the tiles in ascending (row-major) order. The location of the blank space is irrelevant to the win condition.

## Board generation

We want to generate a random game upon starting the scenario.  However, [not all](https://en.wikipedia.org/wiki/15_puzzle#Solvability) permutations of tiles avail themselves to a solution.

Assuming that the empty space is initially in the lower-right position, the criteria for a solvable puzzle is that the number of "inversions" is even.

So, to guarantee solvability we can perform a random shuffle of tiles, count the inversions, and then make a single swap of adjacent tiles if necessary. Swapping adjacent tiles will either increase the inversions by one or decrease by one, both of which toggle the parity.

## Puzzle interaction

The simplest way to implement interaction with the puzzle is by the `push` command. This requires no "monitoring" or board manipulation by system robots. The inherent rules of pushing along with decorative entities arranged along the board boundaries ensure that only legal moves are made. By withholding a "grabber", which precludes "placing" any items, the player is prevented from contaminating the board.

However, I decided to support a second method for sliding the puzzle tiles, by "drilling" the tile that is to be moved.  This actually takes a fair amount of programming for a system robot to facilitate.

## Handling marked tiles

Drilling a tile "marks" it for manipulation by the "maintenance" robot.  Three approaches were considered for handling marked tiles.

Note that there exist "legal" marks and "illegal" marks. One approach to handle "illegal" marks would be to define an inverted prerequisite goal that checks for an illegal mark, and would fail the scenario immediately.  However, to be more forgiving to the player, we instead would like to use a "maintenance" bot to revert illegal marks.

### 1. Neighbor stack

The first method to be implemented entailed positioning the maintenance bot on the blank tile and continually observing the state of its neighbors.  If any of these neighbors were to be "marked", the bot remembers what tile was there before the marking (using its recursion stack) and places this tile in the blank space. The bot then moves to the "marker" tile and removes it from the board.

This method can be somewhat fragile and will not detect illegal marks.

Also, due to limitations of the `instant` command, it can take multiple ticks to handle a marked tile. To ensure it is allowed time to finish, an "ink" entity is consumed each time the player marks a tile. The "ink" is replenished only after the maintenance bot finishes its work.

### 2. Index sum

The second method experimented with was to sum the (1-based) indices of the tiles each tick and subtract this total from 120 to determine which, if any, of the tiles had been marked. This has the advantages of both being *stateless* and able to detect illegal marks---that is, marks that were not made adjacent to the blank space. This method can detect at most one marked tile.

### 3. Bit masking

The third possible method is similar to the second w.r.t. statelessness and illegal move detection, but can actually detect any number of marked tiles. An integer bitmask is utilized, where each bit index corresponds to a tile index. Each tick, the maintenance bot visits each square on the board, marking a bit for each encountered tile index. After traversing the entire board, bits are "popped" from the mask to determine whether any maintenance operations need be performed.

## Solving

See this video: https://www.youtube.com/watch?v=P9Xib-dWlqU