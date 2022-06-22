# Tutorial design

Here are some notes about how the tutorial is designed.

## Outline

In the first few challenges, the starting robot moves and does it all itself.

After the basics are understood, start using the build command to send out robots instead.

Finally, move into the open world and guide through obtaining ingredients and crafting.


## Tips and tricks

### Logger

It is important that at least the base robot has a `logger` device installed.
Without it, the players will not see the errors and will be very confused.

### Goal

Until goal is separate from entity, we name it "Goal" so that it is lexicographically
first, since all other entites use lowercase names.

That way it is the first selected entity and its description is visible to the players.