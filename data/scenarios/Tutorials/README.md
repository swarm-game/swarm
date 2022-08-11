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

### New entities to get old ones

The `yields` key is a trick which can be used to hide `known` entities from player.
When the player `grab`s such entity the specified entity is yielded instead.

But `yields` is also useful for other cosmetic entities like "wavy water".

Note that entities like "known printer" and "wavy water" do not have recipes.

However, it does not solve everything. For example the "spruce" replacement for "tree"
needs to yield itself, so that the player does not plant the slow "tree".
