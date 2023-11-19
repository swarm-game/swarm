Arbitrage
==============

The scenario has two stages:
1. Gain paperclips by increments of 1
2. Gain paperclips by increments of 10

Discovery of the trading path is straightforward.

The path of the first stage is suggested both by the terrain and in the numbering of shops.  The second-stage path is suggested more obliquely by goal text when enough paperclips have been accrued for the first exchange on on that path.

Furthermore, the recipes for a given shop may be viewed by selecting it on the inventory pane. A shop appears in the inventory if it is scanned or traded with. A shop may also appear in the recipe of an item yielded by a previous shop.

## Implementation aspects

The `drill` command is useful in that it does not require the player to specify the inputs and outputs for a particular exchange; they are determined simply by recipes and the player's current inventory.

This behavior does introduce caveats to the scenario design, however.

### Different routes

It is important to ensure that two different routes offer the two different paperclip exchanges, due to the behavior of the `drill` command.

* If a single shop offered two different trades at different paperclip quantities, sometimes the lesser-paperclip trade would be selected randomly.
* Even with two different shops offering the different paperclip exchanges, if they are on the same route, during the "second stage" (higher-quantity) traversals, the lesser-quantity paperclips exchange will sometimes be made even if that shop offers a trade not involving paperclips.

This is why the second stage (i.e. mirrored) route excludes the shop (`A`) in which the lower-quantity paperclip exchange is possible.

## Possible extensions/variations

### Exponential progression

The progression in terms of paperclip quantity is linear, other than the bump in constant factor at the second stage. It may be interesting to design an exponential progression, i.e. where the value doubles in each exchange.

See:
* https://en.wikipedia.org/wiki/Straw_Millionaire
* https://en.wikipedia.org/wiki/One_red_paperclip

### Route determination

In this scenario, the trading route is linear; there is a single, obvious next trade to make at each step.  A variation may be to have forking routes, wherein the player has multiple choices for where to trade in a given item, yielding two different possible outputs. This may entail more effort to optimize the route.
