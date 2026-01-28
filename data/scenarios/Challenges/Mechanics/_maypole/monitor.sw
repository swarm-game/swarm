import "~swarm/lib/control"
import "~swarm/lib/arith"

/*
Quadrants are numbered counter-clockwise, staring in the northeast:
        |
     1  |  0
   _____|_____
        |
     2  |  3
        |
This is same as the standard graph quadrants in mathematics, except
for 0-based numbering rather than 1-based.
*/
def getQuadrant : (Int * Int) -> (Int * Int) -> Int =
    λmatch \baseX. \baseY.
    λmatch \myX. \myY.

    let isUp = myY < baseY in
    let isRight = myX < baseX in

    if (isUp && isRight) {0}
    $ elif (isUp) {1}
    $ elif (isRight) {3}
    $ else {2}
    end;

/*
Possible edge case:
----
If the difference in quadrants is 2, then either
the monitor is not making observations with high
enough frequency, or the target has teleported
diagonally.
Either way, the direction of rotation is ambiguous,
so we should make the increment zero.
*/
def getQuadrantIncrement = \oldQuadrant. \newQuadrant.

    let rawDifference = newQuadrant - oldQuadrant in
    let difference = if (abs rawDifference == 3) {-rawDifference} {rawDifference} in

    if (difference > 0) {1}
    $ elif (difference < 0) {-1}
    $ else {0}
    end;

def getCurrentQuadrant : (Int * Int) -> Cmd Int = \myLoc.
  baseLoc <- as base {whereami};
  pure $ getQuadrant baseLoc myLoc;
  end;

def checkNewQuadrant = \myLoc. \prevQuadrant. \quadrantTraversalCount.
  currentQuadrant <- getCurrentQuadrant myLoc;
  let changeCount = getQuadrantIncrement prevQuadrant currentQuadrant in
  let newQuadrantCount = quadrantTraversalCount + changeCount in

  if (changeCount != 0) {
    swap $ "maypole " ++ format currentQuadrant;
    pure ();
  } {};
  pure (currentQuadrant, newQuadrantCount);
  end;

/*
Although conceivably the current quadrant could
be derived from the quadrant traversal count,
since it is possible to start "traversing" from
any quadrant we just keep track of them separately.

Also, the edge case for disregarding "diagonal" teleportation
means that the traversal number could get out of sync
with the absolute quadrant index.
*/
def monitorAngle : (Int * Int) -> Int -> Int -> Int -> Cmd Unit =
    \myLoc. \targetQuadrantCount. \prevQuadrant. \quadrantTraversalCount.
  result <- instant {checkNewQuadrant myLoc prevQuadrant quadrantTraversalCount};
  match result \currentQuadrant. \newQuadrantCount.

  if (newQuadrantCount < targetQuadrantCount) {
    monitorAngle myLoc targetQuadrantCount currentQuadrant newQuadrantCount;
  } {
    create "dizzy";
    swap "bitcoin";
    pure ();
  }
  end;

def go =
  myLoc <- whereami;
  currentQuadrant <- getCurrentQuadrant myLoc;
  monitorAngle myLoc 12 currentQuadrant 0;
  end;

def monitor = go; selfdestruct end
