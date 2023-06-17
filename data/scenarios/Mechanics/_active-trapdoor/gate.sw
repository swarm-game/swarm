/**
This mechanic operates by polling.
Note that the polling is not naturally "throttled" since
the 'whereami' command is intangible (has zero duration).
So we insert 'wait' commands to be performance-friendly.
*/
def waitAndGate = \myCoords. \armed.
  basePos <- as base {whereami};

  if (basePos == myCoords) {
    if armed {} {
      // Warn the player that they have tripped the trap
      say "ka-chunk"
    };
    // The trapdoor is now armed. Wait to spring it.
    wait 1; // Throttle the polling
    waitAndGate myCoords true;
  } {
    if (armed) {
      place "boulder";
      // recursion ends
    } {
      // Wait to arm the trapdoor
      wait 1; // Throttle the polling
      waitAndGate myCoords false
    };
  };
  end;

myPos <- whereami;
waitAndGate myPos false;