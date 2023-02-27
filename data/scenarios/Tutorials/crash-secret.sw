// A for cycle from start to end (excluded) that carries a state.
def foreachF = \s.\e.\com.\state.
  if (s >= e) {
    return state
  } {
    n <- com state s;
    foreachF (s+1) e com n
  }
end;

// An infinite while cycle that carries a state.
def iterate = \state.\com.
  n <- com state;
  iterate n com;
end;

// At the beginning all robots can be given Win.
def allOK: actor -> bool = \rob.
  true
end;

myLoc <- whereami;

// Try to give a robot a Win, filtering out those that were already given a Win.
// The robot will also receive instructions, so it **must have a logger!**
def tryGive: text -> (actor -> bool) -> cmd (actor -> bool) = \msg.
  // (b -> actor -> cmd b) -> b -> cmd b
  meetAll $ \f.\rob.
    if (not $ f rob) {
      log $ "skipping the robot " ++ format rob ++ "because it already has a Win";
      return f
    } {
      robLoc <- as rob {whereami};
      if (robLoc != myLoc) {
        log $ "the robot" ++ format rob ++ "is not in my cell";
        return f;
      } {
        try {
          reprogram rob { log msg; };
          log $ "successfully reprogrammed robot " ++ format rob;
          give rob "Win";
          log $ "successfully gave Win to robot " ++ format rob;
          return (\r. (rob != r && f rob));
        } {
          log $ "the robot " ++ format rob ++ "is missing a logger!";
          return f;
        };
        
      }
    }
end;

// -------------------------------------------------------------------------
// RUN
// -------------------------------------------------------------------------

log "Hi, I am secret";
iterate allOK (tryGive
  $ "Send a robot to `salvage` me and come back to `give base \"Win\"`.\n"
  ++ "When the rescue robot stands where I am and executes `salvage`,\n"
  ++ "all my inventory and logs will go to it, including the \"Win\".\n"
  ++ "Once you have brought the \"Win\" to your base, you will win!"
)
