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
def allOK: robot -> bool = \rob.
  true
end;

// Try to give a robot a Win, filtering out those that were already given a Win.
// The robot will also receive instructions, so it **must have a logger!**
def tryGive: text -> (robot -> bool) -> int -> cmd (robot -> bool) = \msg.\f.\i.
  r <- try {
    robotNumbered i;
  } {
    log $ "could not find robot " ++ format i;
    return self
  };
  if (r != self && f r) {
    log $ "found the robot " ++ format i;
    l <- whereami;
    rl <- as r {whereami}; wait 1;  // WHY is this 'wait 1' required???
    if (l != rl) {
      log $ "the robot" ++ format i ++ "is not in my cell";
      return f;
    } {
      try {
        reprogram r { log msg; };
        log $ "successfully reprogrammed robot " ++ format i;
        give r "Win";
        log $ "successfully gave Win to robot " ++ format i;
      } {
        log $ "the robot " ++ format i ++ "is missing a logger!"
      };
      return (\rob. (rob != r && f rob));
    }
  } {
    log $ "skipping the robot " ++ format i;
    return f
  }
end;

// -------------------------------------------------------------------------
// RUN
// -------------------------------------------------------------------------

log "Hi, I am secret";
iterate allOK (foreachF 1 16 $ tryGive
  $ "Send a robot to `salvage` me and come back to `give base \"Win\"`.\n"
  ++ "When the rescue robot stands where I am and executes `salvage`,\n"
  ++ "all my inventory and logs will go to it, namely the \"Win\".\n"
  ++ "Once you have brought the \"Win\" to your base, you will win!"
)
