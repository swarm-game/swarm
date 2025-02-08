// A for cycle from start to end (excluded) that carries a state.
def foreachF = \s.\e.\com.\state.
  if (s >= e) {
    pure state
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
def allOK: Actor -> Bool = \rob.
  true
end;

myLoc <- whereami;

def foldM : (rec l. Unit + a * l) -> b -> (b -> a -> Cmd b) -> Cmd b =
  \xs. \b. \f. case xs
    (\_. pure b)
    (\cons. b' <- f b (fst cons); foldM (snd cons) b' f)
end

// Try to give a robot a Win, filtering out those that were already given a Win.
// The robot will also receive instructions, so it **must have a logger!**
def tryGive: Text -> (Actor -> Bool) -> Cmd (Actor -> Bool) = \msg. \ok.
  rs <- meetAll;
  foldM rs ok $ \f.\rob.
    if (not $ f rob) {
      log $ "skipping the robot " ++ format rob ++ "because it already has a Win";
      pure f
    } {
      robLoc <- as rob {whereami};
      if (robLoc != myLoc) {
        log $ "the robot" ++ format rob ++ "is not in my cell";
        pure f;
      } {
        try {
          reprogram rob { log msg; };
          log $ "successfully reprogrammed robot " ++ format rob;
          give rob "Win";
          log $ "successfully gave Win to robot " ++ format rob;
          pure (\r. (rob != r && f rob));
        } {
          log $ "the robot " ++ format rob ++ "is missing a logger!";
          pure f;
        };
      }
    }
end;

// -------------------------------------------------------------------------
// RUN
// -------------------------------------------------------------------------

log "Hi, I am secret";
iterate allOK (tryGive
  $ "Send a robot to `salvage` me and come back to\n"
  ++ "`give base \"Win\".  When the rescue robot stands\n"
  ++ "where I am and executes `salvage`, all my inventory\n"
  ++ "and logs will go to it, including the \"Win\". Once you\n"
  ++ "have brought the \"Win\" to your base, you will win!\n\n"
  ++ "NOTE: if you are still viewing me when I am salvaged,\n"
  ++ "you will be in for a surprise!  If this happens just\n"
  ++ "type `view base` to return to viewing your base."
)
