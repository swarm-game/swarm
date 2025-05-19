def λcase = \f. \g. \s. case s f g end
def λmatch = \f. \p. match p f end

def elif = \p.\t.\f. {if p t f} end;

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

tydef RobotState = [gave_win: Bool, said_log_missing: Bool, said_loc: (Int * Int)] end;

// setters
def set_gave_win         : Bool -> RobotState -> RobotState = \v.\s. [gave_win=v,          said_log_missing=s.said_log_missing, said_loc=s.said_loc] end;
def set_said_log_missing : Bool -> RobotState -> RobotState = \v.\s. [gave_win=s.gave_win, said_log_missing=v,                  said_loc=s.said_loc] end;
def set_said_loc  : (Int * Int) -> RobotState -> RobotState = \v.\s. [gave_win=s.gave_win, said_log_missing=s.said_log_missing, said_loc=v         ]end;

tydef RobotsStateList = rec l. Unit + (Actor * RobotState * l) end;

def emptyList = inl () end;

// At the beginning all robots can be given Win.
def defaultState: RobotState =
  [gave_win=False, said_log_missing=False, said_loc=(-100,-100)]
end;

def query : Actor -> RobotsStateList -> RobotState = \rob.\l.
  case l (\_. defaultState) (λmatch \r. λmatch \s.\tail.
    if (r == rob) {s} {query rob tail}
  )
end;

def update : Actor -> RobotState -> RobotsStateList -> RobotsStateList = \rob.\s.\l.
  case l (\_. inr (rob, s, emptyList)) (λmatch \nr. λmatch \ns. \tail.
    if (nr == rob) {
      inr (rob, s, tail)
    } {
      inr (nr, ns, update rob s tail)
    }
  )
end;

myLoc <- whereami;

def foldM : (rec l. Unit + a * l) -> b -> (b -> a -> Cmd b) -> Cmd b =
  \xs. \b. \f. case xs
    (\_. pure b)
    (λmatch \h.\t. b2 <- f b h; foldM t b2 f)
end

// Try to give a robot a Win, filtering out those that were already given a Win.
// The robot will also receive instructions, so it **must have a logger!**
def tryGive: Text -> RobotsStateList -> Cmd RobotsStateList = \msg. \ok.
  instant (
    rs <- meetAll;
    foldM rs ok (\stateList.\rob.
      let state = query rob stateList in
      robLoc <- as rob {whereami};
      hasLog <- as rob {try {log "test"; pure true} {pure false}};
      if (state.gave_win) {
        // log $ "skipping the robot " ++ format rob ++ "because it already has a Win";
        pure stateList
      }
      $elif (robLoc != myLoc && state.said_loc != robLoc) {
        log $ "the robot " ++ format rob ++ " is not in my cell";
        pure (update rob (set_said_loc robLoc state) stateList);
      }
      $elif (not hasLog && state.said_log_missing)
      {
        say $ "the robot " ++ format rob ++ " is missing a logger!";
        pure (update rob (set_said_log_missing true state) stateList)
      }
      { // else 
        try {
          reprogram rob { log msg; };
          log $ "successfully reprogrammed robot " ++ format rob;
          give rob "Win";
          log $ "successfully gave Win to robot " ++ format rob;
          pure (update rob (set_gave_win true state) stateList)
        } {
          log $ "the robot " ++ format rob ++ " is probably still active!";
          pure stateList
        };
      }
    )
  )
end;

// -------------------------------------------------------------------------
// RUN
// -------------------------------------------------------------------------

log "Hi, I am the system hint robot";
iterate emptyList (tryGive
  $ "Send a robot to `salvage` me and come back to"
  ++ " `give base \"Win\"`.  When the rescue robot stands"
  ++ " where I am and executes `salvage`, all my inventory"
  ++ " and logs will go to it, including the \"Win\". Once you"
  ++ " have brought the \"Win\" to your base, you will win!\n\n"
  ++ "NOTE: if you are still viewing me when I am salvaged,"
  ++ " you will be in for a surprise!  If this happens just"
  ++ " type `view base` to return to viewing your base."
)
