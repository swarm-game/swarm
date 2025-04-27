def elif = \p. \t. \f. {if p t f} end

def foldM: (rec l. Unit + (a * l)) -> b -> (b -> a -> Cmd b) -> Cmd b 
  = \xs. \b. \f.
  case xs (\_. pure b) (\cons. b' <- f b (fst cons); foldM (snd cons) b' f)
end

// An infinite while cycle that carries a state.
def iterate = \state. \com. n <- com state; iterate n com end

// -------------------------------------------------------------------------
// Robot State - this allows us to skip some commands, so that this hont robot
// does not have to busy-wait to meet each robot
tydef RobotState = [gave_win: Bool, said_loc: Int * Int, said_log_missing: Bool] end

// setters
def set_gave_win: Bool -> RobotState -> RobotState 
  = \v. \s.
  [gave_win = v, said_loc = s.said_loc, said_log_missing = s.said_log_missing]
end

def set_said_log_missing: Bool -> RobotState -> RobotState 
  = \v. \s.
  [gave_win = s.gave_win, said_loc = s.said_loc, said_log_missing = v]
end

def set_said_loc: (Int * Int) -> RobotState -> RobotState 
  = \v. \s.
  [gave_win = s.gave_win, said_loc = v, said_log_missing = s.said_log_missing]
end

// At the beginning each robot can be given Win.
def defaultState: RobotState =
  [gave_win = false, said_loc = (-100, -100), said_log_missing = false]
end

// The list of met robots
tydef RobotsStateList = rec l. Unit + (Actor * (RobotState * l)) end

def emptyList = inl () end

def query: Actor -> RobotsStateList -> RobotState 
  = \rob. \l.
  case l (\_. defaultState) (
    \n. if (fst n == rob) {fst (snd n)} {query rob (snd (snd n))}
  )
end

def update: Actor -> RobotState -> RobotsStateList -> RobotsStateList 
  = \rob. \s. \l.
  case l (\_. inr (rob, s, emptyList)) (
    \n. let nr = fst n in
    let ns = fst $ snd n in
    let nnl = snd $ snd n in
    if (nr == rob) {inr (rob, s, nnl)} {inr (nr, ns, update rob s nnl)}
  )
end

// -------------------------------------------------------------------------
// Running state
tydef State = [can_wait: Bool, list: RobotsStateList] end

def updateList: Actor -> RobotState -> State -> State 
  = \rob. \rs. \state.
  [can_wait = true, list = update rob rs state.list]
end

// -------------------------------------------------------------------------
// Running state
myLoc <- whereami;

// Try to give a robot a Win, filtering out those that were already given a Win.
// The robot will also receive instructions, so it **must have a logger!**
def tryGive: Text -> State -> Cmd State 
  = \msg. \state.
  if (state.can_wait) {watch down; wait 2048} {};
  instant (
    rs <- meetall;
    foldM rs state (
      \stateAcc. \rob. let state = query rob stateAcc.list in
      robLoc <- as rob {whereami};
      hasLog <- as rob {try {log "test"; pure true} {pure false}};
      if (state.gave_win) {
        pure stateAcc
      }
      $ elif (robLoc != myLoc && state.said_loc != robLoc) {
        log ("the robot" ++ format rob ++ "is not in my cell");
        pure (updateList rob (set_said_loc robLoc state) stateAcc)
      }
      $ elif (not hasLog && state.said_log_missing) {
        say ("the robot " ++ format rob ++ "is missing a logger!");
        pure (updateList rob (set_said_log_missing true state) stateAcc)
      } // else
       {
        try {
          reprogram rob {log msg};
          log ("successfully reprogrammed robot " ++ format rob);
          give rob "Win";
          log ("successfully gave Win to robot " ++ format rob);
          pure (updateList rob (set_gave_win true state) stateAcc)
        } {
          log ("the robot " ++ format rob ++ "is probably still active!");
          pure [can_wait = false, list = stateAcc.list]
        }
      }
    )
  )
end

// -------------------------------------------------------------------------
// RUN
// -------------------------------------------------------------------------
log "Hi, I am the system hint robot";
iterate [can_wait=true, list=emptyList] (tryGive
  $ "Send a robot to `salvage` me and come back to"
  ++ " `give base \"Win\"`.  When the rescue robot stands"
  ++ " where I am and executes `salvage`, all my inventory"
  ++ " and logs will go to it, including the \"Win\". Once you"
  ++ " have brought the \"Win\" to your base, you will win!\n\n"
  ++ "NOTE: if you are still viewing me when I am salvaged,"
  ++ " you will be in for a surprise!  If this happens just"
  ++ " type `view base` to return to viewing your base."
)