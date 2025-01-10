def ifC: ∀ a. Cmd Bool -> {Cmd a} -> {Cmd a} -> Cmd a
  = \test. \then. \else.
  b <- test;
  if b then else
end

def while: ∀ a. Cmd Bool -> {Cmd a} -> Cmd Unit
  = \test. \body.
  ifC test {force body; while test body} {}
end

def forever: ∀ a b. {Cmd a} -> Cmd b = \c. force c; forever c end

def notC : Cmd Bool -> Cmd Bool = \c.
  b <- c; pure (not b)
end

def or : Cmd Bool -> Cmd Bool -> Cmd Bool = \c1. \c2.
  ifC c1 {pure true} {c2}
end

def followTrack : Cmd Unit =
  move;
  while (or (isHere "track") (isHere "mountain")) { move };
  turn back;
end

def pickup : Cmd Text =
  atomic (b <- isempty; if b {pure ""} {grab});
end

def dropoff : Text -> Cmd Bool = \thing.
  atomic (b <- isempty; if b {place thing} {}; pure b)
end

def deliver : Text -> Cmd Unit = \thing.
  move;
  followTrack;
  if (thing == "") {}
  {
    while (notC (dropoff thing)) { followTrack; followTrack }
  };
end

def go = forever {followTrack; thing <- pickup; deliver thing} end

go;
