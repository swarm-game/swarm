import "../../../../lib/control"

def followTrack : Cmd Unit =
  move;
  while (orC (isHere "track") (isHere "mountain")) { move };
  turn back;
end

def pickup : Cmd Text =
  atomic {b <- isempty; if b {pure ""} {grab}};
end

def dropoff : Text -> Cmd Bool = \thing.
  atomic {b <- isempty; if b {place thing} {}; pure b}
end

def deliver : Text -> Cmd Unit = \thing.
  move;
  followTrack;
  if (thing == "") {}
  {
    while (notC (dropoff thing)) { followTrack; followTrack }
  };
end

def go = forever (followTrack; thing <- pickup; deliver thing) end
