import "~swarm/lib/control"
import "~swarm/lib/scan"

def followTrack : Cmd Unit =
  move;
  while (orC (ishere "track") (ishere "mountain")) { move };
  turn back;
end

def pickup : Cmd Text =
  atomic {h <- scan down; if (h == "") {pure ""} {grab}};
end

def dropoff : Text -> Cmd Bool = \thing.
  atomic {h <- scan down; if (h == "") {place thing; pure true} {pure false}}
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
