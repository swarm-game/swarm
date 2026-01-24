
// Part 2 of 'farming' tutorial: make curry
def go =
  make "log"; make "log"; make "board"; make "board"; make "boat";
  build {
    require "boat";
    move; move; move; turn right; move; move; move; grab;
    turn back; move; move; move; turn left; move; move; move;
    give base "water";
  };
  wait 24;
  make "curry"
end
