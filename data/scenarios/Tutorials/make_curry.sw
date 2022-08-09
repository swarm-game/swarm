
// Part 2 of 'farming' tutorial: make curry
make "log"; make "log"; make "board"; make "board"; make "boat";
build {
  require "boat";
  turn right; move; move; move; grab; turn back; move; move; move;
  give base "water";
};
wait 16;
make "curry"
