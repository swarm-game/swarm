// Solution to 'farming.yaml' tutorial challenge

// Part 1: farm 256 lambdas
def tR = turn right end;
def tL = turn left end;
def tB = turn back end;
def forever = \c. c ; forever c end;
def ifC : cmd bool -> {cmd a} -> {cmd a} -> cmd a = \test. \then. \else.
  b <- test; if b then else
end;
def while : cmd bool -> {cmd a} -> cmd () = \test. \body.
  ifC test {force body ; while test body} {}
end;
def giveall : robot -> string -> cmd () = \r. \thing.
  while (has thing) {give r thing}
end;
def x4 = \c. c; c; c; c end;
def m4 = x4 move end;
def x12 = \c. x4 (c;c;c) end;
def m12 = x12 move end;
def next_row = tB; m12; tL; move; tL end;
def plant_field : string -> cmd () = \thing.
  log "planting";
  x4 (
    x12 (move; place thing; harvest);
    next_row
  )
end;
def harvest_field : string -> cmd () = \thing.
  x4 (
    x12 (move; ifC (ishere thing) {harvest; return ()} {});
    next_row
  );
  tL; m4; tR
end;
def harvest_lambdas =
  forever (
    tB; move; tR; harvest_field "lambda"; tR; move; giveall base "lambda"
  )
end;
build {
  require "treads"; require "harvester"; require "logger";
  require "lambda"; require "branch predictor";             // #540
  require 1 "lambda";
  tB; move; tR; plant_field "lambda";
};
build {
  require "treads"; require "harvester"; require "logger"; require "scanner";
  require "grabber";
  require "lambda"; require "branch predictor"; require "strange loop";  // #540
  harvest_lambdas
}
