import "~swarm/lib/control"

def harvestMay =
  e <- isempty;
  if e {} {harvest; pure ()}
end

def harvestTrees =
  turn back; move; turn left; doN 5 move;
  turn left;
  doN 5 (doN 10 (harvestMay; move); turn back; doN 10 move; turn left; move; turn left);
  turn left; doN 10 move; turn right; move
end

def getWater =
  turn back; doN 3 move; turn left; move;
  doN 32 grab;
  turn back; move; turn right; doN 3 move
end

def getPaper =
  harvestTrees;
  while (has "tree") {make "log"};
  doN 2 (make "board"); make "boat"; equip "boat";
  getWater; doN 4 (make "paper")
end

def scanAt : Int -> Int -> Cmd (Unit + Text) = \h. \v.
  doN h move; turn right; doN v move;
  s <- scan down;
  turn back; doN v move; turn left; doN h move; turn back;
  pure s
end

def atTerminal : Cmd a -> Cmd a = \c.
  doN 12 move; turn left; doN 2 move;
  a <- c;
  turn back; doN 2 move; turn right; doN 12 move; turn back;
  pure a
end

def waitToPlace : Text -> Cmd Unit = \t.
  success <- atomic {b <- isempty; if b {place t} {}; pure b};
  if success {} { watch down; wait 1024; waitToPlace t }
end

def go =
  getPaper;
  doN 2 move; turn left; doN 4 move;
  for_ 8 (\h.
    for_ 4 (\v.
      res <- scanAt (h-1) (v-1);
      case res
        (\_. pure ())
        (\t. atTerminal (p <- print "paper" (format ((h-1,v-1),t)); waitToPlace p))
    )
  )
end
