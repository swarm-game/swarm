def x : Int -> Cmd a -> Cmd Unit = \n. \c.
  if (n == 0) {} {c; x (n-1) c}
end

def ifC: ∀ a. Cmd Bool -> {Cmd a} -> {Cmd a} -> Cmd a
  = \test. \then. \else.
  b <- test;
  if b then else
end

def while: ∀ a. Cmd Bool -> {Cmd a} -> Cmd Unit
  = \test. \body.
  ifC test {force body; while test body} {}
end

def for : Int -> (Int -> Cmd a) -> Cmd Unit = \n. \k.
  if (n == 0) {} {k n; for (n-1) k}
end

def harvestMay =
  e <- isempty;
  if e {} {harvest; return ()}
end

def harvestTrees =
  turn back; move; turn left; x 5 move;
  turn left;
  x 5 (x 10 (harvestMay; move); turn back; x 10 move; turn left; move; turn left);
  turn left; x 10 move; turn right; move
end

def getWater =
  turn back; x 3 move; turn left; move;
  x 32 grab;
  turn back; move; turn right; x 3 move
end

def getPaper =
  harvestTrees;
  while (has "tree") {make "log"};
  x 2 (make "board"); make "boat"; equip "boat";
  getWater; x 4 (make "paper")
end

def scanAt : Int -> Int -> Cmd (Unit + Text) = \h. \v.
  x h move; turn right; x v move;
  s <- scan down;
  turn back; x v move; turn left; x h move; turn back;
  return s
end

def atTerminal : Cmd a -> Cmd a = \c.
  x 12 move; turn left; x 2 move;
  a <- c;
  turn back; x 2 move; turn right; x 12 move; turn back;
  return a
end

def waitToPlace : Text -> Cmd Unit = \t.
  success <- atomic (b <- isempty; if b {place t} {}; return b);
  if success {} { watch down; wait 1024; waitToPlace t }
end

def go =
  getPaper;
  x 2 move; turn left; x 4 move;
  for 8 (\h.
    for 4 (\v.
      res <- scanAt (h-1) (v-1);
      case res
        (\_. return ())
        (\t. atTerminal (p <- print (format ((h-1,v-1),t)); waitToPlace p))
    )
  )
end

go;
