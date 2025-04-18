// Quickly harvesting an entire forest in parallel using breadth-first
// search, with robots spawning more robots.  Fun, though not very practical
// in classic mode.

def repeat : Int -> Cmd Unit -> Cmd Unit = \n.\c.
  if (n == 0)
    {}
    {c ; repeat (n-1) c}
end;
def while : Cmd Bool -> Cmd Unit -> Cmd Unit = \test.\c.
  b <- test;
  if b {c ; while test c} {}
end;
def getX : Cmd Int =
  pos <- whereami;
  pure (match pos \x. \_. x);
end;
def getY : Cmd Int =
  pos <- whereami;
  pure (match pos \_. \y. y);
end;
def gotoX : Int -> Cmd Unit = \tgt.
  cur <- getX;
  if (cur == tgt)
    {}
    {if (cur < tgt)
       {turn east}
       {turn west};
     try {move} {turn south; move};
     gotoX tgt
    }
end;
def gotoY : Int -> Cmd Unit = \tgt.
  cur <- getY;
  if (cur == tgt)
    {}
    {if (cur < tgt)
       {turn north}
       {turn south};
     try {move} {turn east; move};
     gotoY tgt
    }
end;
def goto : Int -> Int -> Cmd Unit = \x. \y. gotoX x; gotoY y; gotoX x; gotoY y end;
def spawnfwd : {Cmd Unit} -> Cmd Unit = \c.
   try {
     move;
     b <- isHere "tree";
     if b
       { build c; pure () }
       {};
     turn back;
     move
   } { turn back }
end;
def clear : Cmd Unit =
  grab;
  repeat 4 (
    spawnfwd {clear};
    turn left
  );
  goto 0 0;
  give base "tree";
  selfdestruct;
end;
def start : Cmd Actor = build {turn west; repeat 7 move; clear} end
