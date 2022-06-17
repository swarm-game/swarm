// Quickly harvesting an entire forest in parallel using breadth-first
// search, with robots spawning more robots.  Fun, though not very practical
// in classic mode.

def repeat : int -> cmd () -> cmd () = \n.\c.
  if (n == 0)
    {}
    {c ; repeat (n-1) c}
end;
def while : cmd bool -> cmd () -> cmd () = \test.\c.
  b <- test;
  if b {c ; while test c} {}
end;
def getX : cmd int =
  pos <- whereami;
  return (fst pos);
end;
def getY : cmd int =
  pos <- whereami;
  return (snd pos);
end;
def gotoX : int -> cmd () = \tgt.
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
def gotoY : int -> cmd () = \tgt.
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
def goto : int -> int -> cmd () = \x. \y. gotoX x; gotoY y; gotoX x; gotoY y end;
def spawnfwd : {cmd ()} -> cmd () = \c.
   try {
     move;
     b <- isHere "tree";
     if b
       { build c; return () }
       {};
     turn back;
     move
   } { turn back }
end;
def clear : cmd () =
  grab;
  repeat 4 (
    spawnfwd {clear};
    turn left
  );
  goto 0 0;
  give base "tree";
  selfdestruct;
end;
def start : cmd robot = build {turn west; repeat 7 move; clear} end
