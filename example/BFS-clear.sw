(def repeat : int -> cmd () -> cmd () = \n.\c.
  if (n == 0)
    {}
    {c ; repeat (n-1) c}
);
(def gotoX : int -> cmd () = \tgt. {
  cur <- getX;
  if (cur == tgt)
    {}
    (if (cur < tgt)
       {turn east}
       {turn west};
     move;
     new <- getX;
     if (new == cur)
       {turn south; move}
       {}
    )
});
(def gotoY : int -> cmd () = \tgt. {
  cur <- getY;
  if (cur == tgt)
    {}
    (if (cur < tgt)
       {turn north}
       {turn south};
     move;
     new <- getY;
     if (new == cur)
       {turn east; move}
       {};
     gotoY tgt
    )
});
(def goto : int -> int -> cmd () = \x. \y. gotoX x; gotoY y; gotoX x; gotoY y);
(def spawnfwd : cmd () -> cmd () = \c.
   move;
   b <- isHere "tree";
   if b
     { build "s" c; return () }
     {};
   turn back;
   move
);
(def clear : cmd () =
  grab;
  repeat 4 {
    spawnfwd clear;
    turn left
  };
  goto 0 0;
  give "base" "tree"
)
