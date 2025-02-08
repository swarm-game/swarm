def doN : Int -> Cmd a -> Cmd Unit = \n. \c.
  if (n == 0) {} {c; doN (n-1) c}
end

def abs : Int -> Int = \n.
  if (n < 0) {-n} {n}
end

// Go to the given absolute coordinates.  End facing east.
def goto : Int * Int -> Cmd Unit = \dest.
  cur <- whereami;
  let x = fst cur in
  let y = snd cur in
  let dx = fst dest - x in
  let dy = snd dest - y in
  if (dx < 0) {turn west} {turn east};
  doN (abs dx) move;
  if (dy < 0) {turn south} {turn north};
  doN (abs dy) move;
  turn east;
end

def liftA2 : (a -> b -> c) -> Cmd a -> Cmd b -> Cmd c = \f. \ca. \cb.
  a <- ca;
  b <- cb;
  pure (f a b)
end

def add : Cmd Int -> Cmd Int -> Cmd Int = liftA2 (\x. \y. x + y) end

def countCell : Cmd Int =
  s <- scan down;
  pure $ case s
    (\_. 0)
    (\t. if (t == "flower") {1} {0})
end

tydef List a = rec l. Unit + (a * l) end

def sum : List Int -> Int = \l.
  case l
    (\_. 0)
    (\cons. fst cons + sum (snd cons))
end

def for : Int -> (Int -> Cmd a) -> Cmd (List a) = \n. \k.
  if (n == 0) {pure (inl ())} {a <- k n; b <- for (n-1) k; pure (inr (a,b))}
end

def countRow : Int -> Cmd Int = \w.
  ns <- for (w-1) (\_. n <- countCell; move; pure n);
  last <- countCell;
  pure (sum ns + last)
end

def isEven : Int -> Bool = \n. (n / 2) * 2 == n end

def around : Dir -> Cmd Unit = \d. turn d; move; turn d end

// countFlowers (w,h) (x,y) counts the number of flowers
// in the w by h rectangle with lower-left corner at (x,y)
def countFlowers : Int * Int -> Int * Int -> Cmd Int = \size. \ll.
  goto ll;
  let w = fst size in
  let h = snd size in
  cnts <- for (h-1) (\i.
    cnt <- countRow w;
    if (isEven i) { around right } { around left };
    pure cnt
  );
  last <- countRow w;
  pure (sum cnts + last)
end

def acquire : Cmd Text =
  thing <- atomic (b <- isempty; if b {pure ""} {grab});
  if (thing == "") {acquire} {pure thing}
end

def countAndReport : Int * Int -> Int * Int -> Cmd Unit = \size. \ll.
  cnt <- countFlowers size ll;
  goto (0,0);
  paper <- acquire;
  let soFar = (read paper : Int) in
  erase paper;
  newPaper <- print "paper" (format (soFar + cnt));
  place newPaper;
end

def until : Cmd Bool -> Cmd a -> Cmd Unit = \test. \body.
  b <- test; if b {} {body; until test body}
end

def acquireFlower : Cmd Unit =
  until (ishere "flower") move; grab; pure ()
end

def go =
  for 4 (\r.
    for 3 (\c.
      build {countAndReport (40,10) (-59 + 40*(c-1), -19 + 10*(r-1))}
    )
  );
  print "paper" "0";
  turn left; move; place "paper: 0";
  wait 1024;
  acquireFlower;
  turn back;
  goto (20,0);
  res <- meet;
  case res (\_. pure ()) (\truelove. give truelove "flower")
end;

go;
