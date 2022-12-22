def forever : cmd unit -> cmd unit = \c. c ; forever c end

def repeat : int -> cmd unit -> cmd unit =
  \n. \c. if (n == 0) {} {c ; repeat (n-1) c}
end

def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def abs = \n. if (n < 0) {-n} {n} end

def randdir : cmd dir =
  d <- random 4;
  return (
    if (d == 0) {north}
    $ elif (d == 1) {east}
    $ elif (d == 2) {south}
    $ else {west}
  )
end

def chooseWait : cmd int =
  t <- random (16*2);
  return (16 + t)
end

def wander =
  d <- randdir;
  turn d;
  dist <- random 2;
  try {repeat dist move} {};
  r <- random 5;
  if (r == 0) { say "meow" } {}
end

def disappointed = \cat. say "meow??"; cat end

def follow : cmd unit -> actor -> cmd unit = \cat. \r.
  rLoc <- as r {whereami};
  myLoc <- whereami;
  let dx = fst rLoc - fst myLoc in
  let dy = snd rLoc - snd myLoc in
  if (abs dx > abs dy)
  { if (dx < 0) {turn west} {turn east} }
  { if (dy < 0) {turn south} {turn north} };
  if (abs dx != 0 || abs dy != 0) {try { move } { disappointed cat }} {};
  wait 4;
  follow cat r
end

def love = \cat.
  say "purr";
  fishGiver <- meet;
  case fishGiver
    (\_. disappointed cat)
    (\r. follow cat r)
end

def cat = \start. \fishCount. \waitTime.
  if (waitTime == 0) { wander; start } { wait 1 };
  n <- count "fish";
  if (n > fishCount)
  { say "yum!";
    if (n >= 3) { love start } { cat start n (waitTime - 1) }
  }
  { cat start fishCount (waitTime - 1) }
end

def startCat =
  n <- count "fish";
  w <- chooseWait;
  cat startCat n w
end;

startCat
