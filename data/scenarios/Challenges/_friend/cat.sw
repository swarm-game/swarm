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

def wander =
  n <- random (16*4);
  wait (16*2 + n);
  d <- randdir;
  turn d;
  dist <- random 2;
  try {repeat dist move} {};
  r <- random 5;
  if (r == 0) { say "meow" } {}
end;

def disappointed = \cat.
  say "meow??";
  n <- count "fish";
  cat n
end

def follow : (int -> cmd unit) -> actor -> cmd unit = \cat. \r.
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
    (\_. say "meow??"; n <- count "fish"; cat n)
    (\r. follow cat r)
end

def cat = \fishCount.
  n <- count "fish";
  if (n > fishCount)
  { say "yum!";
    if (n >= 3) { love cat } { cat n }
  } {};
  wander;
  cat fishCount
end;

cat 0
