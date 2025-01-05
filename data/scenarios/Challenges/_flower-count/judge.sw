def win =
  try {
    firestarter <- robotNamed "firestarter";
    halt firestarter
  } {};
  try {
    fire <- robotNamed "fire";
    halt fire; wait 1;
    reprogram fire { selfdestruct };
  } {};
  create "gold"
end

def judgeCount : Int -> Cmd Unit = \actual.
  watch down;
  wait 1024;
  s <- scan down;
  case s
    (\_. return ())
    (\p.
      try {
        let c = (read p : Int) in
        if (c == actual) { win } {}
      } {}
    )
end

def forever = \c. c; forever c end

def judge =
  numFlowers <- resonate "flower" ((-59,-19),(60,20));
  forever (judgeCount numFlowers);
end;

judge
