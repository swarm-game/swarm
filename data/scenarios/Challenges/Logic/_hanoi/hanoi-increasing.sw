import "~swarm/lib/control"

// if
//  0. all (but max 3) disks in my column are sorted
// then
//  - place "OK"
// else
//  - try to grab "OK"
def null = inl () end;

def toI = \e.
  if (e == "one"   || e == "blocked one")   {1} {
  if (e == "two"   || e == "blocked two")   {2} {
  if (e == "three" || e == "blocked three") {3} {
  fail $ "There should be no other placeable entity: " ++ e
  }}}
end;
def f = \x.\y.
  case x (\_. false) (\i.
    case y (\_. false) (\j.
      let xi = toI i in
      let yj = toI j in
      xi > yj
    )
  )
end;

def go =
w <- whereami;
match w \wx. \wy.
// the middle of the column
let a = (wx, wy + 3) in
forever (
    o <- as self {
        teleport self a;
        x <- scan south;
        y <- scan down;
        z <- scan north;
        if (z == null) {
          if (y == null) {
            pure true
          } {
            pure $ f x y
          }
        } {
          pure $ f x y && f y z
        }
    };
    try {
        if o {place "OK"} {grab; pure ()}
    } {}
)
end
