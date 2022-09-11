// if
//  0. all (but max 3) disks in my column are sorted
// then
//  - place "OK"
// else
//  - try to grab "OK"
def repeat = \c. c; repeat c end;
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

w <- whereami;
// the middle of the column
let a = (fst w, snd w + 3) in
repeat (
    o <- as self {
        teleport self a;
        x <- scan south;
        y <- scan down;
        z <- scan north;
        return (f x y && f y z)
    };
    try {
        if o {place "OK"} {grab; return ()}
    } {}
)