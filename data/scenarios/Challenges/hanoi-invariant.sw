def repeat = \c. c; repeat c end;
def isUnlocked = \e. e == "one" || e == "two" || e == "three" end;
def unlock = \e.
  if (e == "blocked one")   {"one"}   {
  if (e == "blocked two")   {"two"}   {
  if (e == "blocked three") {"three"} {
  fail $ "Can not unlock: " ++ e
  }}}
end;

repeat (
me <- scan down;
case me (\_. return ()) (\e.
// if
//  0. I stand on unlocked X
//  1. place north of me is NOT empty
// then
//  - lock X
if (isUnlocked e)
{
    mn <- scan north;
    case mn (\_. return ()) (\_. grab; place ("blocked " ++ e))
}
// if
//  0. I stand on locked X
//  1. place north of me is empty
//  2. all disks are placed
//  3. other columns are sorted (check "OK")
// then
//  - unlock X
{
    mn <- scan north;
    case mn (\_.
      wait 32;
      allPlaced <- as self {
        teleport self (0,-6);
        ishere "three"
      };
      allSorted <- as self {
        teleport self (-2,-5);
        o1 <- ishere "OK";
        teleport self (0,-5);
        o2 <- ishere "OK";
        teleport self (2,-5);
        o3 <- ishere "OK";
        return (o1 && o2 && o3)
      };
      if (allPlaced && allSorted) {grab; place (unlock e)} {}
    ) (\_. return ())
}
))