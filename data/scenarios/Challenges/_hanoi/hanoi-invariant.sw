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
//     or
//     the count of all placed is NOT 3
// then
//  - lock X
if (isUnlocked e)
{
    northFullOrAllPlaced <- as self {
      mn <- scan north;
      case mn (\_.
        teleport self (0,-6);
        allPlaced <- ishere "three";
        return (not allPlaced)
      ) (\_.
        return true
      );
    };
    if northFullOrAllPlaced {
      swap ("blocked " ++ e); return ()
    } {}
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
      wait 16;
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
      if (allPlaced && allSorted) {swap (unlock e); return ()} {}
    ) (\_. return ())
}
))
