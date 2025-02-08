// A "sheep" that wanders around randomly.

def elif = \p.\t.\e. {if p t e} end;

def turnToClover = \direction.

  x <- scan direction;
  case x (\_. pure false;) (\y.
    if (y == "clover") {
      turn direction;
      pure true;
    } {
      pure false;
    };
  );
  end;


/**
If there is adjacent clover,
turn that direction.
*/
def turnCloverDirection =

    foundN <- turnToClover north;
    if (foundN) {pure true} {
      foundE <- turnToClover east;  
      if (foundE) {pure true} {
        foundS <- turnToClover south;
        if (foundS) {pure true} {
          turnToClover west;
        }
      }
    }
    end;

def decideDirection =

  let randdir : Cmd Dir =
    d <- random 4;
    pure $ if (d == 0) {
        north
      } $ elif (d == 1) {
        east
      } $ elif (d == 2) {
        south
      } {
        west
      }
    in

  cloverCount <- count "clover";
  if (cloverCount > 4) {
    d <- randdir;
    turn d;
  } {
    nearClover <- turnCloverDirection;
    if (nearClover) {} {
      d <- randdir;
      turn d;
    }
  }
  end;

let forever : Cmd Unit -> Cmd Unit = \c. c ; forever c in
let repeat : Int -> Cmd Unit -> Cmd Unit =
  \n. \c. if (n == 0) {} {c ; repeat (n-1) c} in


forever (
  n <- random 30;
  wait (30 + n);

  decideDirection;

  dist <- random 3;
  repeat dist (

    b <- blocked;
    if b {} {
      move;
    };

    // Sheep can drown.
    hasWater <- ishere "water";
    if hasWater {
      say "The jolly jumbuck sprang into the billabong. \"You'll never catch me alive!\", said he.";
      selfdestruct;
    } {};

    // Eat clover.
    x <- scan down;
    case x pure (\y.
        if (y == "clover") {
            harvest;
            cloverCount <- count "clover";
            if (cloverCount < 2) {
              say "yum!"
            } {};
        } {};
    );
  );
  r <- random 30;
  if (r == 0) { say "baaa" } {};

  hasClover <- has "clover";
  if (hasClover) {
    r <- random 8;
    if (r == 0) { 
      let item = "wool" in
      hasWool <- has item;
      if (hasWool) {
        // Make sure nothing's in the way before we place
        // our wool:
        x <- scan down;
        case x pure (\_.
          grab;
          pure ();
        );

        place item;
      } {};
    } {};
  } {};
)
