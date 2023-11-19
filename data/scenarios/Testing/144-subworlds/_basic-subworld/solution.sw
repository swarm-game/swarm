
def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

doN 8 move;
f <- grab;
doN 7 move;
place f;
