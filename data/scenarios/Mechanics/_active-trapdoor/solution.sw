def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

doN 10 move;
grab;