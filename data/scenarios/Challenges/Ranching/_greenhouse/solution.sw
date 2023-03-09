def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;


wait 40;
doN 2 move;
doN 20 (grab; move; wait 8;);