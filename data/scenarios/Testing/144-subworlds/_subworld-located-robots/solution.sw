
def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

doN 3 move;
f <- grab;

doN 5 move;
r <- meet;
case r return $ \j. give j f;

