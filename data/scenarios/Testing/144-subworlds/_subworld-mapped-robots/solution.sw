
def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

doN 16 move;

r <- meet;
case r pure $ \j. give j "bitcoin";

