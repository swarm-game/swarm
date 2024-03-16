def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def ifC = \p.\t.\e.
  b <- p;
  if b t e
end;

def until = \p.\t.
  ifC p t {until p t}
end;

doN 40 (until (ishere "bit (0)") {grab; move;};);

baseLoc <- as base {whereami};
teleport self baseLoc;
place "treads";
