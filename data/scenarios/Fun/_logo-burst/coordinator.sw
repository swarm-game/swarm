def forever : cmd unit -> cmd unit = \c. c ; forever c end

def alternate =
  wait 50;
  say "Green light";
  make "bit (1)";
  wait 100;
  say "Red light";
  make "bit (0)";
  end;

forever alternate;