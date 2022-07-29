def tL = turn left end;
def tR = turn right end;
def tB = turn back end;
def ifM = \p.\t.\e. b <- p; if b t e end;
def DFSn = \n.
  log $ "DFSn at level " ++ format n;
  ifM (ishere "goal") {grab; return ()} {};
  if (n == 0) {} {
    ifM (ishere "rock") {} {
      place "rock";
      tL; b <- blocked; if b {} {move; DFSn (n-1)};
      tR; b <- blocked; if b {} {move; DFSn (n-1)};
      tR; b <- blocked; if b {} {move; DFSn (n-1)};
      tL
    }
  };
  tB; move; tB
end;
def startDFS = \n.
  b <- blocked; if b {} {move; DFSn n}
end;
def DFS = \n.
  startDFS n; tL; startDFS n; tL; startDFS n; tL; startDFS n
end;
def for : int -> int -> (int -> cmd ()) -> cmd () = \lo. \hi. \m.
  if (lo > hi) {} {m lo; for (lo+1) hi m}
end;
build {
  require "treads"; require "scanner"; require "lambda";
  require "strange loop"; require "branch predictor";
  require "grabber"; require "calculator"; require "comparator"; // #540

  require 500 "rock"; require "logger";
  for 1 500 (\n. log ("Searching with depth " ++ format n); DFS n);
}
