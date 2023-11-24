// First attempt, does not work!

def tL = turn left end;
def tR = turn right end;
def tB = turn back end;
def ifM = \p.\t.\e. b <- p; if b t e end;
def DFSn = \n.
  say $ "DFSn at level " ++ format n;
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
def clear_rocks =
  ifM (ishere "rock") {
    grab;
    ifM blocked {} {move; clear_rocks};
    tR; ifM blocked {} {move; clear_rocks};
    tR; ifM blocked {} {move; clear_rocks};
    tR; ifM blocked {} {move; clear_rocks};
    tR
  } {};
  tB; ifM blocked {} {move}; tB
end;
def DFS = \n.
  say ("Searching with depth " ++ format n); place "rock";
  startDFS n; tL; startDFS n; tL; startDFS n; tL; startDFS n;
  clear_rocks
end;
def for : int -> int -> (int -> cmd unit) -> cmd unit = \lo. \hi. \m.
  if (lo > hi) {} {m lo; for (lo+1) hi m}
end;
// build {
//   require 500 "rock";
//   for 1 500 (\n. DFS n);
// }

// Iterative deepening DOES NOT WORK!  Yes, iterative deepening can be
// used to explore all the locations at a distance of precisely n from
// the start, but it does not find the shortest path to each!  When
// visiting a location via some path of length n it might also be
// reachable from the start with a shorter path.
