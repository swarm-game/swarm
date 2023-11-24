def tL = turn left end;
def tR = turn right end;
def tB = turn back end;
def ifM = \p.\t.\e. b <- p; if b t e end;
def DFSn = \n.
  // say $ "DFSn at level " ++ format n;
  ifM (ishere "goal") {swap "path"; return ()} {};
  if (n == 0) {} {
    r <- ishere "rock";
    p <- ishere "path";
    if (r || p) {} {
      place "path";
      tL; b <- blocked; if b {} {move; DFSn (n-1)};
      tR; b <- blocked; if b {} {move; DFSn (n-1)};
      tR; b <- blocked; if b {} {move; DFSn (n-1)};
      tL; swap "rock"; return ()
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
  // say ("Searching with depth " ++ format n);
  place "path";
  startDFS n; tL; startDFS n; tL; startDFS n; tL; startDFS n;
  swap "rock"; clear_rocks
end;
def for : int -> int -> (int -> cmd unit) -> cmd unit = \lo. \hi. \m.
  if (lo > hi) {} {m lo; for (lo+1) hi m}
end;
build {
  require 500 "rock"; require 500 "path";
  for 1 500 (\n. DFS n);
}
