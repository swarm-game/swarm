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
def DFS = \n.
  say ("Searching with depth " ++ format n);
  startDFS n; tL; startDFS n; tL; startDFS n; tL; startDFS n
end;
def for : int -> int -> (int -> cmd unit) -> cmd unit = \lo. \hi. \m.
  if (lo > hi) {} {m lo; for (lo+1) hi m}
end;
build {
  require 500 "rock";
  for 1 500 (\n. DFS n);
}
