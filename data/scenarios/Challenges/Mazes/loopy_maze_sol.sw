def tL = turn left end;
def tR = turn right end;
def tB = turn back end;
def ifM = \p.\t.\e. b <- p; if b t e end;
def DFS =
  ifM (ishere "goal") {grab; pure ()} {};
  ifM (ishere "rock") {} {
    place "rock";
    tL; b <- blocked; if b {} {move; DFS};
    tR; b <- blocked; if b {} {move; DFS};
    tR; b <- blocked; if b {} {move; DFS};
    tL
  };
  tB; move; tB
end;
build {
  require 500 "rock"; DFS
}
