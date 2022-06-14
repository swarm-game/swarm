def ifC : forall a. cmd bool -> {cmd a} -> {cmd a} -> cmd a =
  \test. \thn. \els. b <- test; if b thn els end

// Recursive DFS to harvest a contiguous forest
def dfs : cmd () =
  ifC (ishere "tree") {
    grab;
    turn west;
    ifC blocked {} {move; dfs; turn east; move};
    turn north;
    ifC blocked {} {move; dfs; turn south; move};
  } {}
end
