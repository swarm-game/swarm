// Idea: keep a fringe of locations marked with some entity (say,
// flowers), such that at every step a location has a flower iff it is
// exactly n steps away.  To expand it, do DFS (marking visited spots with rocks),
// and when finding flowers, mark adjacent empty spots with some third entity;
// then do a second pass to get rid of flowers; then a third pass to turn the
// third entity into flowers.
