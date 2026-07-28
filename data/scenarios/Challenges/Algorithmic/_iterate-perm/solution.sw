import "~swarm/lib/control"
import "perm"

def go = \n. \m.
  doN 3 move;
  perm <- readPerm;
  let sol = iteratePermNaive (n+1) perm in
  let sol2 = iteratePerm (m+1) perm in
  upTo_ 26 0 (\i. place (toChar (indexArray sol i + 65)); move);
  turn back; doN 26 (move; grab); turn back;
  upTo_ 26 0 (\i. place (toChar (indexArray sol2 i + 65)); move);
end
