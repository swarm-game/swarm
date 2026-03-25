import "~swarm/lib/control"
import "perm"

def go = \n.
  doN 3 move;
  perm <- readPerm;
  let sol = iteratePermNaive (n+1) perm in
  upTo_ 26 0 (\i. place (toChar (indexArray sol i + 65)); move)
end
