import "~swarm/lib/arith"
import "~swarm/lib/control"

def compose : Array Int -> Array Int -> Array Int = \p. \q.
  unfoldArray 0 (\i. if (i == arraySize p) {inl ()} {inr (indexArray p (indexArray q i), i+1)})
end

def idPerm : Int -> Array Int = \n.
  unfoldArray 0 (\i. if (i == n) {inl ()} {inr (i, i+1)})
end

def iteratePermNaive : Int -> Array Int -> Array Int = \n. \p.
  if (n == 0)
    {idPerm (arraySize p)}
    {compose p (iteratePermNaive (n-1) p)}
end

def iteratePerm : Int -> Array Int -> Array Int = \n. \p.
  if (n == 0)
    { idPerm (arraySize p)}
    { let p2 = iteratePerm (n / 2) p in
      if (isEven n)
        {compose p2 p2}
        {compose p (compose p2 p2)}
    }
end

def readPerm : Cmd (Array Int) =
  arr <- unfoldArrayC 0 (\i. if (i == 26) {pure (inl ())} {x <- grab; move; pure (inr (charAt 0 x - 65, i+1))});
  turn back; doN 26 move;
  turn back;
  pure arr
end
