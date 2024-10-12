tydef Color = Bool end
def black = false end
def red = true end
// TODO: s/\((.*\.c) == red\)/$1.red/g

tydef RBTree k v = rec b. Unit + [c: Color, k: k, v: v, l: b, r: b] end
tydef RBNode k v = rec n. [c: Color, k: k, v: v, l: Unit + n, r: Unit + n] end

def emptyT : RBTree k v = inl () end

def isEmptyT : RBTree k v -> Bool = \d. d == emptyT end

def getT : k -> RBTree k v -> Unit + v = \x.\t.
  case t (\_. inl ()) (\n.
    if (x == n.k) { inr n.v } {
        if (x < n.k) {
            getT x n.l
        } {
            getT x n.r
        }
    }
  )
end

def containsT : k -> RBTree k v -> Bool = \x.\t. case (getT x t) (\_. false) (\_. true) end

/*
balance B (T R (T R a x0 b) x1 c) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 1
balance B (T R a x0 (T R b x1 c)) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 2
balance B a x0 (T R (T R b x1 c) x2 d) = T R (T B a x0 b) x1 (T B c x2 d) -- case 3
balance B a x0 (T R b x1 (T R c x2 d)) = T R (T B a x0 b) x1 (T B c x2 d) -- case 4
balance color a x b = T color a x b -- case 5
*/
def balanceT : Color -> k -> v -> RBTree k v -> RBTree k v -> RBTree k v = \c.\x.\y.\l.\r.
  let balanced : k->v->RBTree k v->RBTree k v  ->  k->v  ->  k->v->RBTree k v ->RBTree k v  ->  RBTree k v
    = \x0.\y0.\a.\b. \x1.\y1. \x2.\y2.\c.\d.
    inr [c=red, k=x1, v=y1, l=inr [c=black, k=x0, v=y0, l=a, r=b], r=inr [c=black, k=x2, v=y2, l=c, r=d]]
  in
  let case5 =
    inr [c=c, k=x, v=y, l=l, r=r]
  in
  let balanceRR : RBNode k v -> RBTree k v = \rn.
    undefined
  in
  let balanceRL : RBNode k v -> RBTree k v = \rn.
    undefined
  in
  let balanceR : RBTree k v =
    undefined
  in
  let balanceLR : RBNode k v -> RBTree k v = \ln.
    case ln.r (\_. balanceR) (\lrn.
      if (lrn.c == red) {
        balanced lln.k lln.v lln.l lln.r  ln.k ln.v  ln.r n.x n.r
      } {
        balanceR
      }
    )  in
  let balanceLL : RBNode k v -> RBTree k v = \ln.
    case ln.l (\_. balanceLR ln) (\lln.
      if (lln.c == red) {
        balanced lln.k lln.v lln.l lln.r  ln.k ln.v  ln.r n.x n.r
      } {
        balanceLR ln
      }
    )
  in
  let balanceL : RBTree k v =
    case l (\_.
      balanceR
    ) (\ln.
      if (ln.c == red) {
        balanceLL ln
      } {
        balanceR
      }
    ) 
  in
  if (c == red) {
    case5
  } {
    balanceL
  }
end

def insertT : k -> v -> RBTree k v -> RBTree k v = \x.\y.\t.
  let ins : k -> v -> RBTree k v -> RBTree k v  = \x.\y.\t. case t
    (\_.
      inr [c=red, k=x, v=y, l=inl (), r=inl ()]
    ) (\n.
      if (x == n.k) { inr [c=n.c, k=x, v=y, l=n.l, r=n.r] } {
        if (x < n.k) {
            inr [c=n.c, k=n.k, v=n.v, l=insertD x y n.l, r=n.r]
        } {
            inr [c=n.c,k=n.k, v=n.v, l=n.l, r=insertD x y n.r]
        }
    }
  )
  in let makeBlack : RBTree k v -> RBTree k v = \t. case t (\_.
        fail "makeBlack must always be called on nonempty"
      ) (\n.
        inr [c=black, k=n.k, v=n.v, l=n.l, r=n.r]
      )
  in makeBlack $ ins x y t
end

/*
// helper function to remove the leftmost element from right subtree
def pullLeft: [k: k, v: v, l: RBTree k v, r: RBTree k v] -> ((k * v) * (RBTree k v)) = \n.
  case (n.l) (\_. ((n.k,n.v),n.r)) (\lt. 
    let res = pullLeft lt in
    (fst res, inr [k=n.k, v=n.v, l=snd res, r=n.r])
  )
end

def deleteD : k -> RBTree k v -> RBTree k v = \x.\d.
  case d (\_. inl ()) (\n.
    if (x == n.k) {
        case (n.l) (\_. n.r) (\lt.
            case (n.r) (\_. inl ()) (\rt.
                let r_kvd = pullLeft rt in
                let r_kv = fst r_kvd in
                inr [k=fst r_kv, v=snd r_kv, l=inr lt, r=snd r_kvd]
            )
        )
    } {
        if (x < n.k) {
            inr [k=n.k, v=n.v, l=deleteD x n.l, r=n.r]
        } {
            inr [k=n.k, v=n.v, l=n.l, r=deleteD x n.r]
        }
    }
  )
end
*/