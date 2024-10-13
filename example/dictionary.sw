tydef Color = Bool end
def black = false end
def red = true end
// TODO: s/\((.*\.c) == red\)/$1.red/g

tydef Maybe a = Unit + a end
def mmap : (a -> b) -> Maybe a -> Maybe b = \f.\m. case m (\_. inl ()) (\a. inr (f a)) end

tydef RBTree k = rec b. Unit + [c: Color, k: k, l: b, r: b] end
tydef RBNode k = rec n. [c: Color, k: k, l: Unit + n, r: Unit + n] end

def emptyT : RBTree k = inl () end

def isEmptyT : RBTree k -> Bool = \d. d == emptyT end

def getT : (k -> a) -> a -> RBTree k -> Unit + k = \p.\x.\t.
  case t (\_. inl ()) (\n.
    if (x == p n.k) { inr n.k } {
        if (x < p n.k) {
            getT p x n.l
        } {
            getT p x n.r
        }
    }
  )
end

def containsT : (k -> a) -> a -> RBTree k -> Bool = \p.\x.\t. getT p x t != inl () end

tydef List a = rec l. Unit + (a * l) end

def emptyL : List a = inl () end
def insertL : a -> List a -> List a = \a.\l. inr (a, l) end
def pureL : a -> List a = \a. insertL a emptyL end

def appendL : List a -> List a -> List a = \l.\r.
  case l (\_. r) (\ln. inr (fst ln, appendL (snd ln) r))
end 

def formatL : List a -> Text = \l.
 let f : List a -> Text = \l. case l (\_. "]") (\n. ", " ++ format (fst n) ++ f (snd n))
 in case l (\_. "[]") (\n. "[" ++ format (fst n) ++ f (snd n))
end

def inorder : RBTree k -> List k = \t.
  case t (\_. inl ()) (\n.
    appendL (inorder n.l) (appendL (pureL n.k) $ inorder n.r)
  )
end

/*
balance B (T R (T R a x0 b) x1 c) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 1: LL red
balance B (T R a x0 (T R b x1 c)) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 2: LR red
balance B a x0 (T R (T R b x1 c) x2 d) = T R (T B a x0 b) x1 (T B c x2 d) -- case 3: RL red
balance B a x0 (T R b x1 (T R c x2 d)) = T R (T B a x0 b) x1 (T B c x2 d) -- case 4: RR red
balance color a x b = T color a x b -- case 5
*/
def balanceT : RBNode k -> RBTree k = \t.
  let balanced : [ a: RBTree k, x0: k, b: RBTree k,   x1: k,   c: RBTree k, x2: k, d: RBTree k] -> RBTree k
    = \v.
    inr [c=red, l=inr [c=black, l=v.a, k=v.x0, r=v.b], k=v.x1, r=inr [c=black, l=v.c, k=v.x2, r=v.d]]
  in let case5 : RBNode k -> RBTree k =
    inr  // just id - TODO: inline?
  in let balanceRR : RBNode k -> RBNode k -> RBTree k = \t.\rn.
    case rn.r (\_. case5 t) (\rrn.
      if (rrn.c == red) {
        balanced [a=t.l, x0=t.k, b=rn.l, x1=rn.k, c=rrn.l, x2=rrn.k, d=rrn.r]
      } {
        case5 t
      }
    )
  in let balanceRL : RBNode k -> RBNode k -> RBTree k = \t.\rn.
    case rn.l (\_. balanceRR t rn) (\rln.
      if (rln.c == red) {
        balanced [a=t.l, x0=t.k, b=rln.r, x1=rln.k, c=rln.r, x2=rn.k, d=rn.r]
      } {
        balanceRR t rn
      }
    )
  in let balanceR : RBNode k -> RBTree k = \t.
    case t.r (\_.
      case5 t
    ) (\lr.
      if (lr.c == red) {
        balanceRL t lr
      } {
        case5 t
      }
    )
  in let balanceLR : RBNode k -> RBNode k -> RBTree k = \t.\ln.
    case ln.r (\_. balanceR t) (\lrn.
      if (lrn.c == red) {
        balanced [a=ln.l, x0=ln.k, b=lrn.l, x1=lrn.k, c=lrn.l, x2=t.k, d=t.r]
      } {
        balanceR t
      }
    )
  in let balanceLL : RBNode k -> RBNode k -> RBTree k = \t.\ln.
    case ln.l (\_. balanceLR t ln) (\lln.
      if (lln.c == red) {
        balanced [a=lln.l, x0=lln.k, b=lln.r, x1=ln.k, c=ln.r, x2=t.k, d=t.r]
      } {
        balanceLR t ln
      }
    )
  in let balanceL : RBNode k -> RBTree k = \t.
    case t.l (\_.
      balanceR t
    ) (\ln.
      if (ln.c == red) {
        balanceLL t ln
      } {
        balanceR t
      }
    )
  in if (t.c == red) {
    case5 t
  } {
    balanceL t
  }
end

def insertT : (k -> a) -> k -> RBTree k -> RBTree k = \p.\x.\t.
  let ins : (k -> a) -> k -> RBTree k -> RBTree k =\p.\x.\t. case t
    (\_.
      inr [c=red, k=x, l=inl (), r=inl ()]
    ) (\n.
      if (p x == p n.k) { inr [c=n.c, k=x, l=n.l, r=n.r] } {
        if (x < n.k) {
            balanceT [c=n.c, k=n.k, l=ins p x n.l, r=n.r]
        } {
            balanceT [c=n.c,k=n.k, l=n.l, r=ins p x n.r]
        }
      }
    )
  in let makeBlack : RBTree k -> RBTree k = \t. case t (\_.
        fail "makeBlack will always be called on nonempty"
      ) (\n.
        inr [c=black, k=n.k, l=n.l, r=n.r]
      )
  in makeBlack $ ins p x t
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

/*******************************************************************/
/*                           DICTIONARY                            */
/*******************************************************************/

tydef Dict k v = RBTree (k * v) end

def emptyD : Dict k v = emptyT end
def isEmptyD : Dict k v -> Bool = isEmptyT end
def getD : k -> Dict k v -> Unit + v = \k.\d. mmap snd (getT fst k d) end 
def containsD : k -> Dict k v -> Bool = containsT fst end
def insertD : k -> v -> Dict k v -> Dict k v = \k.\v. insertT fst (k, v) end

/*******************************************************************/
/*                              SET                                */
/*******************************************************************/

tydef Set k = RBTree k end

def emptyS : Set k = emptyT end
def isEmptyS : Set k -> Bool = isEmptyT end
def containsS : k -> Set k -> Bool = containsT (\x.x) end
def insertS : k -> Set k -> Set k = insertT (\x.x) end

/*******************************************************************/
/*                           UNIT TESTS                            */
/*******************************************************************/

def indent = \i. if (i <= 0) {""} {"  " ++ indent (i - 1)} end

def assert = \i. \b. \m. if b {log $ indent i ++ "OK: " ++ m} {log "FAIL:"; fail m} end

def assert_eq 
  = \i. \exp. \act. \m.
  if (exp == act) {log $ indent i ++ "OK: " ++ m} {
    log (indent i ++ "FAIL: expected " ++ format exp ++ " actual " ++ format act);
    fail m
  }
end

def group = \i. \m. \tests.
  log $ indent i ++ "START " ++ m ++ ":";
  tests $ i + 1;
  log $ indent i ++ "END " ++ m ++ ":";
end

def test_empty: Int -> Cmd Unit = \i.
  group i "EMPTY TESTS" (\i.
    assert i (isEmptyD emptyD) "empty tree should be empty";
    assert_eq i (inl ()) (getD 0 $ emptyD) "no element should be present in an empty tree";
    assert i (not $ containsD 0 $ emptyD) "empty tree should not contain any elements";
  )
end

def test_insert: Int -> Cmd Unit = \i.
  group i "INSERT TESTS" (\i.
    group i "test {0: 1}" (\i.
      let tree0 = insertD 0 1 emptyD in
      assert i (not $ isEmptyD tree0) "nonempty tree should not be empty";
      assert_eq i (inr 1) (getD 0 $ tree0) "one element should be present in a one element tree";
      assert i (containsD 0 $ tree0) "one element tree should contain that elements";
      assert i (not $ containsD 1 $ tree0) "one element tree should contain only that elements";
    );
    group i "test {0: 1, 2: 3}" (\i.
      let tree02 = insertD 2 3 $ insertD 0 1 emptyD in
      assert_eq i (inr 1) (getD 0 $ tree02) "get 0 {0: 1, 2: 3} == 1";
      assert_eq i (inr 3) (getD 2 $ tree02) "get 2 {0: 1, 2: 3} == 3";
    );
  );
end

def test_tree: Cmd Unit =
  group 0 "TREE TESTS" (\i.
    test_empty i;
    test_insert i;
  )
end