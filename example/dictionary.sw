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

def containsT : k -> RBTree k v -> Bool = \x.\t. getT x t != inl () end

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

def inorder : RBTree k v -> List (k * v) = \t.
  case t (\_. inl ()) (\n.
    appendL (inorder n.l) (appendL (pureL (n.k, n.v)) $ inorder n.r)
  )
end

/*
balance B (T R (T R a x0 b) x1 c) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 1: LL red
balance B (T R a x0 (T R b x1 c)) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 2: LR red
balance B a x0 (T R (T R b x1 c) x2 d) = T R (T B a x0 b) x1 (T B c x2 d) -- case 3: RL red
balance B a x0 (T R b x1 (T R c x2 d)) = T R (T B a x0 b) x1 (T B c x2 d) -- case 4: RR red
balance color a x b = T color a x b -- case 5
*/
def balanceT : RBNode k v -> RBTree k v = \t.
  let balanced : k -> v -> RBTree k v -> RBTree k v
              -> k -> v
              -> k -> v -> RBTree k v -> RBTree k v
              -> RBTree k v
    = \x0.\y0.\a.\b. \x1.\y1. \x2.\y2.\c.\d.
    inr [c=red, k=x1, v=y1, l=inr [c=black, k=x0, v=y0, l=a, r=b], r=inr [c=black, k=x2, v=y2, l=c, r=d]]
  in let case5 : RBNode k v -> RBTree k v =
    inr  // just id - TODO: inline?
  in let balanceRR : RBNode k v -> RBNode k v -> RBTree k v = \t.\rn.
    case rn.r (\_. case5 t) (\rrn.
      if (rrn.c == red) {
        balanced t.k t.v t.l rn.l  rn.k rn.v  rrn.k rrn.v rrn.l rrn.r
      } {
        case5 t
      }
    )
  in let balanceRL : RBNode k v -> RBNode k v -> RBTree k v = \t.\rn.
    case rn.l (\_. balanceRR t rn) (\rln.
      if (rln.c == red) {
        balanced t.k t.v t.l rln.r  rln.k rln.v  rn.k rn.v rln.r rn.r
      } {
        balanceRR t rn
      }
    )
  in let balanceR : RBNode k v -> RBTree k v = \t.
    case t.r (\_.
      case5 t
    ) (\lr.
      if (lr.c == red) {
        balanceRL t lr
      } {
        case5 t
      }
    )
  in let balanceLR : RBNode k v -> RBNode k v -> RBTree k v = \t.\ln.
    case ln.r (\_. balanceR t) (\lrn.
      if (lrn.c == red) {
        balanced ln.k ln.v ln.l lrn.l  lrn.k lrn.v  t.k t.v lrn.l t.r
      } {
        balanceR t
      }
    )
  in let balanceLL : RBNode k v -> RBNode k v -> RBTree k v = \t.\ln.
    case ln.l (\_. balanceLR t ln) (\lln.
      if (lln.c == red) {
        balanced lln.k lln.v lln.l lln.r  ln.k ln.v  t.k t.v ln.r t.r
      } {
        balanceLR t ln
      }
    )
  in let balanceL : RBNode k v -> RBTree k v = \t.
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

def insertT : k -> v -> RBTree k v -> RBTree k v = \x.\y.\t.
  let ins : k -> v -> RBTree k v -> RBTree k v  = \x.\y.\t. case t
    (\_.
      inr [c=red, k=x, v=y, l=inl (), r=inl ()]
    ) (\n.
      if (x == n.k) { inr [c=n.c, k=x, v=y, l=n.l, r=n.r] } {
        if (x < n.k) {
            balanceT [c=n.c, k=n.k, v=n.v, l=ins x y n.l, r=n.r]
        } {
            balanceT [c=n.c,k=n.k, v=n.v, l=n.l, r=ins x y n.r]
        }
      }
    )
  in let makeBlack : RBTree k v -> RBTree k v = \t. case t (\_.
        fail "makeBlack will always be called on nonempty"
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
    assert i (isEmptyT emptyT) "empty tree should be empty";
    assert_eq i (inl ()) (getT 0 $ emptyT) "no element should be present in an empty tree";
    assert i (not $ containsT 0 $ emptyT) "empty tree should not contain any elements";
  )
end

def test_insert: Int -> Cmd Unit = \i.
  group i "INSERT TESTS" (\i.
    group i "test {0: 1}" (\i.
      let tree0 = insertT 0 1 emptyT in
      assert i (not $ isEmptyT tree0) "nonempty tree should not be empty";
      assert_eq i (inr 1) (getT 0 $ tree0) "one element should be present in a one element tree";
      assert i (containsT 0 $ tree0) "one element tree should contain that elements";
      assert i (not $ containsT 1 $ tree0) "one element tree should contain only that elements";
    );
    group i "test {0: 1, 2: 3}" (\i.
      let tree02 = insertT 2 3 $ insertT 0 1 emptyT in
      assert_eq i (inr 1) (getT 0 $ tree02) "get 0 {0: 1, 2: 3} == 1";
      assert_eq i (inr 3) (getT 2 $ tree02) "get 2 {0: 1, 2: 3} == 3";
    );
  );
end

def test_tree: Cmd Unit =
  group 0 "TREE TESTS" (\i.
    test_empty i;
    test_insert i;
  )
end