/*******************************************************************/
/*                           CONTAINERS                            */
/*******************************************************************/
// This file has an example implementation of lists, sets and maps
// (a.k.a dictionaries) using recursive types.
//
// The implementation uses a Red-Black tree balanced binary tree
// with projection (k -> a) to get sets and dictionaries (compare
// only first element of tuple). It is based on Haskell code in:
// https://abhiroop.github.io/Haskell-Red-Black-Tree/

tydef Maybe a = Unit + a end
def mmap : (a -> b) -> Maybe a -> Maybe b = \f.\m. case m (\_. inl ()) (\a. inr (f a)) end

/*******************************************************************/
/*                              LISTS                              */
/*******************************************************************/

tydef List a = rec l. Maybe (a * l) end

def emptyL : List a = inl () end
def insertL : a -> List a -> List a = \a.\l. inr (a, l) end
def pureL : a -> List a = \a. insertL a emptyL end

def appendL : List a -> List a -> List a = \l.\r.
  case l (\_. r) (\ln. inr (fst ln, appendL (snd ln) r))
end

def lengthL : List a -> Int =
  let len: Int -> List a -> Int = \a.\l. case l (\_. a) (\ln. len (a + 1) (snd ln)) in len 0
end 

def safeGetL : Int -> List a -> Maybe a = \i.\l.
  case l (\_. inl ()) (\n. if (i <= 0) {inr $ fst n} {safeGetL (i - 1) (snd n)})
end 

def getL : Int -> List a -> a = \i.\l.
  case (safeGetL i l) (\_. fail $ "INDEX " ++ format i ++ " OUT OF BOUNDS!") (\a. a)
end

// get from ordered list
def getKeyL : (a -> k) -> k -> List a -> Maybe a = \p.\x.\l.
  case l (\_. inl ()) (\n.
    let nx = p (fst n) in
    if (nx == x) {
      inr $ fst n
    } {
      if (nx < x) {
        inl ()
      } {
        getKeyL p x $ snd n
      }
    }
  )
end

def containsKeyL : (a -> k) -> k -> List a -> Bool = \p.\x.\l.
  case (getKeyL p x l) (\_. true) (\_. false)
end

// insert into ordered list - replaces elements, like set
def insertKeyL : (a -> k) -> a -> List a -> List a = \p.\y.\l.
  case l (\_. pureL y) (\n.
    let nx = p (fst n) in
    let x = p y in
    if (nx == x) {
      inr (y, snd n)
    } {
      if (nx > x) {
        insertL y l
      } {
        inr (fst n, insertKeyL p y $ snd n)
      }
    }
  )
end

def formatL : List a -> Text = \l.
 let f : List a -> Text = \l. case l (\_. "]") (\n. ", " ++ format (fst n) ++ f (snd n))
 in case l (\_. "[]") (\n. "[" ++ format (fst n) ++ f (snd n))
end

/*******************************************************************/
/*                         RED BLACK TREE                          */
/*******************************************************************/

tydef RBTree k = rec b. Maybe [red: Bool, k: k, l: b, r: b] end

// equirecursive types allow us to get the node type without mutual recursion
tydef RBNode k = rec n. [red: Bool, k: k, l: Maybe n, r: Maybe n] end

def emptyT : RBTree k = inl () end

def isEmptyT : RBTree k -> Bool = \d. d == emptyT end

def getT : (k -> a) -> a -> RBTree k -> Maybe k = \p.\x.\t.
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

def inorder : RBTree k -> List k = \t.
  case t (\_. inl ()) (\n.
    appendL (inorder n.l) (appendL (pureL n.k) $ inorder n.r)
  )
end

def formatT : RBTree k -> Text = \t.
  case t (\_. "N") (\n.
    "(T "
    ++ if n.red {"R "} {"B "}
    ++ formatT n.l ++ " " ++ format n.k ++ " " ++ formatT n.r ++ ")"
  ) 
end

def indent_ = \c.\i. if (i <= 0) {" "} {c ++ indent_ c (i - 1)} end
def indent = indent_ " " end

def debugT : RBTree k -> Cmd Unit =
  let d : Text -> Text -> Text -> RBTree k -> Cmd Unit = \i.\li.\ri.\t. case t (\_. log $ i ++ "+ N") (\n.
    d (i ++ li) "  " "| " n.l;
    log $ i ++ "+ " ++ if n.red {"R "} {"B "} ++ format n.k;
    d (i ++ ri) "| " "  " n.r;
  ) in d "" "" ""
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
    inr [red=true, l=inr [red=false, l=v.a, k=v.x0, r=v.b], k=v.x1, r=inr [red=false, l=v.c, k=v.x2, r=v.d]]
  in let case5 : RBNode k -> RBTree k =
    inr  // just id - TODO: inline?
  in let balanceRR : RBNode k -> RBNode k -> RBTree k = \t.\rn.
    case rn.r (\_. case5 t) (\rrn.
      if rrn.red {
        balanced [a=t.l, x0=t.k, b=rn.l, x1=rn.k, c=rrn.l, x2=rrn.k, d=rrn.r]
      } {
        case5 t
      }
    )
  in let balanceRL : RBNode k -> RBNode k -> RBTree k = \t.\rn.
    case rn.l (\_. balanceRR t rn) (\rln.
      if rln.red {
        balanced [a=t.l, x0=t.k, b=rln.l, x1=rln.k, c=rln.r, x2=rn.k, d=rn.r]
      } {
        balanceRR t rn
      }
    )
  in let balanceR : RBNode k -> RBTree k = \t.
    case t.r (\_.
      case5 t
    ) (\lr.
      if lr.red {
        balanceRL t lr
      } {
        case5 t
      }
    )
  in let balanceLR : RBNode k -> RBNode k -> RBTree k = \t.\ln.
    case ln.r (\_. balanceR t) (\lrn.
      if lrn.red {
        balanced [a=ln.l, x0=ln.k, b=lrn.l, x1=lrn.k, c=lrn.r, x2=t.k, d=t.r]
      } {
        balanceR t
      }
    )
  in let balanceLL : RBNode k -> RBNode k -> RBTree k = \t.\ln.
    case ln.l (\_. balanceLR t ln) (\lln.
      if lln.red {
        balanced [a=lln.l, x0=lln.k, b=lln.r, x1=ln.k, c=ln.r, x2=t.k, d=t.r]
      } {
        balanceLR t ln
      }
    )
  in let balanceL : RBNode k -> RBTree k = \t.
    case t.l (\_.
      balanceR t
    ) (\ln.
      if ln.red {
        balanceLL t ln
      } {
        balanceR t
      }
    )
  in if t.red {
    case5 t
  } {
    balanceL t
  }
end

def insertT : (k -> a) -> k -> RBTree k -> RBTree k = \p.\x.\t.
  let ins : (k -> a) -> k -> RBTree k -> RBTree k =\p.\x.\t. case t
    (\_.
      inr [red=true, k=x, l=inl (), r=inl ()]
    ) (\n.
      if (p x == p n.k) { inr [red=n.red, k=x, l=n.l, r=n.r] } {
        if (x < n.k) {
            balanceT [red=n.red, k=n.k, l=ins p x n.l, r=n.r]
        } {
            balanceT [red=n.red, k=n.k, l=n.l, r=ins p x n.r]
        }
      }
    )
  in let makeBlack : RBTree k -> RBTree k = \t. case t (\_.
        fail "makeBlack will always be called on nonempty"
      ) (\n.
        inr [red=false, k=n.k, l=n.l, r=n.r]
      )
  in makeBlack $ ins p x t
end

def deleteT : (k -> a) -> a -> RBTree k -> RBTree k = \p.\x.\t.
  // ------------------------------------------------------------------------------------------------------------
  tydef Del k a = (k -> a) -> a -> RBTree k -> RBTree k end
  // ------------------------------------------------------------------------------------------------------------
  let makeBlack : RBTree k -> RBTree k = \t.
    case t (\_. t) (\n. inr [red=false, k=n.k, l=n.l, r=n.r])
  in let makeRed : RBNode k -> RBTree k = \n.
    inr [red=true, k=n.k, l=n.l, r=n.r]
  // ------------------------------------------------------------------------------------------------------------
  /*
  balL (T B (T R t1 x t2) y t3) = T R (T B t1 x t2) y t3
  balL (T B t1 y (T B t2 z t3)) = balance' (T B t1 y (T R t2 z t3))
  balL (T B t1 y (T R (T B t2 u t3) z t4@(T B l value r)))
    = T R
      (T B t1 y t2)
      u
      (balance' (T B t3 z
        (T R l value r)))
  */
  in let balL : RBNode k -> RBTree k = \n.
    let balL2 : RBNode k -> RBNode k -> RBTree k = \n.\rn.
      case rn.l (\_. inr n) (\rln.
        if rln.red {inr n} {
          case rn.r (\_. inr n) (\rrn.
            if rrn.red {inr n} {
              inr [red=true,
                   l=inr [red=false, l=n.l, k=n.k, r=rln.l],
                   k=rln.k,
                   r=balanceT [red=false, l=rln.r, k=rn.k, r=inr [red=true, l=rrn.l, k=rrn.k, r=rrn.r]]]
            }
          )
        }
      )
    in let balL1 : RBNode k -> RBTree k = \n.
      case n.r (\_. inr n) (\rn. 
        if rn.red {balL2 n rn} {balanceT [red=false, k=n.k, l=n.l, r=makeRed rn]}
      )
    in case n.l (\_. balL1 n) (\ln. if ln.red {inr [red=true, k=n.k, l=makeBlack n.l, r=n.r]} {balL1 n})
  // ------------------------------------------------------------------------------------------------------------
  /*
  balR (T B t1 y (T R t2 x t3)) = T R t1 y (T B t2 x t3)
  balR (T B (T B t1 z t2) y t3) = balance' (T B (T R t1 z t2) y t3)
  balR (T B
    ln@(T R
      lln@(T B lll llk llr)
      lk
      lrn@(T B lrl lrk lrr))
    k
    rn)
    =
    T R
      (balance' (T B (T R lll llk llr) lk lrl))
      lrk
      (T B lrr k rn)
  */
  in let balR : RBNode k -> RBTree k = \n.
    let balR2 : RBNode k -> RBNode k -> RBTree k = \n.\ln.
      case ln.l (\_. inr n) (\lln.
        if lln.red {inr n} {
          case ln.r (\_. inr n) (\lrn.
            if lrn.red {inr n} {
              inr [red=true,
                   l=balanceT [red=false, l=inr [red=true, l=lln.l, k=lln.k, r=lln.r], k=ln.k, r=lrn.l],
                   k=lrn.k,
                   r=inr [red=false, l=lrn.r, k=n.k, r=n.r]]
            }
          )
        }
      )
    in let balR1 : RBNode k -> RBTree k = \n.
      case n.l (\_. inr n) (\ln. 
        if ln.red {balR2 n ln} {balanceT [red=false, k=n.k, l=makeRed ln, r=n.r]}
      )
    in case n.r (\_. balR1 n) (\rn. if rn.red {inr [red=true, k=n.k, l=n.l, r=makeBlack n.r]} {balR1 n})
  // ------------------------------------------------------------------------------------------------------------
  in let delL : Del k a -> (k -> a) -> a -> RBNode k -> RBTree k = \del.\p.\x.\n.
    let dl = [red=n.red, k=n.k, l=del p x n.l, r=n.r] in
    if n.red { inr dl } { balL dl }
  // ------------------------------------------------------------------------------------------------------------
  in let delR : Del k a -> (k -> a) -> a -> RBNode k -> RBTree k = \del.\p.\x.\n.
    let dr = [red=n.red, k=n.k, l=n.l, r=del p x n.r] in
    if n.red { inr dr } { balR dr }
  // ------------------------------------------------------------------------------------------------------------
  /*
  fuse E t = t
  fuse t E = t
  fuse t1@(T B _ _ _) (T R t3 y t4) = T R (fuse t1 t3) y t4
  fuse (T R t1 x t2) t3@(T B _ _ _) = T R t1 x (fuse t2 t3)
  fuse (T R t1 x t2) (T R t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T R t1 x s1) z (T R s2 y t4))
        (T B _ _ _)   -> (T R t1 x (T R s y t4))
  fuse (T B t1 x t2) (T B t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T B t1 x s1) z (T B s2 y t4))
        (T B s1 z s2) -> balL (T B t1 x (T B s y t4))
  */
  in let fuse: RBTree k -> RBTree k -> RBTree k = \l.\r.
    case l (\_. r) (\ln. case r (\_. l) (\rn.
      if ln.red {
        if rn.red {
          /* RED RED */
          let s = fuse ln.r rn.l in
          let b_case = {inr [red=true, l=ln.l, k=ln.k, r=inr [red=true, l=s, k=rn.k, r=rn.r]]} in
          case s (\_. force b_case) (\sn.
            if sn.red {
              inr [red=true, l=inr [red=true, l=ln.l, k=ln.k, r=sn.l], k=sn.k, r=inr [red=true, l=sn.r, k=rn.k, r=rn.r]]
            } {
              force b_case
            }
          ) 
        } {
          /* RED BLACK */
          inr [red=true, l=ln.l, k=ln.k, r=fuse ln.r (inr rn)]
        }
      } {
        if rn.red {
          /* BLACK RED */
          inr [red=true, l=fuse (inr ln) rn.l, k=rn.k, r=rn.r]
        } {
          /* BLACK BLACK */
          let s = fuse ln.r rn.l in
          let b_case = {balL [red=false, l=ln.l, k=ln.k, r=inr [red=false, l=s, k=rn.k, r=rn.r]]} in
          case s (\_. force b_case) (\sn.
            if sn.red {
              inr [red=true, l=inr [red=false, l=ln.l, k=ln.k, r=sn.l], k=sn.k, r=inr [red=false, l=sn.r, k=rn.k, r=rn.r]]
            } {
              force b_case
            }
          )
        }
      }
    ))
  // ------------------------------------------------------------------------------------------------------------
  in let del : (k -> a) -> a -> RBTree k -> RBTree k = \p.\x.\t.
    case t (\_.t) (\n. 
      if (p n.k == x) {fuse n.l n.r} {
        if (x < p n.k) {delL del p x n} {delR del p x n}
      }
    )
  // ------------------------------------------------------------------------------------------------------------
  in makeBlack $ del p x t
end

/*******************************************************************/
/*                           DICTIONARY                            */
/*******************************************************************/

tydef Dict k v = RBTree (k * v) end

def emptyD : Dict k v = emptyT end
def isEmptyD : Dict k v -> Bool = isEmptyT end
def getD : k -> Dict k v -> Maybe v = \k.\d. mmap snd (getT fst k d) end 
def containsD : k -> Dict k v -> Bool = containsT fst end
def insertD : k -> v -> Dict k v -> Dict k v = \k.\v. insertT fst (k, v) end
def deleteD : k -> Dict k v -> Dict k v = \k. deleteT fst k end

def formatD : Dict k v -> Text = \d.
 let p : (k * v) -> Text = \kv. format (fst kv) ++ ": " ++ format (snd kv) in 
 let f : List (k * v) -> Text = \l. case l (\_. "}") (\n. ", " ++ p (fst n) ++ f (snd n))
 in case (inorder d) (\_. "{}") (\n. "{" ++ p (fst n) ++ f (snd n))
end

/*******************************************************************/
/*                              SET                                */
/*******************************************************************/

tydef Set k = RBTree k end

def emptyS : Set k = emptyT end
def isEmptyS : Set k -> Bool = isEmptyT end
def containsS : k -> Set k -> Bool = containsT (\x.x) end
def insertS : k -> Set k -> Set k = insertT (\x.x) end
def deleteS : k -> Set k -> Set k = \k. deleteT (\x.x) k end
def formatS : Set k -> Text = \s. formatL $ inorder s end

/*******************************************************************/
/*                           UNIT TESTS                            */
/*******************************************************************/

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
  return ()
end

def test_empty: Int -> Cmd Unit = \i.
  group i "EMPTY TESTS" (\i.
    assert i (isEmptyD emptyD) "empty tree should be empty";
    assert_eq i (inl ()) (getD 0 $ emptyD) "no element should be present in an empty tree";
    assert i (not $ containsD 0 $ emptyD) "empty tree should not contain any elements";
  )
end

def test_ordered_list : Int -> Cmd Unit = \i.
  let s = [insert=insertKeyL (\x.x), contains=containsKeyL (\x.x)] in
  group i "ORDERED LIST TESTS" (\i.
    let expected = insertL 1 $ insertL 2 $ insertL 3 emptyL in
    let actual = s.insert 2 $ s.insert 1 $ s.insert 3 emptyL in
    assert_eq i (formatL expected) (formatL actual) "insertKeyL should insert in order";
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

def randomTestS = \i.\s.\test.
  if (i <= 0) {return s} {
    x <- random 20;
    let ns = insertS x s in
    test ns;
    randomTestS (i - 1) ns test
  }
end

def delete_insert_prop = \i.\t.
  x <- random 20;
  let i_t = inorder t in
  let f_t = formatS t in
  if (not $ containsS x t) {
    log $ format x;
    assert_eq i
      (formatL i_t)
      (formatS $ deleteS x $ insertS x t)
      (f_t ++ " == delete " ++ format x ++ " $ insert " ++ format x ++ " " ++ f_t)
  } { delete_insert_prop i t /* reroll */}
end

def delete_delete_prop = \i.\t.
  let i_t = inorder t in
  i <- random (lengthL i_t);
  let x = getL i i_t in
  let ds = deleteS x t in
  let dds = deleteS x ds in
  let f_t = formatS t in
  let f_dx = "delete " ++ format x in
  assert_eq i (formatS ds) (formatS dds)
    (f_dx ++ f_t ++ " == " ++ f_dx ++ " $ " ++ f_dx ++ " " ++ f_t)
end

def test_delete: Int -> Cmd Unit = \i.
  group i "DELETE TESTS" (\i.
    randomTestS 15 emptyS (\s.
      group i (formatS s) (\i.
        delete_insert_prop i s;
        delete_delete_prop i s;
      )
    )
  )
end

def test_tree: Cmd Unit =
  group 0 "TREE TESTS" (\i.
    test_empty i;
    test_ordered_list i;
    test_insert i;
    test_delete i;
  );
  log "ALL TREE TESTS PASSED!"
end