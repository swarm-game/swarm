// 'rec t. F(t)' creates a recursive type which is the solution of t = F(t).
// For example 'rec l. Unit + Int * l' is the type l such that l = Unit + Int * l,
// that is, arbitrary-length finite lists of Int.
//
// Recursive types are equal up to alpha-renaming, so e.g.
// (rec l. Unit + Int * l) = (rec q. Unit + Int * q)

////////////////////////////////////////////////////////////
// Utilities
////////////////////////////////////////////////////////////

// Flipped versions of match and case come in handy for
// writing functions where we want to immediately match or case on
// the argument.  e.g. instead of  `\p. match p \x. \y. ...` we can
// write `λmatch \x. \y. ...`

def λmatch : (a -> b -> c) -> (a * b) -> c =
  \f. \p. match p f
end

def λcase : (a -> c) -> (b -> c) -> (a + b) -> c =
  \f. \g. \s. case s f g
end

////////////////////////////////////////////////////////////
// Lists
////////////////////////////////////////////////////////////

tydef List a = rec l. Unit + a * l end

def nil : List a = inl () end
def cons : a -> List a -> List a = \x. \l. inr (x, l) end

def foldr : (a -> b -> b) -> b -> List a -> b = \f. \z. λcase
  (\_. z)
  (λmatch \x. \xs. f x (foldr f z xs))
end

def map : (a -> b) -> List a -> List b = \f.
  foldr (\y. cons (f y)) nil
end

def append : List a -> List a -> List a = \xs. \ys.
  foldr cons ys xs
end

def concat : List (List a) -> List a = foldr append nil end

def sum : List Int -> Int =
  foldr (\x. \y. x + y) 0
end

def twentySeven = sum (cons 12 (cons 5 (cons 3 (cons 7 nil)))) end

// Note that if a function returns e.g. (rec t. Unit + (Int * Int) * t),
// that is the *same type* as List (Int * Int), so we can use any List
// functions on the output.

def someFun : Int -> (rec t. Unit + (Int * Int) * t) = \x. inr ((x, x), inl ()) end

def doSomethingWithSomeFun : List (Int * Int) =
  (cons (2,3) (cons (4,7) (someFun 5)))
end

////////////////////////////////////////////////////////////
// Binary trees with a at internal nodes and b at leaves
////////////////////////////////////////////////////////////

tydef BTree a b = rec bt. b + bt * a * bt end

def leaf : b -> BTree a b = inl end

def branch : BTree a b -> a -> BTree a b -> BTree a b =
  \l. \a. \r. inr (l, a, r)
end

def foldBTree : (b -> c) -> (c -> a -> c -> c) -> BTree a b -> c =
  \lf. \br. λcase
    lf
    (λmatch \l. λmatch \x. \r.
      br (foldBTree lf br l) x (foldBTree lf br r)
    )
end

def max : Int -> Int -> Int = \a. \b. if (a > b) {a} {b} end

def height : BTree a b -> Int =
  foldBTree (\_. 0) (\l. \_. \r. 1 + max l r)
end

////////////////////////////////////////////////////////////
// Rose trees
////////////////////////////////////////////////////////////

// It would be better to reuse the definition of List
// and define Rose a = rec r. a * List r,
// but we do it this way just to show off nested rec
tydef Rose a = rec r. a * (rec l. Unit + r * l) end

def foldRose : (a -> List b -> b) -> Rose a -> b = \f. λmatch \a. \ts.
  f a (map (foldRose f) ts)
end

def flatten : Rose a -> List a =
  foldRose (\a. \ts. cons a (concat ts))
end

////////////////////////////////////////////////////////////
// Equirecursive types
////////////////////////////////////////////////////////////

// Swarm has equirecursive types, which means a recursive type is
// *equal to* its unfolding.  This has some interesting consequences
// including the fact that types are equal if their infinite unfoldings
// would be equal.

// For example, U1 and U2 below are the same, and the Swarm
// typechecker can tell:

tydef U1 = rec u1. Unit + u1 end
tydef U2 = rec u2. Unit + Unit + u2 end

def u : U1 -> U2 = \u. u end
