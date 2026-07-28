import "list"
import "control"

// Generate an array from a size and a function mapping indices to array values.
def generate : Int -> (Int -> a) -> Array a
  = \n. \f. unfoldArray 0 (\k. if (k == n) {inl ()} {inr (f k, k+1)})
end

def generateC : Int -> (Int -> Cmd a) -> Cmd (Array a) = \n. \f.
  unfoldArrayC 0 (\k. if (k == n) {pure (inl ())} {a <- f k; pure (inr (a, k+1))})
end

def appendArray : Array a -> Array a -> Array a
  = \y. \z.
    let m = arraySize y in
    let n = arraySize z in
    generate (m + n) (\k. if (k < m) {indexArray y k} {indexArray z (k - m)})
end

// Spell out the type (rec l. Unit + a * l) instead of using the tydef List,
// so that downstream users can make use of it without needing to import the list module.
// Once we have the ability to 'export' tydefs, we should just re-export the List tydef instead.
def fromList : (rec l. Unit + a * l) -> Array a
  = \l. unfoldArray l ((\x.x) : List a -> Unit + a * List a)
end

def toList : Array a -> (rec l. Unit + a * l) = \arr.
  let n = arraySize arr
  in  unfoldr (\k. if (k == n) {inl ()} {inr (indexArray arr k, k+1)}) 0
end

// Merge sort

def merge : Array a -> Array a -> Array a = \ls. \rs.
  unfoldArray (0,0) (λmatch \i. \j.
    if (i >= arraySize ls && j >= arraySize rs) {inl ()}
    {if (i >= arraySize ls) {inr (indexArray rs j, (i,j+1))}
    {if (j >= arraySize rs) {inr (indexArray ls i, (i+1,j))}
    {if (indexArray ls i < indexArray rs j) {inr (indexArray ls i, (i+1,j))}
    {inr (indexArray rs j, (i,j+1))}}}}
  )
end

def mergeSortR : Int -> Int -> Array a -> Array a = \lo. \hi. \arr.
  if (hi == lo) { [| |] }
  { if (hi - lo == 1) { [| indexArray arr lo |] }
  { let mid = (lo + hi) / 2 in
    let ls = mergeSortR lo mid arr in
    let rs = mergeSortR mid hi arr in
    merge ls rs
  }}
end

def mergeSort : Array a -> Array a = \arr. mergeSortR 0 (arraySize arr) arr end

// Fisher-Yates shuffle

// Randomly shuffle the elements of an array via Fisher-Yates.
// Note, since we don't have mutable arrays, this is actually O(n^2).
def shuffle : Array a -> Cmd (Array a) = \arr.
  let n = arraySize arr in
  let set : Int -> Int -> (Int -> a) -> (Int -> a)
        = \i. \j. \f. \k. if (k == i) {f j} {f k}
  in
  unfoldArrayC (0, indexArray arr) (λmatch
    \i. \a.
      if (i == n)
        { pure (inl ()) }
        { j <- random (n-i);
          pure (inr (a (i+j), (i+1, set (i+j) i a)))
        }
  )
end
