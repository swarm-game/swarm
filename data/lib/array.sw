import "list"

// Generate an array from a size and a function mapping indices to array values.
def generate : Int -> (Int -> a) -> Array a
  = \n. \f. unfoldArray (\k. if (k == n) {inl ()} {inr (f k, k+1)}) 0
end

def appendArray : Array a -> Array a -> Array a
  = \y. \z.
    let m = size y in
    let n = size z in
    generate (m + n) (\k. if (k < m) {index y k} {index z (k - m)})
end

// Spell out the type (rec l. Unit + a * l) instead of using the tydef List,
// so that downstream users can make use of it without needing to import that tydef.
// Once we have 'export' we should just re-export the List tydef instead.
def fromList : (rec l. Unit + a * l) -> Array a
  = unfoldArray ((\x.x) : List a -> Unit + a * List a)
end

def toList : Array a -> (rec l. Unit + a * l) = \arr.
  let n = size arr
  in  unfoldr (\k. if (k == n) {inl ()} {inr (index arr k, k+1)}) 0
end
