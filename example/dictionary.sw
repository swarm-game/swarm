tydef Dict k v = rec b. Unit + [k: k, v: v, l: b, r: b] end

def emptyD : Dict k v = inl () end

def isEmptyD : Dict k v -> Bool = \d. d == emptyD end

def containsD : k -> Dict k v -> Bool = \x.\d.
  case d (\_. false) (\n.
    if (x == n.k) { true } {
        if (x < n.k) {
            containsD x n.l
        } {
            containsD x n.r
        }
    }
  )
end

def insertD : k -> v -> Dict k v -> Dict k v = \x.\y.\d.
  case d (\_. inr [k=x, v=y, l=inl (), r=inl ()]) (\n.
    if (x == n.k) { inr [k=x, v=y, l=n.l, r=n.r] } {
        if (x < n.k) {
            inr [k=n.k, v=n.v, l=insertD x y n.l, r=n.r]
        } {
            inr [k=n.k, v=n.v, l=n.l, r=insertD x y n.r]
        }
    }
  )
end

// helper function to remove the leftmost element from right subtree
def pullLeft: [k: k, v: v, l: Dict k v, r: Dict k v] -> ((k * v) * (Dict k v)) = \n.
  case (n.l) (\_. ((n.k,n.v),n.r)) (\lt. 
    let res = pullLeft lt in
    (fst res, inr [k=n.k, v=n.v, l=snd res, r=n.r])
  )
end

def deleteD : k -> Dict k v -> Dict k v = \x.\d.
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
