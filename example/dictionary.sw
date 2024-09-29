tydef Dict k v = rec b. Unit + (k * v * b * b) end

def emptyD : Dict k v = inl () end

def isEmptyD : Dict k v -> Bool = \d. case d (\_. true) (\_. false) end

def containsD : k -> Dict k v -> Bool = \x.\d.
  case d (\_. false) (\n.
    let k = fst n in
    if (x == k) { true } {
        let vbb = snd n in
        let v = fst vbb in
        let bb = snd vbb in
        if (x < k) {
            containsD x (fst bb)
        } {
            containsD x (snd bb)
        }
    }
  )
end

def insertD : k -> v -> Dict k v -> Dict k v = \x.\y.\d.
  case d (\_. inr (x,y,inl (), inl ())) (\n.
    let k = fst n in
    let vbb = snd n in
    let v = fst vbb in
    let bb = snd vbb in
    if (x == k) { inr (x,y,bb) } {
        if (x < k) {
            inr (k, v, insertD x y (fst bb), snd bb)
        } {
            inr (k, v, fst bb, insertD x y (snd bb))
        }
    }
  )
end

// helper function to remove the leftmost element from right subtree
def pullLeft: k * v * (Dict k v) * (Dict k v) -> ((k * v) * (Dict k v)) = \n.
  let k = fst n in
  let vbb = snd n in
  let v = fst vbb in
  let bb = snd vbb in
  case (fst bb) (\_. ((k,v),snd bb)) (\lt. 
    let res = pullLeft lt in
    let rkv = fst res in
    let rd = snd res in
    (rkv, inr (k,v,rd, snd bb))
  )
end

def deleteD : k -> Dict k v -> Dict k v = \x.\d.
  case d (\_. inl ()) (\n.
    let k = fst n in
    let vbb = snd n in
    let v = fst vbb in
    let bb = snd vbb in
    if (x == k) {
        case (fst bb) (\_. snd bb) (\lt.
            case (snd bb) (\_. fst bb) (\rt.
                let r_kvd = pullLeft rt in
                let r_kv = fst r_kvd in
                inr (fst r_kv, snd r_kv, inr lt, snd r_kvd)
            )
        )
    } {
        if (x < k) {
            inr (k, v, deleteD x (fst bb), snd bb)
        } {
            inr (k, v, fst bb, deleteD x (snd bb))
        }
    }
  )
end
