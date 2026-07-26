def getLetterEntityByIndex = \idx.  /* one-based index */
    let letter = toChar $ idx - 1 + charAt 0 "a" in
    letter ++ "-tile";
    end;

def getOrdinal : Text -> Cmd Int = \item.
    count $ item ++ "-ordinal";
    end;

def getValueHere =
    item <- scan down;
    if (item == "") {pure 0} {getOrdinal item}
    end;

def itemIsHere = \item.
    found <- scan down;
    pure $ found == item;
    end;
