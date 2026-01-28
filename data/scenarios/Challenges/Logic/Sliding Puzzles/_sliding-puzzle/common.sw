def getLetterEntityByIndex = \idx.  /* one-based index */
    let letter = toChar $ idx - 1 + charAt 0 "a" in
    letter ++ "-tile";
    end;

def getOrdinal : Text -> Cmd Int = \item.
    count $ item ++ "-ordinal";
    end;

def getValueHere =
    maybeItem <- scan down;
    ordNum <- case maybeItem (\_. pure 0) getOrdinal;
    end;

def itemIsHere = \item.
    x <- scan down;
    case x (\_. pure false) (\found. pure $ found == item);
    end;
