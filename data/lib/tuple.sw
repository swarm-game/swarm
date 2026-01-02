import "control"

def mapTuple = \f. λmatch \x. \y. (f x, f y) end;

def sumTuples = λmatch \t11. \t12. λmatch \t21. \t22.
    (t11 + t21, t12 + t22);
    end;
