def max = \a. \b.
    if (a > b) {a} {b};
    end;

def mod : Int -> Int -> Int = \i.\m.
    i - m * (i / m)
end

def abs = \n. if (n < 0) {-n} {n} end

def isEven = \n.
    mod n 2 == 0;
    end
