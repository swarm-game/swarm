def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def pillagePlanters =
    doN 20 (grab; move);

    turn left;
    doN 2 move;
    turn left;
    doN 4 move;

    doN 20 (grab; move);
    end;

bot <- robotnamed "solutionchecker";
as bot {
    isUndisturbed <- ishere "planter box receptacle";
    if isUndisturbed {return false} {
        try {
            pillagePlanters;
            // This will fail if we do not possess
            // every variety of flower: 
            make "flower pouch";
            return true;
        } {
            return false;
        };
    };
};
