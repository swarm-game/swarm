/* Algorithm:
If, at any point, the base is more than two cells from its previous location,
it must have teleported.
*/

def abs = \n. if (n<0) {-n} {n} end;

def getBasePos =
    as base {whereami};
    end;

def go = \lastBasePos.
    wait 1;
    curBasePos <- getBasePos;
    let deltaX = abs(fst curBasePos - fst lastBasePos) in
    if (deltaX > 1) {
        create "bit (0)";
    } {go curBasePos};
    end;

curBasePos <- getBasePos;
go curBasePos;