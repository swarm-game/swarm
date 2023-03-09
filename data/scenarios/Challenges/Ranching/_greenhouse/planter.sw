
def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def getFlowerbyIndex : int -> cmd text = \i.
    return (if (i == 1) {"dahlia"}
    $ elif (i == 2) {"morning glory"}
    $ elif (i == 3) {"daisy"}
    $ elif (i == 4) {"rose"}
    $ elif (i == 5) {"marigold"}
    $ elif (i == 6) {"tulip"}
    $ elif (i == 7) {"sunflower"}
    $ elif (i == 8) {"thistle"}
    $ elif (i == 9) {"chrysanthemum"}
    $ elif (i == 10) {"iris"}
    $ else {"lily"}); // index 0
    end;

def plantAllInRow = \i.
    if (i >= 0) {

        item <- getFlowerbyIndex i;
        create item;
        place item;
        move;

        plantAllInRow $ i - 1;
    } {};
    end;

plantAllInRow 10;
    