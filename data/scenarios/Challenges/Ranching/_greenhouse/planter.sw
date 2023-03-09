
def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def getFlowerbyIndex : int -> cmd text = \i.
    return (if (i == 1) {"chrysanthemum"}
    $ elif (i == 2) {"dahlia"}
    $ elif (i == 3) {"daisy"}
    $ elif (i == 4) {"iris"}
    $ elif (i == 5) {"marigold"}
    $ elif (i == 6) {"morning glory"}
    $ elif (i == 7) {"rose"}
    $ elif (i == 8) {"sunflower"}
    $ elif (i == 9) {"thistle"}
    $ elif (i == 10) {"tulip"}
    $ elif (i == 11) {"hydrangea"}
    $ elif (i == 12) {"pansy"}
    $ elif (i == 13) {"azalea"}
    $ elif (i == 14) {"daffodil"}
    $ elif (i == 15) {"peony"}
    $ elif (i == 16) {"poppy"}
    $ elif (i == 17) {"hibiscus"}
    $ elif (i == 18) {"lupine"}
    $ elif (i == 19) {"zinnia"}
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

plantAllInRow 19;
    