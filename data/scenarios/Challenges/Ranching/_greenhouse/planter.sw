def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def modulus = \a. \b.
    a - (b * (a/b));
    end;

def abs = \a.
    if (a < 0) {
        return $ a * -1;
    } {
        return a;
    }
    end;

def getMushroomByIndex = \i.
    if (i == 1) {"truffle"}
    $ elif (i == 2) {"portobello"}
    $ elif (i == 3) {"chanterelle"}
    $ else {"shiitake"}
    end;

// Auto-generated
def getFlowerbyIndex = \i.
    if (i == 1) {"aster"}
$ elif (i == 2) {"azalea"}
$ elif (i == 3) {"begonia"}
$ elif (i == 4) {"buttercup"}
$ elif (i == 5) {"chrysanthemum"}
$ elif (i == 6) {"clematis"}
$ elif (i == 7) {"coneflower"}
$ elif (i == 8) {"crocus"}
$ elif (i == 9) {"daffodil"}
$ elif (i == 10) {"dahlia"}
$ elif (i == 11) {"daisy"}
$ elif (i == 12) {"forget-me-not"}
$ elif (i == 13) {"forsythia"}
$ elif (i == 14) {"freesia"}
$ elif (i == 15) {"gardenia"}
$ elif (i == 16) {"geranium"}
$ elif (i == 17) {"gladiolus"}
$ elif (i == 18) {"hellebore"}
$ elif (i == 19) {"hibiscus"}
$ elif (i == 20) {"hydrangea"}
$ elif (i == 21) {"iris"}
$ elif (i == 22) {"jasmine"}
$ elif (i == 23) {"lavender"}
$ elif (i == 24) {"lilac"}
$ elif (i == 25) {"lily"}
$ elif (i == 26) {"lupine"}
$ elif (i == 27) {"marigold"}
$ elif (i == 28) {"morning glory"}
$ elif (i == 29) {"orchid"}
$ elif (i == 30) {"pansy"}
$ elif (i == 31) {"peony"}
$ elif (i == 32) {"poppy"}
$ elif (i == 33) {"primrose"}
$ elif (i == 34) {"rhododendron"}
$ elif (i == 35) {"rose"}
$ elif (i == 36) {"snapdragon"}
$ elif (i == 37) {"sunflower"}
$ elif (i == 38) {"thistle"}
$ elif (i == 39) {"tulip"}
$ else {"zinnia"}
;
    end;

def plantAllInRow = \fixedPlacement1. \fixedPlacement2. \requiredItem1. \requiredItem2. \i.
    if (i > 0) {
        let pseudoIdx = i - 1 in
        item <- (
            if (pseudoIdx == fixedPlacement1) {return requiredItem1;}
            $ elif (pseudoIdx == fixedPlacement2) {return requiredItem2;}
            $ else {
                r <- random 40;
                return $ getFlowerbyIndex r;
            }
        );

        create item;
        place item;
        move;

        plantAllInRow fixedPlacement1 fixedPlacement2 requiredItem1 requiredItem2 $ i - 1;
    } {};
    end;

def prepareEverything =
    pos <- whereami;
    yPos <- abs $ snd pos;

    let flowerVarietyCount = 40 in
    let rowLength = 8 in

    // Guarantee one specific flower occurrences per row
    // Probabilistically place a mushroom in a row while
    // avoiding the guaranteed flower.
    fixedFlowerPlacementPos <- random rowLength;

    // 50% chance of placing a mushroom
    rand0 <- random 2;

    mushroomPos <- if (rand0 == 0) {
        rand2 <- random $ rowLength - 1;
        let mushroomOffset = 1 + rand2 in
        let position = modulus (fixedFlowerPlacementPos + mushroomOffset) rowLength in
        return position;
    } {
        // The row iterator goes from 0 to N-1, so returning an index of N
        // will ensure that the mushroom will not be placed.
        return rowLength;
    };

    let requiredFlowerIdx = modulus yPos flowerVarietyCount in
    let requiredFlower = getFlowerbyIndex requiredFlowerIdx in

    requiredMushroomIdx <- random 4;
    let requiredMushroom = getMushroomByIndex requiredMushroomIdx in

    plantAllInRow fixedFlowerPlacementPos mushroomPos requiredFlower requiredMushroom rowLength;
    end;

def go =
    prepareEverything;
    place "bit (0)";
    end;

go;