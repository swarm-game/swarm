import "~swarm/lib/control"

def waitForMap =
    mapPieceCount <- count "map piece";
    if (mapPieceCount < 2) {
        wait 1;
        waitForMap;
    } {};
    end;

def randomReverse =
    x <- random 2;
    if (x == 0) {
        turn back;
    } {}
    end;

def goToTreasure = \dirMin. \dirMax.
    let randAmplitude = dirMax - dirMin in

    xRand <- random randAmplitude;
    let xDist = dirMin + xRand in
    randomReverse;
    doN xDist move;

    turn left;

    yRand <- random randAmplitude;
    let yDist = dirMin + yRand in
    randomReverse;
    doN yDist move;

    place "bitcoin";
    end;

def go =
    waitForMap;
    goToTreasure 10 40;
    end;
