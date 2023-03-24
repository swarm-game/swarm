/* Algorithm:
If the player was previously on one side of me upon last inspection
and is now on top of me or on the other side, teleport them across
the map.
*/

def isBeyond = \func.
    basePos <- as base {
        whereami;
    };
    return $ func $ fst basePos;
    end;

def observeAndTeleport = \criteriaFunc. \telporterPos. \wasExceeded.
    currentlyExceeded <- isBeyond criteriaFunc;

    // Boundary was crossed
    if (currentlyExceeded && not wasExceeded) {
        teleport base (-(fst telporterPos), snd telporterPos);
        create "bit (0)";
    } {};

    observeAndTeleport criteriaFunc telporterPos currentlyExceeded;
    end;

def init =
    telporterPos <- whereami;
    let teleX = fst telporterPos in
    let isWesternTeleporter = teleX < 0 in

    criteriaFunc <- if isWesternTeleporter {
        return $ \baseX. baseX < teleX;
    } {
        return $ \baseX. baseX > teleX;
    };

    initiallyBeyond <- isBeyond criteriaFunc;
    observeAndTeleport criteriaFunc telporterPos initiallyBeyond;
    end;

init;