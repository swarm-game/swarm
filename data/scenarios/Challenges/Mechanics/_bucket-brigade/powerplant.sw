def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def flashLights =
    doN 8 (
        move;
        wait 8;
        turn back;
    );
    end;

def checkForEnergy = \lastCount.
    hauler <- robotnamed "hauler";
    energyCount <- as hauler {
        count "energy";
    };
    if (energyCount > lastCount) {flashLights} {};
    checkForEnergy energyCount;
    end;

checkForEnergy 0;