import "../../../../lib/control"

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

