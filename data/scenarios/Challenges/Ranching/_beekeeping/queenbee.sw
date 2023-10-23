// Spawns worker bees when structures are detected

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def mod : int -> int -> int = \a. \b. a - (a/b)*b end;

def workerProgram = \hiveIdx. \structureLoc.
    foundStructure <- structure "beehive" hiveIdx;
    let stillHasStructure = case foundStructure (\_. false) (\fs.
        structureLoc == snd fs;
    ) in

    if (stillHasStructure) {
        doN 10 (move; wait 4;);
        turn back;
        doN 10 (move; wait 4;);
        workerProgram hiveIdx structureLoc;
    } {
        selfdestruct;
    }
    end;

def workerProgramInit = \hiveIdx. \structureLoc.
    teleport self structureLoc;
    appear "8";
    if (mod hiveIdx 2 == 0) {turn left;} {};
    workerProgram hiveIdx structureLoc;
    end;

def observeHives = \lastHiveCount.

    foundStructure <- structure "beehive" lastHiveCount;
    newHiveCount <- case foundStructure (\_. return lastHiveCount) (\fs. 
        let newHiveCount = fst fs in

        if (newHiveCount > lastHiveCount) {
            // Build worker bee, assign ID, location
            build {workerProgramInit lastHiveCount $ snd fs};
            return ();
        } {};

        return newHiveCount;
    );

    wait 1;
    observeHives newHiveCount;
    end;

def go =
    observeHives 0;
    end;

go;
