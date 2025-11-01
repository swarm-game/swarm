def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

// Grab the empty water tank and crucible
move;

// get the map
grab;


// get the peat
doN 8 move;
doN 10 (move; drill forward);
turn right;
doN 2 move;
turn right;
doN 21 move;
turn right;
doN 2 move;

// get the water tank
grab;

// get the clay
turn left;
doN 3 move;

intersperse 8 move grab;
make "crucible";
turn right;

doN 14 move;
turn right;
doN 4 move;
turn left;

// Start working on the lava
move;
drill forward;
turn back;
doN 2 move;
turn left;
doN 30 move;
drill forward;
turn back;
doN 30 move;
turn right;
doN 2 move;
drill forward;
turn right;
move;
turn left;
drill forward;

turn back;
doN 2 move;
turn left;
doN 30 move;
drill forward;
turn back;
doN 31 move;
turn right;
doN 3 move;
drill forward;

drill right;
turn back;
doN 3 move;
turn left;
doN 32 move;
drill forward;
turn back;
doN 32 move;
turn right;
doN 4 move;
drill forward;

drill right;
turn back;
doN 4 move;
turn left;
doN 33 move;
drill forward;
turn back;
doN 33 move;
turn right;
doN 5 move;

// Since there are two recipes that take in lava
// one will (according to the seed) fill the crucible,
// and the next will tranform into obsidian.
drill forward;
drill forward;
turn back;
doN 5 move;
turn left;
doN 34 move;

def meltRoundTrip = \hDist.

    drill forward;
    // dump out the pail
    make "empty water tank";
    turn back;
    doN hDist move;
    turn right;
    doN 2 move;
    drill forward;
    turn back;
    doN 2 move;
    turn left;
    doN (hDist + 1) move;
    end;


meltRoundTrip 28;
meltRoundTrip 29;
meltRoundTrip 30;
meltRoundTrip 31;
meltRoundTrip 32;
meltRoundTrip 33;


drill forward;
// dump out the pail
make "empty water tank";

doN 7 move;
turn left;
doN 5 move;
turn left;
doN 8 move;
turn right;
doN 6 move;
// Get 3 iron ores
doN 3 (drill forward);
// Make 6 iron plates
doN 3 (make "iron plate");

turn back;
doN 6 move;
turn left;
doN 8 move;
turn right;
doN 5 move;
turn right;
doN 43 move;


// Harvest hemp for rope

def uTurn = \d.
    turn d;
    move;
    turn d;
    end;

def harvestHempField =
    turn left;
    intersperse 5 move harvest;
    uTurn right;
    intersperse 5 move harvest;
    uTurn left;
    intersperse 5 move harvest;
    uTurn right;
    intersperse 5 move harvest;
    uTurn left;
    intersperse 5 move harvest;
    uTurn right;
    intersperse 5 move harvest;
    uTurn left;
    intersperse 5 move harvest;
    uTurn right;
    intersperse 5 move harvest;
    uTurn left;
    intersperse 5 move harvest;
    end;

def repositionToHempCorner =
    turn left;
    doN 8 move;
    turn left;
    doN 4 move;
    turn left;
    end;

// Default growth stage time is 100-200. There are two growth stages.
intersperse 3 (repositionToHempCorner; wait 400) harvestHempField;
doN 2 (make "rope");
make "rubble skip";

// Go to the quarry
turn left;
doN 4 move;
turn right;
doN 3 move;
drill forward;

def causewayTrek = \dist.
    turn back;
    doN dist move;
    drill forward;
    turn back;
    doN dist move;
    drill forward;
    end;

causewayTrek 18;
causewayTrek 19;
causewayTrek 20;
causewayTrek 21;

def secondCauseway = \dist.
    turn back;
    doN 24 move;
    turn right;
    doN dist move;
    drill forward;
    turn back;
    doN dist move;
    turn left;
    doN 24 move;
    drill forward;
    end;

secondCauseway 4;
secondCauseway 5;
secondCauseway 6;
secondCauseway 7;


def thirdCausewayHalf = \dist.
    turn back;
    doN 24 move;
    turn right;
    doN 11 move;
    turn left;
    move;
    turn right;
    doN dist move;
    drill forward;
    end;

def thirdCauseway = \dist.
    thirdCausewayHalf dist;
    turn back;
    doN dist move;
    turn left;
    move;
    turn right;
    doN 11 move;
    turn left;
    doN 24 move;
    drill forward;
    end;


thirdCauseway 1;
thirdCauseway 2;
thirdCauseway 3;
thirdCauseway 4;
thirdCauseway 5;

thirdCausewayHalf 6;

// Navigate through the mountain peaks
doN 5 move;
turn left;
doN 2 move;
turn right;
doN 4 move;
turn right;
move;

// Make the rope
make "rope";
// Install the rope
drill forward;
// Abseil down the cliff
doN 2 move;

turn left;
doN 7 move;
turn left;
move;
turn right;
move;
turn left;
move;
turn right;
doN 5 move;
turn right;
doN 2 move;

// Make the rope
make "rope";
// Install the rope
drill forward;
// Abseil down the cliff
doN 2 move;
turn left;
doN 4 move;
turn right;
move;
// Get the machete
grab;

turn back;
move;
turn left;
doN 4 move;
turn right;
doN 4 move;
turn left;
doN 5 move;
turn left;
move;
turn right;
move;
turn left;
move;
turn right;
doN 7 move;
turn right;
doN 3 move;
turn left;
doN 5 move;
turn left;
doN 2 move;
turn right;
doN 9 move;
turn left;
move;
turn right;
doN 10 move;

// harvest the palm tree
turn right;
move;
p <- harvest;
turn back;
move;
turn right;
doN 2 move;

turn left;
doN 17 move;
turn left;

def sowSeed = \p.
   place p;
   _p <- harvest;
   move;
   end;
// Harvest palm trees,
// traverse bog,
// grab flower.

def sowAndHarvestRow = \item. \length. \waitTime.

    doN length $ sowSeed item;
    turn back;
    doN length move;
    turn back;
    wait waitTime;
    doN length (grab; move;);
    end;


doN 9 move;

sowAndHarvestRow p 5 400;

// Head toward the bog
doN 16 move;
turn right;
doN 3 move;

// Make coconuts and flimsy boards
doN 6 (make "coconut"; make "flimsy board");

doN 2 (drill forward; move;);
turn left;
doN 4 (drill forward; move;);
move;

def navigateToFlower =
    turn right;
    doN 2 move;
    turn left;
    doN 4 move;
    turn right;
    move;
    turn left;
    doN 2 move;
    turn left;
    doN 2 move;
    turn right;
    doN 2 move;
    turn right;
    doN 2 move;
    turn left;
    doN 3 move;
    turn right;
    move;
    turn left;
    doN 2 move;
    turn left;
    move;
    grab;
    end;

flower <- navigateToFlower;

def exitBog =
    turn back;
    move;
    turn right;
    doN 2 move;
    turn right;
    move;
    turn left;
    doN 3 move;
    turn right;
    doN 2 move;
    turn left;
    doN 2 move;
    turn left;
    doN 2 move;
    turn right;
    doN 2 move;
    turn right;
    doN 2 move;
    turn left;
    doN 4 move;
    turn right;
    move;
    turn left;
    doN 5 move;
    turn right;
    doN 3 move;
    end;

exitBog;

turn left;

// Flower growth stage is 50 (two stages)
sowAndHarvestRow flower 7 100;

doN 23 move;
turn right;
doN 13 move;
turn right;

// Head toward the jungle
doN 16 move;
doN 8 (drill forward; move);

doN 2 move;
turn left;
scan forward;
turn right;
move;
turn left;

// Plant flower ring
intersperse 4 (turn left) (doN 2 (place flower; move));
turn back;
move;
turn right;
move;
wait 20;
plate <- grab;

turn back;
move;
turn right;
doN 26 move;
turn left;
doN 17 move;
turn left;
doN 11 move;
turn right;
doN 7 move;
turn right;
doN 2 move;
place plate;
