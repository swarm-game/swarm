def ifC = \c.\t.\e. b <-c; if b t e end
def for = \i.\e.\c. if (i < e) {c i; for (i + 1) e c} {} end

def x2 = \c. c;c; end
def x3 = \c. c;c;c; end
def x4 = \c. c;c;c;c end
def x5 = \c. c;c;c;c;c end
def x6 = \c. c;c;c;c;c;c end
def x7 = \c. c;c;c;c;c;c;c end
def x8 = \c. x4 c; x4 c end
def x16 = \c. x8 c; x8 c end
def x32 = \c. x16 c; x16 c end


/*************************************************************/
/*                  TURN ABBREVIATIONS                       */
/*************************************************************/
def tL = turn left end
def tR = turn right end
def tB = turn back end

def tN = turn north end
def tE = turn east end
def tS = turn south end
def tW = turn west end

def m1 : Cmd Unit = move; end
def m2 : Cmd Unit = move; m1; end
def m3 : Cmd Unit = move; m2; end
def m4 : Cmd Unit = move; m3; end
def m5 : Cmd Unit = move; m4; end
def m6 : Cmd Unit = move; m5; end
def m7 : Cmd Unit = move; m6; end
def m8 : Cmd Unit = move; m7; end
def m9 : Cmd Unit = move; m8; end
def m10 : Cmd Unit = move; m9; end
def m11 : Cmd Unit = move; m10; end
def m12 : Cmd Unit = move; m11; end
def m13 : Cmd Unit = move; m12; end
def m14 : Cmd Unit = move; m13; end
def m15 : Cmd Unit = move; m14; end
def m16 : Cmd Unit = move; m15; end
def m17 : Cmd Unit = move; m16; end
def m18 : Cmd Unit = move; m17; end
def m19 : Cmd Unit = move; m18; end
def m20 : Cmd Unit = move; m19; end
def m21 : Cmd Unit = move; m20; end
def m22 : Cmd Unit = move; m21; end
def m23 : Cmd Unit = move; m22; end
def m24 : Cmd Unit = move; m23; end
def m25 : Cmd Unit = move; m24; end
def m26 : Cmd Unit = move; m25; end
def m27 : Cmd Unit = move; m26; end
def m28 : Cmd Unit = move; m27; end
def m29 : Cmd Unit = move; m28; end
def m30 : Cmd Unit = move; m29; end
def m31 : Cmd Unit = move; m30; end
def m32 : Cmd Unit = move; m31; end
def m33 : Cmd Unit = move; m32; end
def m34 : Cmd Unit = move; m33; end
def m35 : Cmd Unit = move; m34; end
def m36 : Cmd Unit = move; m35; end
def m37 : Cmd Unit = move; m36; end
def m38 : Cmd Unit = move; m37; end
def m39 : Cmd Unit = move; m38; end
def m40 : Cmd Unit = move; m39; end
def m41 : Cmd Unit = move; m40; end
def m42 : Cmd Unit = move; m41; end
def m43 : Cmd Unit = move; m42; end
def m44 : Cmd Unit = move; m43; end
def m45 : Cmd Unit = move; m44; end
def m46 : Cmd Unit = move; m45; end
def m47 : Cmd Unit = move; m46; end
def m48 : Cmd Unit = move; m47; end
def m49 : Cmd Unit = move; m48; end
def m50 : Cmd Unit = move; m49; end
def m51 : Cmd Unit = move; m50; end
def m52 : Cmd Unit = move; m51; end
def m53 : Cmd Unit = move; m52; end
def m54 : Cmd Unit = move; m53; end
def m55 : Cmd Unit = move; m54; end
def m56 : Cmd Unit = move; m55; end
def m57 : Cmd Unit = move; m56; end
def m58 : Cmd Unit = move; m57; end
def m59 : Cmd Unit = move; m58; end
def m60 : Cmd Unit = move; m59; end
def m61 : Cmd Unit = move; m60; end
def m62 : Cmd Unit = move; m61; end
def m63 : Cmd Unit = move; m62; end
def m64 : Cmd Unit = move; m63; end
def m65 : Cmd Unit = move; m64; end
def m66 : Cmd Unit = move; m65; end
def m67 : Cmd Unit = move; m66; end
def m68 : Cmd Unit = move; m67; end
def m69 : Cmd Unit = move; m68; end
def m70 : Cmd Unit = move; m69; end
def m71 : Cmd Unit = move; m70; end
def m72 : Cmd Unit = move; m71; end
def m73 : Cmd Unit = move; m72; end
def m74 : Cmd Unit = move; m73; end
def m75 : Cmd Unit = move; m74; end
def m76 : Cmd Unit = move; m75; end
def m77 : Cmd Unit = move; m76; end
def m78 : Cmd Unit = move; m77; end
def m79 : Cmd Unit = move; m78; end
def m80 : Cmd Unit = move; m79; end
def m81 : Cmd Unit = move; m80; end
def m82 : Cmd Unit = move; m81; end
def m83 : Cmd Unit = move; m82; end
def m84 : Cmd Unit = move; m83; end
def m85 : Cmd Unit = move; m84; end
def m86 : Cmd Unit = move; m85; end
def m87 : Cmd Unit = move; m86; end
def m88 : Cmd Unit = move; m87; end
def m89 : Cmd Unit = move; m88; end
def m90 : Cmd Unit = move; m89; end
def m91 : Cmd Unit = move; m90; end
def m92 : Cmd Unit = move; m91; end
def m93 : Cmd Unit = move; m92; end
def m94 : Cmd Unit = move; m93; end
def m95 : Cmd Unit = move; m94; end
def m96 : Cmd Unit = move; m95; end
def m97 : Cmd Unit = move; m96; end
def m98 : Cmd Unit = move; m97; end
def m99 : Cmd Unit = move; m98; end
def m100 : Cmd Unit = move; m99; end

/*************************************************************/
/*                  SCOUTING & FETCHING                      */
/*************************************************************/

// generic fetch robot
def goFetchG : Cmd a -> Cmd b -> Cmd Actor = \movethere.\moveback.
  build {
    movethere;
    x <- grab;
    turn back;
    moveback;
    give base x;
  }
end

// generic scout robot with scanner (scan one ahead)
def goScanG : Cmd a -> Cmd b -> Cmd Actor = \movethere.\moveback.
  build {
    movethere;
    scan forward;
    turn back;
    moveback;
    upload base;
  }
end

// simple fetch - e.g. go north 52 steps and bring it back
def goFetch : Dir -> Cmd a -> Cmd Actor = \d.\m.
  goFetchG (turn d; m) m
end

// simple scout&scan
def goScan : Dir -> Cmd a -> Cmd Actor = \d.\m.
  goScanG (turn d; m) m
end

// helper fetch to go one direction and turn
def goFetchXY : Cmd b -> Cmd c -> Cmd e -> Cmd a -> Cmd d -> Cmd Actor = \tX.\tY.\antiX.
  \mN.\mE.
    goFetchG (tX; mN; tY; mE) (mE; antiX; mN)
end

// helper fetch to go one direction and turn
def goScanXY : Cmd b -> Cmd c -> Cmd e -> Cmd a -> Cmd d -> Cmd Actor = \tX.\tY.\antiX.
  \mN.\mE.
    goScanG (tX; mN; tY; mE) (mE; antiX; mN)
end

def gN : Cmd a -> Cmd Actor = goFetch forward end
def gE : Cmd a -> Cmd Actor = goFetch right end
def gS : Cmd a -> Cmd Actor = goFetch back end
def gW : Cmd a -> Cmd Actor = goFetch left end

def gNE : Cmd a -> Cmd b -> Cmd Actor = goFetchXY noop tR tL end
def gNW : Cmd a -> Cmd b -> Cmd Actor = goFetchXY noop tL tR end
def gEN : Cmd a -> Cmd b -> Cmd Actor = goFetchXY tR tL tR end
def gES : Cmd a -> Cmd b -> Cmd Actor = goFetchXY tR tR tL end
def gSE : Cmd a -> Cmd b -> Cmd Actor = goFetchXY tB tL tR end
def gSW : Cmd a -> Cmd b -> Cmd Actor = goFetchXY tB tR tL end
def gWS : Cmd a -> Cmd b -> Cmd Actor = goFetchXY tL tL tR end
def gWN : Cmd a -> Cmd b -> Cmd Actor = goFetchXY tL tR tL end


def scN : Cmd a -> Cmd Actor = goScan forward end
def scE : Cmd a -> Cmd Actor = goScan right end
def scS : Cmd a -> Cmd Actor = goScan back end
def scW : Cmd a -> Cmd Actor = goScan left end

def scNE : Cmd a -> Cmd b -> Cmd Actor = goScanXY noop tR tL end
def scNW : Cmd a -> Cmd b -> Cmd Actor = goScanXY noop tL tR end
def scEN : Cmd a -> Cmd b -> Cmd Actor = goScanXY tR tL tR end
def scES : Cmd a -> Cmd b -> Cmd Actor = goScanXY tR tR tL end
def scSE : Cmd a -> Cmd b -> Cmd Actor = goScanXY tB tL tR end
def scSW : Cmd a -> Cmd b -> Cmd Actor = goScanXY tB tR tL end
def scWS : Cmd a -> Cmd b -> Cmd Actor = goScanXY tL tL tR end
def scWN : Cmd a -> Cmd b -> Cmd Actor = goScanXY tL tR tL end

/*************************************************************/
/*                     PLANTING A FIELD                      */
/*************************************************************/

def giveAll = \a.\p. ifC (has p) {give a p; giveAll a p} {} end

def plantField = \plant.\initialTurn.\x.\y.
  r <- build {
    require "boat";
    initialTurn;
    y (
        turn right;
        x (move; ifC (ishere plant) {} {place plant}; harvest; pure ());
        turn back;
        x move;
        turn right;
        move
    );
    turn back;
    y move;
    giveAll base plant;
  };
  give r plant
end

/*************************************************************/
/*                           DRILL                           */
/*************************************************************/

// generic drill
def goDrillG : Cmd a -> Cmd b -> Cmd Actor = \movethere.\moveback.
  build {
    movethere;
    drill forward;
    turn back;
    moveback;
    upload base;
  }
end

// simple drill
def goDrill : Dir -> Cmd a -> Cmd Actor = \d.\m.
  goDrillG (turn d; m) m
end

// helper fetch to go one direction and turn
def goDrillXY : Cmd b -> Cmd c -> Cmd e -> Cmd a -> Cmd d -> Cmd Actor = \tX.\tY.\antiX.
  \mN.\mE.
    goDrillG (tX; mN; tY; mE) (mE; antiX; mN)
end

def dN : Cmd a -> Cmd Actor = goDrill forward end
def dE : Cmd a -> Cmd Actor = goDrill right end
def dS : Cmd a -> Cmd Actor = goDrill back end
def dW : Cmd a -> Cmd Actor = goDrill left end

def dNE : Cmd a -> Cmd b -> Cmd Actor = goDrillXY noop tR tL end
def dNW : Cmd a -> Cmd b -> Cmd Actor = goDrillXY noop tL tR end
def dEN : Cmd a -> Cmd b -> Cmd Actor = goDrillXY tR tL tR end
def dES : Cmd a -> Cmd b -> Cmd Actor = goDrillXY tR tR tL end
def dSE : Cmd a -> Cmd b -> Cmd Actor = goDrillXY tB tL tR end
def dSW : Cmd a -> Cmd b -> Cmd Actor = goDrillXY tB tR tL end
def dWS : Cmd a -> Cmd b -> Cmd Actor = goDrillXY tL tL tR end
def dWN : Cmd a -> Cmd b -> Cmd Actor = goDrillXY tL tR tL end
