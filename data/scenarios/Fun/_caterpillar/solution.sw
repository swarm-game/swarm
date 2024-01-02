def bindE = \f. \x. case x inL f end
def fmapE = \f. \x. case x inL (\y. inR $ f y) end

def nil : (a -> b -> b) ->
           b ->
           b = \_. \x. x end

def cons : a ->
           ((a -> b -> b) -> b -> b) ->
           (a -> b -> b) ->
           b ->
           b = \x. \xs. \f. \b. f x $ xs f b end

// Accumulator functions

def offsetCoords : (int * int) -> (int * int) -> (int * int) = \coord. \offset.
  (fst coord + fst offset, snd coord + snd offset)
  end

def getLength : ((a -> int -> int) -> int -> int) -> int = \list.
  list (\_. \y. 1 + y) 0
  end

def lastAccumulatorFunc : a -> (unit + a) -> (unit + a) = \x. \b.
  case b (\_. inR x) inR
  end

def getLast : ((a -> (unit + a) -> (unit + a)) -> (unit + a) -> (unit + a)) -> (unit + a) = \list.
  list lastAccumulatorFunc $ inL ()
  end

def initAccumulatorFunc : a ->
           unit + ((a -> b -> b) -> b -> b) ->
           unit + ((a -> b -> b) -> b -> b) = \x. \b.
  case b (\_. inR nil) (\y. inR $ cons x y)
  end

def getInit = \list.
  list initAccumulatorFunc $ inL ()
  end

// "pop" from the tail;
// returns the last element of the list and the truncated list.
def shift = \list.
  list (\x. \b. case b (\_. inR (x, nil)) (\y. inR (fst y, cons x $ snd y))) $ inL ()
  end


def getPassthrough = \list.
  list cons nil
  end



// Scenario-specific functions

def doAtLoc = \currLoc. \targetLoc. \func.
  teleport self targetLoc;
  func;
  teleport self currLoc;
  end;

def makeList = \i. \currentList.
  if (i > 0) {
    move;
    myLoc <- whereami;
    place "tail";
    
    newList <- makeList (i - 1) $ cons myLoc currentList;
    return newList;
  } {
    return currentList;
  };
  end

def go2 = \myList.


  wait 10;
  move;

  newLoc <- whereami;

  let maybeLastElem = getLast myList in
  case maybeLastElem return (\lastElem.
    log $ "Going to teleport to: " ++ format lastElem;
    doAtLoc newLoc lastElem grab;
  );


  // let listInit = getPassthrough myList in
  // log $ "List length: " ++ format (getLength listInit);

  // let shifted = shift myList in

  // FIXME: Uncommenting the following line causes a type error!
  // let oldRattleLoc = fst shifted in
  // let shiftedList = snd shifted in
  // doAtLoc newLoc oldRattleLoc grab;

  go2 $ cons newLoc myList;
  end;


def go =
  myList <- makeList 5 $ nil;

  let w = myList offsetCoords (0, 0) in
  let x = getLength myList in
  let y = getLast myList in
  let z = bindE getLast $ getInit myList in

  let shifted = shift myList in

  log "======";
  log $ format w;
  log $ format x;
  log $ format y;
  log $ format z;

  log "-------";
  log $ format $ fmapE fst shifted;
  log $ format $ fmapE fst $ bindE shift $ fmapE snd shifted;
  log $ format $ fmapE fst $ bindE shift $ fmapE snd $ bindE shift $ fmapE snd shifted;

  go2 myList;
  end;

go;