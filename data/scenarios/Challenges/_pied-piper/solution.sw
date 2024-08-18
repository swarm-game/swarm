def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def makeOatsCrumb = \spacing.
  place "oats";
  doN spacing move;
  end;


def waitUntilDisappeared =
  found <- scout north;
  if found {
      wait 1;
      waitUntilDisappeared;
  } {};
  end;

/**
Place a trail of breadcrumbs, and return
one crumb short of the first crumb.
*/
def makeOatsTrail = \spacing. \segments.
  doN segments $ makeOatsCrumb spacing;
  turn back;
  doN ((segments - 1)*spacing) move;
  turn back;
  watch down;
  wait 2000;
  end;

def go =
  let spacing = 4 in
  let segments = 5 in
  makeOatsTrail spacing segments;
  // Has probably awakened due to rat eating food here.
  waitUntilDisappeared;
  makeOatsTrail spacing $ segments;
  end;

go;