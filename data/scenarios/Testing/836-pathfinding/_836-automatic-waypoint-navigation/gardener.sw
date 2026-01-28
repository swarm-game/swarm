def harvestPlant =
  emptyHere <- isempty;
  if emptyHere {
    watch down;
    wait 1000;
  } {
    wait 50;
    harvest;
    pure ();
  };
  end;

def go =
    harvestPlant;
    go;
    end;
