def tB = turn back end
def tR = turn right end
def tL = turn left end

def m = move end
def m2 = m;m end
def m3 = m;m;m end
def m4 = m2;m2 end
def m5 = m4;m end
def m8 = m4;m4 end
def m12 = m4;m8 end
def m26 = m12;m12;m2 end

def mg = m; grab end

def get_3_trees : Cmd Unit =
  tL; m; tL; m3; grab; tR; m; tL; mg; mg; tB; m5; tR; m2
end

def make_harvester : Cmd Unit =
  make "log"; make "log"; make "log";
  make "board"; make "board"; make "board";
  make "box";
  make "wooden gear"; make "wooden gear";
  make "harvester"
end

def get_lambda : Cmd Unit =
  m12; tL; m26; harvest; tB; m26; tR; m12
end

def solution : Cmd Unit =
  build {get_3_trees}; wait 24; salvage;
  make_harvester;
  build {get_lambda}; wait 100; salvage
end;
