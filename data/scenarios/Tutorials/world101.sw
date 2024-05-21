def tB = turn back end
def tR = turn right end
def tL = turn left end

def m = move end
def m2 = m;m end
def m4 = m2;m2 end
def m8 = m4;m4 end
def m9 = m8;m end
def m10 = m8;m2 end

def mg = m; grab end

def get_3_trees : Cmd Unit =
  tB; m; mg; mg; mg; tB; m4
end

def make_harvester : Cmd Unit =
  make "log"; make "log"; make "log";
  make "board"; make "board"; make "board";
  make "box";
  make "wooden gear"; make "wooden gear";
  make "harvester"
end

def get_lambda : Cmd Unit =
  m10; tR; m9; harvest; tB; m9; tL; m10
end

def solution : Cmd Unit =
  build {get_3_trees}; wait 16; salvage;
  make_harvester;
  build {get_lambda}; wait 50; salvage
end;

solution
