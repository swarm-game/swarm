def m2 = move; move end
def m5 = m2; m2; move end
def m10 = m5; m5 end
def m20 = m10; m10 end

def give_fish = \n.
  if (n == 0) {}
  { mcat <- meet;
    case mcat (\_. give_fish n) (\cat. give cat "fish"; give_fish (n-1))
  }
end;

build {
  require 3 "fish";
  m2; turn left; m20;
  give_fish 3;
  turn back; m20; turn right; m2
}
