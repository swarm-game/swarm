def checkPath : cmd bool =
  loc <- as base {whereami};
  teleport self loc;
  checkPathRec 0
end

def checkPathRec : int -> cmd bool = \len.
  loc <- whereami;
  if (loc == (27, -17))
  { return (len == 50) }
  {
    
  }
end
