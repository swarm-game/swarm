import "../../../../lib/control"
import "../../../../lib/list"

def readRow : Cmd (List (Unit + Text)) =
  r <- for 8 (\_. s <- scan down; move; pure s);
  turn back; doN 8 move; turn right; move; turn right;
  pure r
end

tydef Rect = List (List (Unit + Text)) end

def readRect : Cmd Rect =
  lst <- for 4 (\_. readRow);
  turn right; doN 4 move; turn left;
  pure lst
end

def checkCell : Unit + Text -> Cmd Bool = \pat.
  actual <- scan down;
  move;
  pure (actual == pat)
end

def checkRow : List (Unit + Text) -> Cmd Bool = λcase
  (\_. turn back; doN 8 move; turn right; move; turn right; pure true)
  (λmatch \hd. \tl. andC (checkCell hd) (checkRow tl))
end

def checkRect : Rect -> Cmd Bool = λcase
  (\_. pure true)
  (λmatch \hd. \tl. andC (checkRow hd) (checkRect tl))
end

def check : Rect -> Cmd Unit = \rect.
  log "check!";
  origLoc <- whereami;
  teleport self (53, -8);
  b <- checkRect rect;
  if b {create "X"} {};
  teleport self origLoc; turn east;
end

def judge =
  instant {
    loc <- whereami;
    match loc \locx. \locy.
    for 4 (\y.
      for 8 (\x.
        surveil (locx + x, locy + y)
      )
    );
  };
  wait 1024;
  instant {
    rect <- readRect;
    check rect;
  }
end

def go = forever judge end
