def forever: ∀ a b. {Cmd a} -> Cmd b = \c. force c; forever c end

def x : Int -> Cmd a -> Cmd Unit = \n. \c.
  if (n == 0) {} {c; x (n-1) c}
end

def andC : Cmd Bool -> Cmd Bool -> Cmd Bool = \c1. \c2.
  b1 <- c1;
  if b1 {c2} {pure false}
end

tydef List a = rec l. Unit + a * l end

def for : Int -> (Int -> Cmd a) -> Cmd (List a) = \n. \k.
  if (n == 0)
    { pure $ inl () }
    { x <- k (n-1);
      xs <- for (n-1) k;
      pure (inr (x,xs))
    }
end

def readRow : Cmd (List (Unit + Text)) =
  r <- for 8 (\_. s <- scan down; move; pure s);
  turn back; x 8 move; turn right; move; turn right;
  pure r
end

tydef Rect = List (List (Unit + Text)) end

def readRect : Cmd Rect =
  lst <- for 4 (\_. readRow);
  turn right; x 4 move; turn left;
  pure lst
end

def checkCell : Unit + Text -> Cmd Bool = \pat.
  actual <- scan down;
  move;
  pure (actual == pat)
end

def λmatch = \f. \p. match p f end
def λcase = \f. \g. \s. case s f g end

def checkRow : List (Unit + Text) -> Cmd Bool = λcase
  (\_. turn back; x 8 move; turn right; move; turn right; pure true)
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

forever {judge};
