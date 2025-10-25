def forever: ∀ a b. {Cmd a} -> Cmd b = \c. force c; forever c end

def x : Int -> Cmd a -> Cmd Unit = \n. \c.
  if (n == 0) {} {c; x (n-1) c}
end

def andC : Cmd (e + Unit) -> Cmd (e + Unit)  -> Cmd (e + Unit) = \c1. \c2.
  b1 <- c1;
  case b1 (\e. pure (inl e)) (\_. c2)
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

def readRow : Int -> Cmd (List Bool) = \c.
  r <- for c (\_. s <- isempty; move; pure s);
  turn back; x c move; turn right; move; turn right;
  pure r
end

tydef Rect = List (List Bool) end

def readRect : Int -> Int -> Cmd Rect = \r. \c.
  lst <- for r (\_. readRow c);
  turn right; x r move; turn left;
  pure lst
end

def checkCell : Bool -> Cmd ((Text * Int * Int) + Unit) = \pat.
  mactual <- scan down;
  move;
  loc <- whereami;
  pure $ case mactual (\_. inl ("empty", loc)) (\actual.
    if (actual == (if pat {"pixel (B)"} {"pixel (W)"})) {inr ()} {inl (actual, loc)}
  )
end

def λmatch = \f. \p. match p f end
def λcase = \f. \g. \s. case s f g end

def checkRow : Int -> List Bool -> Cmd ((Text * Int * Int) + Unit) = \c. λcase
  (\_. turn back; x c move; turn right; move; turn right; pure (inr ()))
  (λmatch \hd. \tl. andC (checkCell hd) (checkRow c tl))
end

def checkRect : Int -> Int -> Rect -> Cmd ((Text * Int * Int) + Unit) = \r. \c. λcase
  (\_. pure (inr ()))
  (λmatch \hd. \tl. andC (checkRow c hd) (checkRect r c tl))
end

def check : Int -> Int -> Rect -> Cmd Unit = \r. \c. \rect.
  log "check!";
  b <- checkRect r c rect;
  case b
    (\errLoc. log ("Incorrect location: " ++ format errLoc))
    (\_. log "Correct!"; create "gold");
end

def judge =
  let r = 6 in
  let c = 8 in
  instant {
    loc <- whereami;
    match loc \locx. \locy.
    for r (\y.
      for c (\x.
        surveil (locx + x, locy + y)
      )
    );
  };
  wait 1024;
  instant {
    origLoc <- whereami;
    teleport self (0, -5);
    rect <- readRect r c;
    teleport self origLoc; turn east;
    check r c rect;
    teleport self origLoc; turn east;
  }
end

forever {judge};
