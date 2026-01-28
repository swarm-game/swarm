import "control"

tydef List a = rec l. Unit + a * l end

def index : Int -> List a -> a = \i. λcase
  (\_. fail "bad index")
  (λmatch \hd. \tl. if (i == 0) {hd} {index (i-1) tl})
end

def length : List a -> Int = λcase
  (\_. 0) (λmatch \_. \tl. 1 + length tl)
end

def sum : List Int -> Int = λcase
  (\_. 0)
  (λmatch \hd. \tl. hd + sum tl)
end

def for : Int -> (Int -> Cmd a) -> Cmd (List a) = \n. \k.
  if (n == 0)
    { pure $ inl () }
    { x <- k (n-1);
      xs <- for (n-1) k;
      pure (inr (x,xs))
    }
end

def foldM : List a -> b -> (b -> a -> Cmd b) -> Cmd b =
  \xs. \b. \f. case xs
    (\_. pure b)
    (λmatch \h. \t. b' <- f b h; foldM t b' f)
end

def any : (a -> Cmd Bool) -> List a -> Cmd Bool = \p. λcase
  (\_. pure false)
  (λmatch \hd. \tl. b <- p hd; if b {pure true} {any p tl})
end;

def mapM_ : (a -> Cmd b) -> List a -> Cmd Unit = \f. λcase
  (\_. pure ())
  (λmatch \hd. \tl. f hd; mapM_ f tl)
  end;
