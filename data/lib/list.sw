import "control"

tydef List a = rec l. Unit + a * l end

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
