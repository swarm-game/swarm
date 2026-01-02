import "control"

def foldM : (rec l. Unit + a * l) -> b -> (b -> a -> Cmd b) -> Cmd b =
  \xs. \b. \f. case xs
    (\_. pure b)
    (λmatch \h. \t. b' <- f b h; foldM t b' f)
end
