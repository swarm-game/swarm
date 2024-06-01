tydef Maybe a = Unit + a end

def just : a -> Maybe a = inr end

def nothing : Maybe a = inl () end

def positive : Int -> Maybe Int = \x.
  if (x > 0) {just x} {nothing}
end
