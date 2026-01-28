def id : a -> a = \t. t end

def compose : (b -> c) -> (a -> b) -> a -> c =
  \f. \g. \x. f (g x)
end
