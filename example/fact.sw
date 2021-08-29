let repeat : int -> cmd () -> cmd () = \n.\c.
  if (n == 0) {} (c ; repeat (n-1) c)
in let fact : int -> int = \n:int.
  if (n == 0)
    1
    (n * fact (n-1))
in move; move; repeat (fact 4) move