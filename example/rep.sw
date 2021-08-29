let rep : int -> cmd () -> cmd () =
    \n.\c.
      if (n == 0)
        {}
        {c ; rep (n-1) c}
in
  rep 10 move
