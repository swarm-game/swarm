// Defining simple recursive functions.

def repeat : Int -> Cmd Unit -> Cmd Unit = \n.\c.
  if (n == 0) {} {c ; repeat (n-1) c}
end

def fact : Int -> Int = \n:Int.
  if (n == 0)
    {1}
    {n * fact (n-1)}
end

def gofar = repeat (fact 4) move end
