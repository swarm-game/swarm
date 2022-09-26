// Defining simple recursive functions.

def repeat : int -> cmd unit -> cmd unit = \n.\c.
  if (n == 0) {} {c ; repeat (n-1) c}
end

def fact : int -> int = \n:int.
  if (n == 0)
    {1}
    {n * fact (n-1)}
end

def gofar = repeat (fact 4) move end
