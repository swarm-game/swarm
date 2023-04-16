// Proof of concept illustrating the possibility of key
// handlers that process multi-key sequences.

def cons : a * b -> (a -> b) -> (a -> b) = \p. \k. \a.
  if (a == fst p) {snd p} {k a}
end

def nil : a -> cmd unit = \a. return () end

// The delay around the first argument is necessary to prevent
// infinite recursion
def handlerB : {key -> cmd unit} -> key -> cmd unit = \hA. \k.
  cons (key "b", move) nil k;
  installKeyHandler "" (force hA)
end

// Typing 'a' then 'b' in sequence will cause the robot to move.
def handlerA : key -> cmd unit =
  cons (key "a", installKeyHandler "" (handlerB {handlerA})) nil
end
