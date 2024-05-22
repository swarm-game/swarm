def forever : {Cmd Unit} -> Cmd Unit =
  \c. force c ; forever c
end

// Wander randomly forever.
def wander : Cmd Unit =
  forever {
    b <- random 2;
    turn (if (b == 0) {left} {right});
    move
  }
end
