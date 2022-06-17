def forever : {cmd ()} -> cmd () =
  \c. force c ; forever c
end

// Wander randomly forever.
def wander : cmd () =
  forever {
    b <- random 2;
    turn (if (b == 0) {left} {right});
    move
  }
end
