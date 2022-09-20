def forever : {cmd unit} -> cmd unit =
  \c. force c ; forever c
end

// Wander randomly forever.
def wander : cmd unit =
  forever {
    b <- random 2;
    turn (if (b == 0) {left} {right});
    move
  }
end
