def hasMetBase = \r.
    let basename = "base" in
    x <- as r {whoami};
    if (x == basename) {
        pure false;
    } {
        mr0 <- as r {meet};
        case mr0
          (\_. pure false)
          (\bot. name <- as bot {whoami}; pure $ name == basename);
    };
    end;

/**
Iterates sequentially until
encountering an invalid robot index.

Distinguishes system bots from the base by name.
Returns true if a bot has "met" the base.
*/
def anyHasMetBase : Int -> Cmd Bool = \idx.

    try {
        bot <- robotnumbered idx;
        intermediate <- hasMetBase bot;
        let foo = intermediate in
        let newIdx = idx + 1 in
        recursiveResult <- anyHasMetBase newIdx;
        pure $ foo || recursiveResult;
    } {
      // Terminates the recursion on the
      // lowest index at which a robot does not exist
      pure false;
    };
    end;
