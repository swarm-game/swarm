def hasMetBase = \r.
    let basename = "base" in
    x <- as r {whoami};
    if (x == basename) {
        return false;
    } {
        mr0 <- as r {meet};
        case mr0
          (\_. return false)
          (\bot. name <- as bot {whoami}; return $ name == basename);
    };
    end;

/**
Iterates sequentially until
encountering an invalid robot index.

Distinguishes system bots from the base by name.
Returns true if a bot has "met" the base.
*/
def anyHasMetBase : int -> cmd bool = \idx.
    
    try {
        bot <- robotnumbered idx;
        intermediate <- hasMetBase bot;
        let foo = intermediate in
        let newIdx = idx + 1 in
        recursiveResult <- anyHasMetBase newIdx;
        return $ foo || recursiveResult;
    } {
      // Terminates the recursion on the
      // lowest index at which a robot does not exist
      return false;
    };
    end;

anyHasMetBase 1;