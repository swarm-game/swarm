def get_x_coord = \r.
    as r {
        pos <- whereami;
        return $ fst pos;
    };
    end;

def is_unattended_together = \baseX. \robotName.
    r <- robotnamed robotName;
    thisX <- get_x_coord r;

    if (baseX == thisX) {
        return false;
    } {
        as r {
            try {grab; return ()} {};
            move;
            try {grab; return ()} {};
            move;
            try {grab; return ()} {};

            has_wolf <- has "wolf";
            has_goat <- has "goat";
            has_cabbage <- has "cabbage";
            return $ (has_wolf && has_goat) || (has_goat && has_cabbage);
        };        
    }
    end;

baseX <- get_x_coord base;

west_bad <- is_unattended_together baseX "west_detector";
east_bad <- is_unattended_together baseX "east_detector";

return $ west_bad || east_bad;