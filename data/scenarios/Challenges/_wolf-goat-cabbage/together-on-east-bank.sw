def get_x_coord = \r.
    as r {
        pos <- whereami;
        return $ fst pos;
    };
    end;

def all_on_bank = \baseX. \robotName.
    r <- robotnamed robotName;
    thisX <- get_x_coord r;

    as r {
        try {grab; return ()} {};
        move;
        try {grab; return ()} {};
        move;
        try {grab; return ()} {};

        has_wolf <- has "wolf";
        has_goat <- has "goat";
        has_cabbage <- has "cabbage";
        return $ baseX == thisX && has_wolf && has_goat && has_cabbage;
    };
    end;

baseX <- get_x_coord base;

all_on_bank baseX "east_detector";