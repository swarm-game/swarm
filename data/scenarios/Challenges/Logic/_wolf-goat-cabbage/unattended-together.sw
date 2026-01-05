def get_x_coord = \r.
    as r {
        pos <- whereami;
        match pos \x. \_.
        pure x;
    };
    end;

def is_unattended_together = \baseX. \robotName.
    r <- robotnamed robotName;
    thisX <- get_x_coord r;

    if (baseX == thisX) {
        pure false;
    } {
        as r {
            try {grab; pure ()} {};
            move;
            try {grab; pure ()} {};
            move;
            try {grab; pure ()} {};

            has_wolf <- has "wolf";
            has_goat <- has "goat";
            has_cabbage <- has "cabbage";
            pure $ (has_wolf && has_goat) || (has_goat && has_cabbage);
        };
    }
    end;

def go =
  baseX <- get_x_coord base;

  west_bad <- is_unattended_together baseX "west_detector";
  east_bad <- is_unattended_together baseX "east_detector";

  pure $ west_bad || east_bad;
end
