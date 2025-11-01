def go =
    pelletCount <- as base {count "pellet"};
    if (pelletCount > 90) {
        teleport self (0, -2);
        place "strawberry";
    } {go};
    end;

go;
selfdestruct;