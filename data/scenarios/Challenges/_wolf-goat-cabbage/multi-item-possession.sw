has_wolf <- has "wolf";
has_goat <- has "goat";
has_cabbage <- has "cabbage";
return $ (has_wolf && has_goat) || (has_goat && has_cabbage) || (has_wolf && has_cabbage);