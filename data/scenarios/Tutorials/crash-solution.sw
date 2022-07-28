crasher <- build {
    turn east; move; move; move; log "bye"; move
};
wait 32;
salvager <- build {
    log "I will bring home the Win!";
    turn east; move; move; move; salvage; turn back; move; move; give base "Win"
};