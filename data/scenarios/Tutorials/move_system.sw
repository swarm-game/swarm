def until = \c. b <- c; if b {} {until c} end;

// name format: NA Entity
// N - one digit room number
// A one letter action
nameCheck <- atomic (
    name <- whoami;
    check <- robotNamed ("check" ++ fst (split 1 name));
    return (name, check)
);
let a = fst $ split 1 $ snd $ split 1 $ fst nameCheck in
let e = snd $ split 3 $ fst nameCheck in

until (as (snd nameCheck) {has "Win"});

if (a == "S") {
    if (e != "") { create e } {};
    swap e;
    return ()
} { if (a == "G") {
    grab;
    return ()
} { if (a == "P") {
    if (e != "") { create e } {};
    place e
} {
    say $ "Finished waiting for check but I don't know what to do: '" ++ a ++ "'"
}}}
