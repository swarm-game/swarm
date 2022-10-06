def until = \c. b <- c; if b {} {until c} end;

// name format: NA Entity
// N - one digit room number
// A one letter action
name <- whoami;
let n = fst (split 1 name) in
let a = fst $ split 1 $ snd $ split 1 name in
let e = snd $ split 3 name in

check <- robotNamed ("check" ++ n);
until (as check {has "Win"});

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
    say $ "Finished waiting for check" ++ n ++ " but I don't know what to do: " ++ a
}}}
