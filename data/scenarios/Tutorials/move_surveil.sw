// name format: 00 00 N A Entity
// 00 00  - the position to surveil (two digit with leading zeros)
// N      - wait for the nth change
// A      - one letter action (S/G/P = swap/grab/place)
// Entity - the entity name parameter to the place/swap action
name <- whoami;

def repeat : Int -> Cmd Unit -> Cmd Unit =
  \n. \c. if (n == 0) {} {c ; repeat (n-1) c}
end

def elif = \b.\t.\e. {if b t e} end
def else = \e. e end

def fromDigit = \d.
    if (d == "0") {0}
    $elif (d == "1") {1}
    $elif (d == "2") {2}
    $elif (d == "3") {3}
    $elif (d == "4") {4}
    $elif (d == "5") {5}
    $elif (d == "6") {6}
    $elif (d == "7") {7}
    $elif (d == "8") {8}
    $elif (d == "9") {9}
    $else {undefined}
end

def text2int = \t. fromDigit (tochar $ charat (chars t - 1) t) + if (chars t > 1) {0} {10 * text2int (fst $ split 1 t)} end

def args =
    let x = fst (split 2 name) in
    let y = fst $ split 2 $ snd $ split 3 name in
    let n = fst $ split 1 $ snd $ split 6 name in
    let a = fst $ split 1 $ snd $ split 8 name in
    let e = snd $ split 10 name in
    [name=name, position=(text2int x, text2int y), n=fromDigit n, action=a, entity=e]
end

def act_lazy: Text -> Text -> Cmd (Cmd Unit) = \a.\e.
    if (a == "S") {
        if (e != "") { create e } {};
        return (swap e; log $ a ++ ": " ++ e)
    } { if (a == "G") {
        return (grab; log a)
    } { if (a == "P") {
        if (e != "") { create e } {};
        return (place e; log $ a ++ ": " ++ e)
    } {
        return (say $ "Finished waiting for check but I don't know what to do: '" ++ a ++ "'")
    }}}
end

def main =
    log $ format args;
    act <- act_lazy args.action args.entity;
    repeat args.n (
        surveil args.position;
        wait 1000000;
    );
    act
end

if (name != "base") {main} {}