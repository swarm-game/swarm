/*******************************************************************/
/*                       LIST ENCODING IN INT                      */
/*                                                                 */
/* This file defines functions that allow storing arbitrary list   */
/* of values in one integer. It is possible because an integer can */
/* have unlimited length and can be split into chunks of smaller   */
/* integers. See Haskell docs for Integer (used by swarm for int). */
/*                                                                 */
/* For examples of usage see the unit tests in testLIST function.  */
/*                                                                 */
/*******************************************************************/

// Definitions:
// - MAIN: nil (Haskell []), cons (Haskell (:)), head, tail
// - BONUS: headTail, index, for, for_each, for_each_i
// - ARITH: len, mod, shiftL, shiftR, shiftLH, shiftRH
// - HELPERS: consP, getLenPart, setLenPart, to1numA, getLenA
// - TESTS: assert, testLIST, testLIST_BIG, testLIST_ALL

/*******************************************************************/
/*                              TYPE                               */
/*******************************************************************/

// Type of list of ints - the inner ints can themselves be lists,
// so *any* type can be encoded inside.
//
// It is the callers responsibility to make sure a program using this
// "type" is type safe. Notably 2 == [0] != [] == 0 but [] !! x == 0.
// 
// TODO: once #153 is resolved, add types to definitions
//
// type listI = int

/*******************************************************************/
/*                             LAYOUT                              */
/*******************************************************************/

// The length of a number is kept in the header and split into 7bit
// chunks each prefixed by 1bit that marks if the byte is last in
// the header (0=YES).

/* EXAMPLE - [short_x,long_y] - concretly e.g. [42, 2^(2^7)] 

0   < len short_x < 2^7
2^7 < len long_y  < 2^14 

cons short_x   $          cons long_y          $ nil
vvvvvvvvvvvv       vvvvvvvvvvvvvvvvvvvvvvvvv     vvv
  0|len x|x    |   1|len y%2^7|0|len y/2^7|y   |  0
^^^^^^^^^^^^       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  head                         tail            
*/

/*******************************************************************/
/*                         ARITH PRIMITIVES                        */
/*******************************************************************/

// bitlength of a number (shifting by one)
def naive_len : int -> int = \i.
  if (i == 0) {0} {1 + naive_len (i/2)}
end

// modulus function (%)
def mod : int -> int -> int = \i.\m.
  i - m * (i / m)
end

// f X Y = Y * 2^(-X)
def shiftL : int -> int -> int = \s.\i.
  i / 2^s
end

// f X Y = Y * 2^(X)
def shiftR : int -> int -> int = \s.\i.
  i * 2^s
end

// shift by (8bit) bytes
def shiftLH : int -> int -> int = \s. shiftL (s*8) end
def shiftRH : int -> int -> int = \s. shiftR (s*8) end

// bitlength of a number (shifting by 64)
def len : int -> int = \i.
  let next = i / 2^64 in
  if (next == 0) {naive_len i} {64 + len next}
end

/*******************************************************************/
/*                       helper functions                          */
/*******************************************************************/

def getLenPart : int -> int = \i. mod (i/2) (2^7) end
def setLenPart : int -> int = \i. 2 * (mod i (2^7)) end

// Split length into 7-bit parts and prefix all but last with 1
def to1numA : int -> int * int = \i.
  let nextPart = shiftL 7 i in
  if (nextPart == 0)
    { ((2 * i), 1) } /* last part */
    { let p = to1numA nextPart in
      ( 1 /* header isEnd bit */ + setLenPart i + shiftRH 1 (fst p)
      , 1 + snd p
      )
    }
end

// Get bitlength of the first number in the list
// and also the list starting at the number itself
//
// getLenA : listI -> int * int
def getLenA = \xs.
  let isEnd = 0 == mod xs 2 in
  let l = getLenPart xs in
  let nextPart = shiftLH 1 xs in
  if isEnd
    { (l, nextPart) }
    { let p = getLenA nextPart in
      (l + shiftR 7 (fst p), snd p)
    }
end

/*******************************************************************/
/*                         LIST FUNCTIONS                          */
/*******************************************************************/

// headTail : listI -> {int} * {listI} 
def headTail = \xs.
  let sign = mod xs 2 in
  let ns = xs / 2 in
  let p = getLenA ns in
  ( { let x = mod (snd p) $ 2 ^ fst p in
      if (sign == 0) {x} {-x}
    }
  , { shiftL (fst p) (snd p) }
  )
end

// head : listI -> int
def head : int -> int = \xs.
  force $ fst $ headTail xs
end

// tail : listI -> listI 
def tail = \xs.
  force $ snd $ headTail xs
end

// nil : listI
def nil = 0 end

// Add non-negative number to beginning of list (cons adds the sign)
// consP : nat -> listI -> int
def consP = \x.\xs.
  if (x == 0)
    { 2 /* header says one bit length */ + shiftR (8+1) xs}
    { let l = len x in
      let pl = to1numA l in
      (fst pl + shiftRH (snd pl) (x + shiftR l xs))
    }
end

// Add integer to the beginning of the list
// consP : int -> listI -> listI
def cons = \x.\xs.
  if (x >= 0)
    {     2 * consP   x  xs }
    { 1 + 2 * consP (-x) xs }
end


/*******************************************************************/
/*                       MORE LIST FUNCTIONS                       */
/*******************************************************************/

// index : int -> listI -> int
def index = \i.\xs.
  if (i <= 0)
    {head xs}
    {index (i-1) (tail xs)}
end

def for : int -> int -> (int -> cmd a) -> cmd unit = \s.\e.\f.
  if (s == e) {}
  {f s; for (s+1) e f}
end

// for_each_i : int -> listI int -> (int * int -> cmd a) -> cmd unit
def for_each_i = \i.\xs.\f.
  if (xs == nil) {}
  { let ht = headTail xs
    in f i (force $ fst ht); for_each_i (i+1) (force $ snd ht) f
  }
end

// for_each : listI int -> (int -> cmd a) -> cmd unit
def for_each = \xs.\f.
  for_each_i 0 xs (\i. f)
end

/*******************************************************************/
/*                           UNIT TESTS                            */
/*******************************************************************/

// TODO: show values when #248 is implemented
def assert = \b.\m.
  if b {} {log "FAIL:"; fail m}
end

def testLIST =
  log "STARTING TESTS OF LIST:";

  log "check [0]";
  let l0 = cons 0 nil in
  assert (l0 == 4)                   "[0] ~ 4";
  assert (head l0 == 0)              "head [0] == 0";
  assert (tail l0 == nil)            "tail [0] == []";

  log "check [1]";
  let l1 = cons 1 nil in
  assert (l1 == 516)                 "[1] ~ 516";
  assert (head l1 == 1)              "head [1] == 1";
  assert (tail l1 == nil)            "tail [1] == []";

  log "check [-1]";
  let ln1 = cons (-1) nil in
  assert (ln1 == 517)                "[-1] ~ 517";
  assert (head ln1 == -1)            "head [-1] == -1";
  assert (tail ln1 == nil)           "tail [-1] == []";
  
  log "check [42]";
  let l42 = cons 42 nil in
  assert (l42 == 21528)              "[42] ~ 21528";
  assert (head l42 == 42)            "head [42] == 42";
  assert (tail l42 == nil)           "tail [42] == []";
  
  log "check [499672]";
  let l499672 = cons 499672 nil in
  assert (l499672 == 255832140)      "[499672] ~ 255832140";
  assert (head l499672 == 499672)    "head [499672] == 499672";
  assert (tail l499672 == nil)       "tail [499672] == []";
  
  log "check [1,0]";
  let l1_0 = cons 1 l0 in
  assert (l1_0 == 4612)              "[1,0] ~ 4612";
  assert (head l1_0 == 1)            "head [1,0] == 1";
  assert (tail l1_0 == l0)           "tail [1,0] == [0]";

  log "check [11,1,0]";
  let l11_1_0 = cons 11 l1_0 in
  assert (head l11_1_0 == 11)        "head [11,1,0] == 11";
  assert (tail l11_1_0 == l1_0)      "tail [11,1,0] == [1,0]";

  log "check [0..9]";
  let l0__9 = cons 0 $ cons 1 $ cons 2 $ cons 3 $ cons 4 $ cons 5 $ cons 6 $ cons 7 $ cons 8 $ cons 9 nil in
  assert (head l0__9 == 0)           "head [0..9] == 0";

  log "check indices";
  assert (index 0 l0__9 == 0)        "[0..9] !! 0 == 0";
  assert (index 9 l0__9 == 9)        "[0..9] !! 9 == 9";

  // TODO: log the number of iteration once #248 is implemented

  log "check for";
  for 0 9 (\i.
    assert (index i l0__9 == i)      "[0..9] - every index I has value I"
  );

  log "check for_each";
  for_each l0__9 (\x.
    assert (x < 10)                  "[0..9] - every value X < 10"
  );

  log "check for_each_i";
  for_each_i 0 l0__9 (\i.\x.
    assert (i == x)                  "[0..9] - I == X for every value X at index I"
  );

  log "OK - ALL TEST PASSED\a"
end

// This tests uses VERY LONG lists of size up to 2097332 bits.
// TIP: increase the game speed (ticks/s) when you run this test.
def testLIST_BIG =
  log "STARTING TESTS OF BIG LISTS:";

  log "check [2^(2^7)] aka [big]";
  let big = 2^2^7 in
  assert (len big == 129)            "len (2^2^7) == 2^7+1";
  assert (to1numA 129 == (515,2))    "to1numA: 129 == 1000_0001 --> 515 == [0000 001 0] [0000 001 1]";
  assert (getLenA 515 == (129,nil))  "getLenA: 515 --> 129";
  let ibig = 2 * (shiftR 16 big + 515) in
  let lbig = cons big nil in
  assert (lbig == ibig)              "[big] ~ 2 * ((big * 2^(2*8)) + 515)";
  assert (head lbig == big)          "head [big] == big";
  assert (tail lbig == nil)          "tail [big] == []";

  log "check [2^(2^21)] aka [bigger]";
  let bigger = 2^(2^21) in
  let lbigger = cons bigger nil in
  assert (head lbigger == bigger)    "head [bigger] == bigger";
  assert (tail lbigger == nil)       "tail [bigger] == []";

  log "check [2^(2^21),2^(2^7)] aka [bigger,big]";
  let lbiggest = cons bigger lbig in
  assert (head lbiggest == bigger)  "head [bigger,big] == bigger";
  assert (tail lbiggest == lbig)    "tail [bigger,big] == [big]";
  
  log "OK - ALL TEST PASSED";
end

def testLIST_ALL =
  testLIST;
  testLIST_BIG;
end
