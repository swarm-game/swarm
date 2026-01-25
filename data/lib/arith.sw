def boolToInt: Bool -> Int = \b. if b {1} {0} end

def signum: Int -> Int = \x. if (x < 0) {-1} {if (x > 0) {1} {0}} end

def max: Int -> Int -> Int = \a. \b. if (a > b) {a} {b} end

def min: Int -> Int -> Int = \x. \y. if (x < y) {x} {y} end

def mod: Int -> Int -> Int = \i. \m. i - m * (i / m) end

def abs: Int -> Int = \n. if (n < 0) {-n} {n} end

def isEven: Int -> Bool = \n. mod n 2 == 0 end

def isDivisibleBy: Int -> Int -> Bool
  = \dividend. \divisor.
  mod dividend divisor == 0
end
