def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def λcase = \f. \g. \s. case s f g end
def λmatch = \f. \p. match p f end

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def forever : Cmd Unit -> Cmd Unit = \c. c ; forever c end

def liftA2 : (a -> b -> c) -> Cmd a -> Cmd b -> Cmd c = \f. \ca. \cb.
  a <- ca;
  b <- cb;
  pure (f a b)
end

def ifC: ∀ a. Cmd Bool -> {Cmd a} -> {Cmd a} -> Cmd a
  = \test. \then. \else.
  b <- test;
  if b then else
end

def notC : Cmd Bool -> Cmd Bool = \c.
  b <- c; pure (not b)
end

def andC : Cmd Bool -> Cmd Bool -> Cmd Bool = \c1. \c2.
  b1 <- c1;
  if b1 {c2} {pure false}
end

def orC : Cmd Bool -> Cmd Bool -> Cmd Bool = \c1. \c2.
  ifC c1 {pure true} {c2}
end

def while: ∀ a. Cmd Bool -> {Cmd a} -> Cmd Unit
  = \test. \body.
  ifC test {force body; while test body} {}
end

def until = \p. \c. q <- p; if q {} {c; until p c} end;

def when : Cmd Bool -> Cmd a -> Cmd Unit = \test. \cmd.
  b <- test;
  if b {cmd; pure ()} {}
end

def for_ : Int -> (Int -> Cmd a) -> Cmd Unit = \n. \k.
  if (n == 0) {} {k n; for_ (n-1) k}
end

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

