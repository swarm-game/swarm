// 562-lodestone solution

def ifC = \p.\t.\e.
  b <- p;
  if b t e
end;

def until = \p.\t.
  ifC p t {until p t}
end;

def repeat = \c.
  c;
  repeat c;
end

def m2 =
  move;
  move
end;

// ---------------------------------------------------
// ┌─────┐
// │o.AT~ 
// │..AAA│
// │B.0.A│
// └─────┘
// ---------------------------------------------------

// get one lodestone
build {log "Hey!"; turn north; m2; l <- grab; turn back; m2; place l};
until (ishere "lodestone") {grab};

// get two bit (0)
// TODO: require should not be necessary
build {
  log "Hi!";
  require "branch predictor";
  repeat (
    turn east; m2; x <- until (ishere "bit (0)") {harvest}; turn back; m2; place x
)};
until (ishere "bit (0)") {grab};
until (ishere "bit (0)") {grab};

make "drill bit"
