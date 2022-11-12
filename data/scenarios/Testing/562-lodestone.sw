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
build {
  log "Hi!";
  repeat (
    log "I am going for a bit";
    turn east; m2; x <- until (ishere "bit (0)") {harvest}; turn back; m2; place x;
		log "I brought a bit";
)};
until (ishere "bit (0)") {grab};
until (ishere "bit (0)") {grab};

make "bit (1)";
make "drill bit"
