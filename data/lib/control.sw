def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def λmatch = \f. \p. match p f end

def forever : Cmd Unit -> Cmd Unit = \c. c ; forever c end

