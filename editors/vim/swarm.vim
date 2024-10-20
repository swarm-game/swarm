syn keyword Keyword def tydef rec end let in require
syn keyword Builtins self parent base if inl inr case fst snd force undefined fail not format chars split charat tochar key
syn keyword Command noop wait selfdestruct move backup volume path push stride turn grab harvest sow ignite place ping give equip unequip make has equipped count drill use build salvage reprogram say listen log view appear create halt time scout whereami locateme waypoint structure floorplan hastag tagmembers detect resonate density sniff chirp watch surveil heading blocked scan upload ishere isempty meet meetall whoami setname random run return try swap atomic instant installkeyhandler teleport as robotnamed robotnumbered knows
syn keyword Direction east north west south down forward left back right
syn match Type "\<[A-Z][a-zA-Z_]*\>"
syn match Operators "[-=!<>|&+*/^$:]"


syn match Comment "//.*$"
syn region MultilineComment start="/\*" end="\*/"
syn match Brackets "[\[\]\(\)\{\}]"
syn match String "\"\(\\\(['\"bfnrtav0-9]\|x[0-9a-fA-F]\)\|[^\\\"]\)*\""
syn match Number "\<[-]\=\d\+\>"

hi def link Keyword Statement
hi def link Builtins Keyword
hi def link Command Function
hi def link Direction Function
hi def link Comment Comment
hi def link MultilineComment Comment
hi def link Brackets Keyword
hi def link Operators Keyword
hi def link String String
hi def link Number Number
