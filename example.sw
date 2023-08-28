// TODO: make a test from me maybe?
def harvestbox : dir -> (cmd unit -> cmd unit) -> (cmd unit -> cmd unit) -> cmd unit = \d. \rep1. \rep2.
  doboxP harvest d rep1 rep2 notempty
end