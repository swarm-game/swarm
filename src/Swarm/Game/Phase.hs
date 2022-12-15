module Swarm.Game.Phase where

-- | The phase of XXX
data Phase
  = -- | A value has just been read in from a scenario description.
    --   Any programs it contains are raw, unprocessed text.
    Raw
  | -- | Any contained programs have been processed (parsed,
    --   typechecked, imports resolved, etc.).  However, values still
    --   represent /templates/ that may later be instantiated as one
    --   or more concrete world values.
    Template
  | -- | A concrete value (such as a @Robot@) that exists in the
    --   world.
    Concrete
