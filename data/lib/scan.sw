def ishere : Text -> Cmd Bool = \thing.
  here <- scan down;
  pure (here == thing)
end

def isempty : Cmd Bool = ishere "" end
