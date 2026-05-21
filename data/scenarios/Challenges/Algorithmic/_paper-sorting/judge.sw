def fst : a * b -> a = \p.
  match p \a. \_. a
end

def ensureSorted : Int -> Text -> Cmd Bool = \n. \prev.
  if (n == 0) {pure True}
  { res <- scan down;
    case res
      (\_. pure false)
      (\e. if (fst (split 5 e) == "paper" && prev <= e)
             {move; ensureSorted (n-1) e}
             {pure false}
      )
  }
end

def judge =
  j <- robotnamed "judge";
  as j {
    // Flag that setup is done
    hasBitcoin <- has "bitcoin";
    if hasBitcoin {
      ensureSorted 16 "paper"
    } {
      pure false;
    }
  }
end
