module R where
hey x = case x of
  Just x
    | x == "lol" -> 2
  Just x -> 3
  Nothing -> 4
