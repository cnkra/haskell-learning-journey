-- | The 'returner1' function returns every nth element from the list.
-- Example:
-- >>> returner1 3 [1,9,3,4,1,6,7,18,4,11]
-- [3,6,4]

returner1 :: Int -> [a] -> [a]
returner1 n list = helper 1 list where
  helper _ [] = []
  helper i (x : rest)
    | i == n = x : helper 1 rest
    | otherwise = helper (i+1) rest

-- | The 'returner2' function returns every nth element from the list using zip and filter.
-- Example:
-- >>> returner2 3 [1,9,3,4,1,6,7,18,4,11]
-- [3,6,4]
returner2 :: Int -> [a] -> [a]
returner2 n list =
  let zipped = zip [1 ..] list
      filtered = filter (\(i,elm) -> i `mod` n == 0) zipped
      noCounter = map snd filtered
  in noCounter

-- | The 'returner3' function returns every nth element from the list using concatMap.
-- Example:
-- >>> returner3 3 [1,9,3,4,1,6,7,18,4,11]
-- [3,6,4]
returner3 :: Int -> [a] -> [a]
returner3 n list =
  let zipped = zip [1 ..] list
      f (i,elm)
        | i `mod` n == 0 = [elm]
        | otherwise = []
      filteredANDmapped = concatMap f zipped
  in filteredANDmapped

-- | The 'returner4' function returns every nth element from the list using cycle.
-- Example:
-- >>> returner4 3 [1,9,3,4,1,6,7,18,4,11]
-- [3,6,4]
returner4 :: Int -> [a] -> [a]
returner4 0 list = []
returner4 n list =
  let zipped = zip (cycle [1 .. n]) list
      f (i,elm)
        | i == n = [elm]
        | otherwise = []
      filteredANDmapped = concatMap f zipped
  in filteredANDmapped

-- | The 'returner5' function returns every nth element from the list using drop.
-- Example:
-- >>> returner5 3 [1,9,3,4,1,6,7,18,4,11]
-- [3,6,4]
returner5 :: Int -> [a] -> [a]
returner5 0 list = []
returner5 n list =
  let dropping = drop (n-1) list
  in case dropping of
       [] -> []
       (x : rest) -> x : returner5 n rest

-- | The 'returner6' function returns every nth element from the list using list comprehension.
-- Example:
-- >>> returner6 3 [1,9,3,4,1,6,7,18,4,11]
-- [3,6,4]
returner6 :: Int -> [a] -> [a]
returner6 0 list = []
returner6 n list = [ x | (i,x) <- zip [1 ..] list
                       , i `mod` n == 0]

-- | The 'returner7' function returns every nth element from the list using German variable names.
-- Example:
-- >>> returner7 3 [1,9,3,4,1,6,7,18,4,11]
-- [3,6,4]
returner7 :: Int -> [a] -> [a]
returner7 n list =
  let verkuppeln = zip [1 ..] list
  in [ elm | (index,elm) <- verkuppeln, index `mod` n == 0 ]