-- | The 'reverseChunks' function takes a list and an integer n,
-- and returns a new list where every n-sized chunk of the original list
-- is reversed.
-- Example:
-- >>> reverseChunks 3 [8,18,15,20,12,12,14,12,2,19,0]
-- [15,18,8,12,12,20,2,12,14,0,19]

reverseChunks :: Int -> [a] -> [a]
reverseChunks n ls = concat $ if null ls then [] else [reverse $ take n ls] ++ [reverseChunks n (drop n ls)]