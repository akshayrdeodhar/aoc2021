
{- counts the number of "increasing" steps -}
countLarger' :: Ord a => [a] -> Integer
countLarger' [] = 0
countLarger' (x:xs) 
  | null xs = 0
  | x < head xs = 1 + countLarger' xs
  | otherwise = countLarger' xs

main = do
  contents <- getContents 
  let intSequence = map read (lines contents) :: [Integer]
  print (countLarger' intSequence)