
{- counts the number of "increasing" steps -}

countLarger' :: [Int] -> Int
countLarger' x 
  | length x < 4 = 0 {- need atleast 4 elements -}
  | sum (take 3 x) < sum (take 3 (tail x)) = 1 + countLarger' (tail x)
  | otherwise = countLarger' (tail x)

main = do
  contents <- getContents 
  let intSequence = map read (lines contents) :: [Int]
  print (countLarger' intSequence)
