parseCommand :: String -> (String, Int)
parseCommand command = 
    let split = words command
    in (split !! 0, (read (split !! 1) :: Int))

{- State: (x axis, y axis) -}

applyCommand :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
applyCommand state command 
    | direction == "forward" = (x + magnitude, y + aim * magnitude, aim)
    | direction == "down" = (x, y, aim + magnitude)
    | direction == "up" = (x, y, aim - magnitude)
    where 
    direction = fst command
    magnitude = snd command
    (x, y, aim) = state

drive :: (Int, Int, Int) -> [(String, Int)] -> (Int, Int, Int)
drive state commands = 
    if null commands then state
    else drive (applyCommand state (head commands)) (tail commands) 

main = do
    input <- getContents
    let input_lines = lines input
    let parsed_input = [parseCommand x | x <- input_lines]
    let (x, y, aim) = (drive (0, 0, 0) parsed_input)
    print (x * y)
