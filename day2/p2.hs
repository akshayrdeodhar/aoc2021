
parseCommand :: String -> (String, Int)
parseCommand command = 
    let split = words command
    in (split !! 0, (read (split !! 1) :: Int))

{- State: (x axis, y axis) -}

applyCommand :: (Int, Int) -> (String, Int) -> (Int, Int)
applyCommand state command 
    | direction == "forward" = (x + magnitude, y)
    | direction == "down" = (x, y - magnitude)
    | direction == "up" = (x, y + magnitude)
    where 
    direction = fst command
    magnitude = snd command
    x = fst state
    y = snd state

drive :: (Int, Int) -> [(String, Int)] -> (Int, Int)
drive state commands = 
    if null commands then state
    else drive (applyCommand state (head commands)) (tail commands) 

main = do
    input <- getContents
    let input_lines = lines input
    let parsed_input = [parseCommand x | x <- input_lines]
    print (drive (0, 0) parsed_input)
