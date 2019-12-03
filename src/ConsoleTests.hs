module ConsoleTests where

shouldEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
shouldEqual label val exp =
  if val == exp
    then putStrLn $ label ++ " PASSED"
    else putStrLn $ label ++ " FAILED: expected " ++ show exp ++ " but got " ++ show val