module ConsoleTests where

shouldEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
shouldEqual label val expected =
  if val == expected
    then putStrLn $ label ++ " PASSED"
    else putStrLn $ label ++ " FAILED: expected " ++ show expected ++ " but got " ++ show val