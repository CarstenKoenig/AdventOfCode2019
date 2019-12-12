module Combinatorics where


pairs :: [a] -> [(a,a)]
pairs xs = do
  (x,xs') <- pickOne xs
  (x',_) <- pickOne xs'
  pure (x,x')


pickOne :: [a] -> [(a,[a])]
pickOne [] = []
pickOne (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- pickOne xs ]


findRepeatIn :: Ord a => [a] -> Int
findRepeatIn (x0:xs) = go 1 xs
  where 
    go n (x':xs') | x' == x0 = n
                  | otherwise = go (n+1) xs'
