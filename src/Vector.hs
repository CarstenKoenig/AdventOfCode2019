module Vector where

newtype Vector3 
  = V (Int, Int, Int)
  deriving (Show, Eq, Ord)

type Coord = Vector3
type Velocity = Vector3

instance Num Vector3 where
  fromInteger n = V (fromInteger n,fromInteger n,fromInteger n)
  V (a,b,c) + V (a',b',c') = V (a+a',b+b',c+c')
  negate (V (a,b,c)) = V (negate a, negate b, negate c)
  signum (V (a,b,c)) = V (signum a, signum b, signum c)

x :: Vector3 -> Int
x (V (x,_,_)) = x

y :: Vector3 -> Int
y (V (_,y,_)) = y

z :: Vector3 -> Int
z (V (_,_,z)) = z
