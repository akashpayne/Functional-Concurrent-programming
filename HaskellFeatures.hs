{- #########################################################

        HaskellFeatures.hs
        Simon Thompson, March 2014.

######################################################### -}

module HaskellFeatures where

--
--      Infinite lists 
--

--      The infinite list of ones: [1, 1, ...]

ones :: [Integer]
ones = 1 : ones

--      The integers from n:  [n, n+1, ...]

nums :: Integer -> [Integer]
nums n = n : nums (n+1)

--      Numbers as a lazy stream
--      compare with Erlang processes:
--      here the definitions reflect the values
--      along the various channels in the system
        
nats :: [Integer]
nats = 0 : zipWith (+) ones nats

--
--     The Hamming numbers are those numbers whose
--     only prime divisors are 2 and 3.
--

--     A "generate and test" solution.

hammingFilter :: [Integer]

hammingFilter 
  = filter checkDivisors (nums 1)
    where
    checkDivisors 1 = True
    checkDivisors n
     | n>1
       = (n `mod` 2 == 0 && checkDivisors (n `div` 2)) ||
         (n `mod` 3 == 0 && checkDivisors (n `div` 3)) 
     | n<0
       = False

hammingGen :: [Integer]

hammingGen = 1 : merge (map (*2) hammingGen) (map (*3) hammingGen)

-- Merge two ordered lists in order, removing duplicates

merge :: Ord  a => [a] -> [a] -> [a]

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x<y   = x : merge xs (y:ys)
  | x==y  = x : merge xs ys
  | x>y   = y : merge (x:xs) ys
    
--
-- Expressions
-- 

data Expr 
  = Lit Integer
  | Var Var
  | App Op Expr Expr
    deriving (Eq,Ord)
    
instance Show Expr where
 show (Lit n) = show n
 show (Var v) = [v]
 show (App op e1 e2)
  = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

type Var = Char

data Op = Add | Mul
          deriving (Eq,Ord)

instance Show Op where
 show Add = "+"
 show Mul = "*"

expressions :: [Expr]

expressions = interleave
                (interleave (map Lit (nums 0)) (map Var ['a','b'..'z']))   --- numbers and variables
                (interleave (zipWith (App Add) expressions expressions)    --- additions
                            (zipWith (App Mul) expressions expressions))   --- multiplications

interleave :: [a] -> [a] -> [a]

interleave (x:xs) ys
  = x : interleave ys xs
interleave [] ys = ys
