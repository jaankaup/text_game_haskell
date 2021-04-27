import Control.Monad.Trans.State.Lazy
import Data.List (find)

inc :: Int -> State Int Int 
inc n = state $ \x -> (n,x+n) 

dec :: State Int ()
dec = state $ \x -> ((),x-1)

test :: State Int ()
test = do inc 3
          inc 5
          dec
          dec 

add :: Int -> StateT [Int] (Either Int) Int
add n = StateT $ \x -> if find (==n) x == Nothing then Right (n,n:x) else Left 666 



addTest :: StateT [Int] (Either Int) Int
addTest = do add 5
             add 3
             add 88
