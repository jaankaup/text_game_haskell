module World where
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C
import qualified Misc as Misc
import Data.Monoid
import Data.List (stripPrefix)
import Data.Char (isSpace, isDigit)
import Data.Monoid
import Control.Monad.Trans.State
import Control.Monad
import Parser
-- import Data.Functor

data Object = Obj !Pos !Char 
type Pos = (Int,Int)
type World = M.Map Pos Char
 
data PData  -- I !Int
       = Range !(Int,Int) !(Int,Int) !Char 
       | Point !(Int,Int) !Char
       deriving (Show)
           
pdata :: Parser PData
pdata = pd 
     where    
       pd = range <> point 
       pair = pure (,) <*> (char '(' *>number<*char ',') <*> (number <* char ')') 
       range = pure Range <*> pair <*> (space *> string "->" *> space *> pair) <*> (space *> char '\'' *> char' <* char '\'')
       point = pure Point <*> pair <*> (space *> char '\'' *> char' <* char '\'') 

createRange :: Pos -> Pos -> Char -> World  
createRange (x0,y0) (x1,y1) c = let w = M.empty :: World 
                                    q = [((a,b),c) | b <- [y0..y1], a <- [x0..x1]]
                                in M.fromList q 

createPoint :: Pos -> Char -> World
createPoint p c = M.singleton p c 

createMap :: PData -> World
createMap pd = case pd of
                 (Point p c) -> createPoint p c 
                 (Range p1 p2 c) -> createRange p1 p2 c

combine :: World -> World -> World
combine a b = M.union a b 

translate :: Pos -> World -> World
translate (x,y) w = let list = M.toList w
                    in M.fromList $ map (\((a,b),c) -> ((a+x,b+y),c)) list

build :: String -> StateT World Maybe ()
build str = StateT $ \w -> case runParser pdata str of
                             Just (a,_) -> Just ((),combine w (createMap a))
                             Nothing -> Nothing

drawWorld :: World -> IO ()
drawWorld w = let list = map (\((a,b),_) -> (a,b)) $ M.toList w
                  xCoords = map (\(m,_) -> m) list
                  yCoords = map (\(_,n) -> n) list
                  maxH = maximum yCoords 
                  minH = minimum yCoords
                  maxW = maximum xCoords
                  minW = minimum xCoords
                  backGround = createRange  (minW,minH) (maxW,maxH) ' '
                  combined = combine w backGround 
                  str = [M.lookup (x,y) combined | y <- [minH..maxH], x <- [minW..(maxW+1)]] 
              in  mapM_ (\x -> case x of
                                 Nothing -> putChar '\n'
                                 Just c  -> putChar c) str  

clip :: (Int,Int) -> Int -> World -> World
clip (x,y) r w = let screen = createRange (-r+x,-r+y) (r+x,r+y) ' '     
                     union = M.union w screen 
                 in M.intersection union screen

loadWorld :: String -> IO (Either String World)
loadWorld loc = do x <- Misc.readFile loc
                   case x of
                     Left s  -> return $ Left s 
                     Right s -> do let lns = (lines . C.unpack) s 
                                   let tr  = traverse (\x -> build x)
                                   let result = runStateT (tr lns) M.empty 
                                   case result of
                                     Just (_,w) -> return $ Right w
                                     Nothing    -> return $ Left ("Failed to build world from file " ++ loc) 

collides :: Pos -> World -> Bool
collides p w = case M.lookup p w of
               Nothing -> False
               Just x  -> if x == ' ' then False else True
                     
main = do let x = "(0,0) -> (10,10) x"
          let y = "(0,5) k" 
          let p = runParser pdata y
          print p
          
-- (0,0) -> (10,10) 'X'
-- (0,5) ' ' 
-- (1,1) -> (9,9) ' '
