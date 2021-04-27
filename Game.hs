{-# LANGUAGE DeriveFunctor #-}

module Game where

import Control.Monad.Free
import qualified Data.Map as M
import World

data GAction next
  = Command (String -> next)
  | Move String next
  | Draw next
  deriving (Functor)

command :: Free GAction String
command = Free (Command Pure)

move :: String -> Free GAction ()
move s = Free (Move s (Pure ()))

draw :: Free GAction () 
draw = Free (Draw (Pure ()))

run :: Free GAction a -> (Pos,World) -> IO a
run (Free (Command f)) st = getLine >>= (\x -> run (f x) st)
run (Free (Move s n)) r@((x,y),w) = case s of
                                "w" -> if collides (x,y-1) w then putStrLn "outs" >> run n r else run n ((x,y-1),w) 
                                "s" -> if collides (x,y+1) w then putStrLn "damn that hurts!" >> run n r else run n ((x,y+1),w)
                                "a" -> if collides (x-1,y) w then putStrLn "aiai!" >> run n r else run n ((x-1,y),w)
                                "d" -> if collides (x+1,y) w then putStrLn "oioi!" >> run n r else run n ((x+1,y),w)
                                _   ->  run n r 
run (Free (Draw n)) (p,w) = drawWorld (clip p 7 (combine (createPoint p 'O') w)) >> run n (p,w) 
run (Pure a) _           = return a

exec :: Free GAction a -> IO a
exec f = do x <- load 
            run f ((1,1),x)

gameLoop :: Free GAction ()
gameLoop = do a <- command
              move a 
              draw
              gameLoop

load :: IO World
load = do mainWorld <- loadWorld "scene1.wld"
          house <- loadWorld "littleHouse.wld"
          let w = pure (\x y -> combine x (translate (28,5) y)) <*> mainWorld <*> house
          case w of
            Left x -> do putStrLn x >> return M.empty 
            Right wld -> return wld 
                  
main = exec gameLoop    


