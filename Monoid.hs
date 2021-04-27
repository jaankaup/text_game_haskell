module Mon where

import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import World

newtype Wld a b = W (M.Map a b)

instance Ord a => Monoid (Wld a b) where
  mempty = W M.empty
  mappend (W x) (W y) = W $ M.union x y

loadWorlds2 = do x <- runWriterT $ traverse loadWorlds ["scene1.wld","littleHouse.wld","thing1.wld","thing2.wld","thing3.wld"]
                 return $ snd x

loadWorlds :: FilePath -> WriterT [Either String World] IO ()
loadWorlds fp = do x <- (liftIO . loadWorld) fp
                   case x of
                     Left s -> tell $ [Left s] -- TODO: Endo 
                     Right s -> tell [x]
                   return () 
