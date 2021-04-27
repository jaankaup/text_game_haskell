import Control.Applicative
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C

data Vertex = V !Float !Float !Float deriving (Show)
data TextCoord = T !Float !Float deriving (Show)

-- Parses number(s) from a bytestring inside of container f. 
parseNumbers :: (Read a, Functor f) => f C.ByteString -> f (Maybe a)
parseNumbers = fmap (readMaybe . C.unpack)

-- -- Checks if given f contains only Just values.
validateNumbers :: (Eq a, Functor f, Foldable f) => f (Maybe a) -> Bool
validateNumbers = and . fmap (/= Nothing) 

extract :: (Read r, Eq r) => [C.ByteString] -> Maybe [r]
extract c = let numbs = map (readMaybe . C.unpack) c
                val   = and . map (/= Nothing)
            in if val numbs then Just $ map fromJust numbs else Nothing  

-- parseV :: [C.ByteString] -> Maybe Vertex
-- parseV src = case extract src of
--                Just [a,b,c] -> Just $ V a b c 
--                _                   -> Nothing 
parseV :: [C.ByteString] -> StateT [Float] Either [Float]
parseV src = case extract src of
               Just [a,b,c] -> 

parseT :: [C.ByteString] -> Maybe TextCoord 
parseT src = case extract src of
               Just [a,b] -> Just $ T a b
               _          -> Nothing







