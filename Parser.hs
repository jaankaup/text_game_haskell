module Parser where
import Data.Monoid
import Data.List (stripPrefix)
import Data.Char (isSpace, isDigit)

newtype Parser a = P (String -> Maybe (a,String))

runParser :: Parser a -> String -> Maybe (a,String)
runParser (P f) = f

instance Functor Parser where
  fmap g (P f) = P $ fmap (\(a,s) -> (g a, s)) . f

instance Applicative Parser where
  pure x = P (\s -> Just (x,s))
  (P ff) <*> (P fx) = P (\s -> case ff s of
                           Nothing -> Nothing
                           Just (f,s') -> case fx s' of
                             Nothing -> Nothing
                             Just (a, s'') -> Just (f a, s''))

instance Monad Parser where
  return = pure
  pa >>= f = do x <- pa
                f x

instance Monoid (Parser a) where
  mempty = P (const Nothing)
  mappend pa pb = P (\s -> case runParser pa s of
                         Just a  -> Just a
                         Nothing -> runParser pb s)

takeP :: (Char -> Bool) -> Parser String
takeP p = P (\s -> case span p s of
                     ("",_) -> Nothing
                     (s,e)  -> Just (s,e))

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = (:) <$> p <*> ((sep*>sepBy sep p)<>pure [])

space :: Parser ()
space = fmap (const ()) (takeP isSpace)

parens :: Parser a -> Parser a
parens = between "(" ")"

between :: String -> String -> Parser a -> Parser a
between l r p = string l *> p <* string r

string :: String -> Parser String
string str = P (fmap (\s ->(str,s)) . stripPrefix str)

char' :: Parser Char
char' = P (\s -> case s of
                  []     -> Nothing
                  (x:xs) -> Just (x,xs))

char :: Char -> Parser Char
char c = P (\s -> case s of
                    [] -> Nothing
                    (x:xs) -> if x == c then Just (c,xs) else Nothing)

number :: Parser Int
number = fmap read (takeP isDigit)

