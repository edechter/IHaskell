
-- | This module splits a shell command line into a list of strings,
--   one for each command / filename
module IHaskell.Eval.ParseShell (parseShell) where 

import Prelude hiding (words)
import Text.ParserCombinators.Parsec hiding (manyTill)
import Control.Applicative hiding ((<|>), many, optional)

import Debug.Trace
eol :: Parser Char
eol = oneOf "\n\r" <?> "end of line"

quote :: Parser Char 
quote = char '\"' <?> "quote"

-- | @manyTill p end@ different from hidden @manyTill@ in that it appends the result of @end@
manyTill :: Parser a -> Parser [a] -> Parser [a]
manyTill p end = scan
  where
    scan = end <|> do
      x <- p
      xs <- scan
      return $ x:xs

-- | like manyTill, but requires at least one instance of the parser.
manyTill1 :: Parser a 
             -> Parser [a] 
             -> Parser [a]
manyTill1 p end = do x <- p 
                     xs <- manyTill p end 
                     return $ x : xs

unescapedChar :: Parser Char -> Parser String 
unescapedChar p = do 
  x <- noneOf "\\"
  lookAhead p
  return [x]

quotedString :: Parser String
-- | parses strings surrounded by quotes, e.g., \"blah blah \\\"interior quote blah blach \"
quotedString = do
  q <- quote 
  xs <- manyTill anyChar end 
  return $ q:xs
  where end =  try (do c <- unescapedChar quote 
                       q <- quote 
                       return $ c ++ [q])
               <|> (lookAhead eol >> return []) 



unquotedString = manyTill1 anyChar end  
  where end = try (unescapedChar space) <|> (lookAhead eol >> return [])

word = quotedString <|> unquotedString <?> "word"

separator :: Parser String
separator = many1 space <?> "separator"

-- | Input must terminate in a space character (like a \n)
words :: Parser [String]
words = do
  x <-  word 
  rest1 <- lookAhead (many anyToken)
  (eol >> return [x]) 
      <|> do ss <-  separator 
             rest2 <-  lookAhead (many anyToken)
             xs <-  words 
             return $ x : xs 
  
parseShell :: String -> Either ParseError [String]
parseShell string = parse words "shell" (string ++ "\n")
