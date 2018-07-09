

module ParserCombinators where

import Prelude hiding (fail)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Control.Monad hiding (fail)

data Parser a = P (String -> [(a, String)])
-- Parser是一个复合数据类型，构造子接受一个函数作为参数，这个函数接受一个String，返回一个列表。
-- 这个列表是可能的分析结果。

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

fail :: Parser a
fail = P $ \_ -> []
-- 一个失败的Parser其中的函数的返回值为空列表。

item :: Parser Char 
item = P $ \inp -> case inp of 
                     (x:xs) -> [(x, xs)]
                     [] -> []
                     
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P $ \inp -> case parse p inp of 
                        [] -> parse q inp
                        [(v, inp')] -> [(v, inp')]
-- +++运算是先用p来分析，如果失败了再用q来分析。

instance Monad Parser where 
    p >>= q = P $ \inp -> case parse p inp of 
                            [] -> []
                            [(v, inp')] -> let q' = q v in parse q' inp'
    return v = P $ \inp -> [(v, inp)]
-- 不用p里面自带的函数分析，而是用q来分析。

instance Applicative Parser where
  pure = return
  (<*>) = ap
  
instance Functor Parser where
  fmap = liftM    
-- 新版本haskell的编译器，实现Monad必须要实现Applicative和Functor，但是不影响语义。
-- 所以可以这样用模板声明。
  
  
setParserState state =  P $ \_ -> [((), state)]
getParserState = P $ \inp -> [(inp, inp)]

wouldSucceed p = P $ \inp -> case parse p inp of
                               [(v, vs)] -> [(True, inp)]
                               [] -> [(False, inp)]

notFollowedBy p q = do
  state <- getParserState
  v <- p
  r <- wouldSucceed q
  if not r 
    then return v
    else setParserState state >> fail
-- 如果用q不可以分析，那么用p分析。

sat :: (Char -> Bool) -> Parser Char
sat pred = item >>= \c -> 
           if pred c then return c else fail

char :: Char -> Parser Char
char x = sat (== x)

digit, letter, alphanum :: Parser Char
digit = sat isDigit
letter = sat isAlpha
alphanum = sat isAlphaNum

string :: String -> Parser String 
string [] = return []
string (x:xs) = char x >>= \c ->
                string xs >>= \cs ->
                return (c:cs)
                
identifier :: Parser String
identifier = letter >>= \l ->
               many alphanum >>= \ls ->
               return (l:ls)

nat :: Parser Int
nat = many1 digit >>= \s -> return (read s)

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v ->
          many p >>= \vs -> 
          return (v:vs)
          
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

space :: Parser ()
space = many (space1 +++ comment) >> return ()

space1 = many1 (sat isSpace) >> return ()
comment = string "--" >> many (sat (/= '\n')) >> return ()

token :: Parser a -> Parser a
token p = p >>= \v ->
          space >>
          return v

symbol :: String -> Parser String
symbol xs = token (string xs)

opSymbols = "=<>~!@#$%^&*+-?/,.|\\"

operator :: String -> Parser String
operator xs = token (notFollowedBy (string xs) (sat (\x-> elem x opSymbols)))

keyword :: String -> Parser String
keyword xs = do 
  token (notFollowedBy (string xs) alphanum)

parens :: Parser a -> Parser a
parens p = symbol "(" >> p >>= \v -> symbol ")" >> return v

braces :: Parser a -> Parser a
braces p = symbol "{" >> p >>= \v -> symbol "}" >> return v
