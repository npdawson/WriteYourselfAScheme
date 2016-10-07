module Lib where

import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

import Prims
import Types

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, conseq, alt]) =
  do result <- eval predicate
     case result of
       Bool False -> eval alt
       Bool True -> eval conseq
       val -> throwError $ TypeMismatch "boolean" val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $
                         NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseDottedList <|> parseList
                char ')'
                return x

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf ['\\', '"']
  _ <- char '"'
  return $ String x

escapedChars :: Parser Char
escapedChars = do
  c <- char '\\' >> oneOf ['\\', '"', 'n', 'r', 't']
  return $ case c of
    '\\' -> c
    '"' -> c
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  list <- endBy parseExpr spaces
  extra <- char '.' >> spaces >> parseExpr
  return $ DottedList list extra

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseNumber :: Parser LispVal
parseNumber = parseRadix <|> parseNum

parseRadix :: Parser LispVal
parseRadix = char '#' >> ( parseDec
                           <|> parseHex
                           <|> parseOct
                           -- <|> parseBin
                         )

parseNum :: Parser LispVal
parseNum = (Number . read) <$> many1 digit

parseDec :: Parser LispVal
parseDec = do
  char 'd'
  (Number . read) <$> many1 digit

parseOct :: Parser LispVal
parseOct = do
  char 'o'
  (Number . readWith . readOct) <$> many1 octDigit

parseHex :: Parser LispVal
parseHex = do
  char 'x'
  (Number . readWith . readHex) <$> many1 hexDigit

-- parseBin :: Parser LispVal
-- parseBin = do
--   char 'b'
--   (Number . readWith . readBin) <$> many1 binDigit

readWith :: [(a, b)] -> a
readWith = fst . head

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

