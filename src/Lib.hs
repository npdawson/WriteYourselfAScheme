module Lib where

import Control.Monad.Except
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

import Prims
import Types

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) =
  do result <- eval env predicate
     case result of
       Bool False -> eval env alt
       Bool True -> eval env conseq
       val -> throwError $ TypeMismatch "boolean" val
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom func : args)) =
  mapM (eval env) args >>= liftThrows . apply func
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting and unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
         valueRef <- newIORef value
         env <- readIORef envRef
         writeIORef envRef ((var, valueRef) : env)
         return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = (++ env) <$> (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
