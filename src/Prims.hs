module Prims where

import Control.Monad.Except

import Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", symbolp),
              ("string?", stringp),
              ("number?", numberp),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equals?", equal)]

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return . Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return . Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return . Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return . Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return . Bool $
  (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left _ -> False
                             Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                     [ AnyUnpacker unpackNum
                     , AnyUnpacker unpackBool
                     , AnyUnpacker unpackStr]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom name] = return $ String name
symbolToString list@(_ : _) = throwError $ NumArgs 1 list
symbolToString [] = throwError $ NumArgs 1 []

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol (String str : _) = return $ Atom str
stringToSymbol list@(_ : _) = throwError $ NumArgs 1 list
stringToSymbol [] = throwError $ NumArgs 1 []

symbolp :: [LispVal] -> ThrowsError LispVal
symbolp [Atom _] = return $ Bool True
symbolp [] = throwError $ NumArgs 1 []
symbolp [_] = return $ Bool False
symbolp list@(_ : _) = throwError $ NumArgs 1 list

stringp :: [LispVal] -> ThrowsError LispVal
stringp [String _] = return $ Bool True
stringp [] = throwError $ NumArgs 1 []
stringp [_] = return $ Bool False
stringp list@(_ : _) = throwError $ NumArgs 1 list

numberp :: [LispVal] -> ThrowsError LispVal
numberp [Number _] = return $ Bool True
numberp [] = throwError $ NumArgs 1 []
numberp [_] = return $ Bool False
numberp list@(_ : _) = throwError $ NumArgs 1 list

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return . Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                         then throwError $ TypeMismatch "number" $ String n
                         else return . fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool n) = return n
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String n) = return n
unpackStr (Number n) = return $ show n
unpackStr (Bool n) = return $ show n
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` const (return False)

