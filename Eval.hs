module Eval (
	eval
) where

import Parse
import qualified Data.Map as Map
import Data.List (foldl1')
import Control.Monad.Error

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, cons, alt]) = eval pred >>= \result -> case result of
	Bool False -> eval alt
	otherwise -> eval cons
eval (List (Atom f:args)) = mapM eval args >>= apply f

eval bad = throwError $ BadSpecialForm "bad" bad

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "Unrecognized" f) ($ args) (Map.lookup f primitives)

primitives :: Map.Map String ([LispVal] -> ThrowsError LispVal)
primitives = Map.fromList [("+", numOp (+)),
	("-", numOp (-)),
	("*", numOp (*)),
	("/", numOp div),
	("mod", numOp mod),
	("quotient", numOp quot),
	("remainder", numOp rem),
	("=", numBoolOp (==)),
	("/=", numBoolOp (/=)),
	("<", numBoolOp (<)),
	(">", numBoolOp (>)),
	("<=", numBoolOp (<=)),
	(">=", numBoolOp (>=)),
	("||", boolBoolOp (||)),
	("&&", boolBoolOp (&&)),
	("string=?", strBoolOp (==)),
	("string?", strBoolOp (>)),
	("string<=?", strBoolOp (<=)),
	("string>=?", strBoolOp (>=))]

car :: [LispVal] -> ThrowsError LispVal
car [List [x]] = throwError $ TypeMismatch "pair" x
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car bad = throwError $ NumArgs 1 bad

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List [x]] = throwError $ TypeMismatch "pair" x
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [x] y] = return y
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr bad = throwError $ NumArgs 1 bad

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x , y] = return $ DottedList [x] y
cons bad = throwError $ NumArgs 2 bad 
	
numOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numOp op val@[_] = throwError $ NumArgs 2 val
numOp op params = mapM unpackNum params >>= return . Number . foldl1' op

boolOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolOp unpacker op args = if length args /= 2 then throwError $ NumArgs 2 args else
	(unpacker (args !! 0) >>= \left -> unpacker (args !! 1) >>= \right -> return $ Bool $ op left right)

numBoolOp = boolOp unpackNum
boolBoolOp = boolOp unpackBool
strBoolOp = boolOp unpackStr	
	
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s in
	if (null parsed) then (throwError $ TypeMismatch "number" $ String s) else (return $ fst $ parsed !! 0)
unpackNum (List [n]) = unpackNum n
unpackNum bad = throwError $ TypeMismatch "number" bad 

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool bad = throwError $ TypeMismatch "bool" bad

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Bool b) = return $ show b
unpackStr (Number n) = return $ show n
--unpackStr (List [s]) = unpackStr s
unpackStr bad = throwError $ TypeMismatch "string" bad
	