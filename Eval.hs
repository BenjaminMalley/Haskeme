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
	("remainder", numOp rem)]

numOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numOp op val@[_] = throwError $ NumArgs 2 val
numOp op params = mapM unpack params >>= return . Number . foldl1' op

unpack :: LispVal -> ThrowsError Integer
unpack (Number n) = return n
unpack (String s) = let parsed = reads s in
	if (null parsed) then (throwError $ TypeMismatch "number" $ String s) else (return $ fst $ parsed !! 0)
unpack (List [n]) = unpack n
unpack bad = throwError $ TypeMismatch "number" bad 
	