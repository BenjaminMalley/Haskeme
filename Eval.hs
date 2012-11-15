module Eval (
	eval
) where

import Parse
import qualified Data.Map as Map

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f:args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ Map.lookup f primitives

primitives :: Map.Map String ([LispVal] -> LispVal)
primitives = Map.fromList [("+", numOp (+)),
	("-", numOp (-)),
	("*", numOp (*)),
	("/", numOp div),
	("mod", numOp mod),
	("quotient", numOp quot),
	("remainder", numOp rem)]

numOp op params = Number $ foldl1 op $ map unpack params where
	unpack (Number n) = n
	--TODO
	--unpack (Float n) = n
	unpack (String s) = let parsed = reads s in
		if null parsed then 0 else fst $ parsed !! 0
	unpack (List [n]) = unpack n
	unpack _ = 0
	

