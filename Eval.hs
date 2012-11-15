module Eval (
	eval
) where

import Parse
import Data.Map as Map

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f:args)) = apply f $ map eval args

apply :: String

primitives :: Map.Map String ([LispVal] -> LispVal)
primitives = Map.fromList [("+", numOp (+)),
	("-", numOp (-)),
	("*", numOp (*)),
	("/", numOp div),
	("mod", numOp mod),
	("quotient", numOp quot),
	("remainder", numOp rem)]

numOp :: (Num a) => (a -> a -> a) -> [LispVal] -> LispVal
numOp op params = Number $ foldl1 op $ map unpackNum params
