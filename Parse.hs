module Parse (
	LispVal (..),
	LispError (..),
	ThrowsError,
	parseExpr,
	readExpr,
	trap,
	extract
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Debug.Trace (traceShow)
import Numeric (readOct, readHex)
import Control.Monad.Error

data LispError = NumArgs Integer [LispVal]
	| TypeMismatch String [LispVal]
	| Parser ParseError
	| BadSpecialForm String LispVal
	| NotFunction String String
	| UnboundVar String String
	| Default String deriving (Show)

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Bool Bool
	| Float Double
	| Char Char deriving (Show)
	
type ThrowsError = Either LispError

instance Error LispError where
	noMsg = Default "An error has occurred"
	strMsg = Default
	
trap :: ThrowsError String -> ThrowsError String
trap action = catchError action (return . show)

extract :: ThrowsError a -> a
extract (Right v) = v

describe :: LispVal -> String
describe (Atom _) = "atom"
describe (List _) = "list"
describe (DottedList _ _) = "dotted list"
describe (Number _) = "number"
describe (String _) = "string"
describe (Bool _) = "bool"
describe (Float _) = "float"
describe (Char _) = "char"

parseExpr :: Parser LispVal
parseExpr = parseNumber
	<|> parseQuoted
	<|> parseParen
	<|> parseAtom
	<|> parseString
	<|> parseChar

parseParen :: Parser LispVal
parseParen = (char '(' >>
	((try parseList) <|> parseDottedList) >>= 
	\x -> char ')' >>
	return x)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = endBy parseExpr spaces >>=
	\head -> char '.' >>
	spaces >>
	parseExpr >>=
	\tail -> return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >>
	parseExpr >>=
	\x -> return $ List [Atom "quote", traceShow x x]

parseNumber :: Parser LispVal
parseNumber = parseInt <|> parseFloat <|> parseOct <|> parseFloat

parseDec :: Parser LispVal
parseDec = liftM (Number . read) $ many1 digit

parseInt :: Parser LispVal
parseInt = parseDec <|> parseOct <|> parseHex
			
parseFloat :: Parser LispVal
parseFloat = liftM (Float . read) $ many1 digit >> char '.' >> many1 digit

parseOct :: Parser LispVal
parseOct = liftM (Number . fst . head . readOct) $ string "#o" >> many1 (oneOf "01234567")

parseHex :: Parser LispVal
parseHex = liftM (Number . fst . head . readHex) $ string "#h" >> many1 (oneOf "012345")

parseAtom :: Parser LispVal
parseAtom = (letter <|> symbol) >>=
	\first -> many (letter <|> digit <|> symbol) >>=
	\rest -> return $ case first : rest of
			"#t" -> Bool True
			"#f" -> Bool False
			otherwise -> Atom otherwise
	
parseString :: Parser LispVal
parseString = char '"' >>
	many (noneOf "\"" <|> (char '\\' >> oneOf "\"nrt\\")) >>=
	\s -> char '"' >>
	(return $ String s)
	
parseChar :: Parser LispVal
parseChar = string "#\\" >>
	((sc "space" ' ') <|> (sc "newline" '\n') <|> anyChar) >>=
	\c -> return $ Char c where
	sc x y = string x >> return y

symbol :: Parser Char
symbol = oneOf "!@#$%^&*()"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> throwError $ Parser err
	Right x -> return x

