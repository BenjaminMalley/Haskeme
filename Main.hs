import Parse
import Eval
import System.Environment

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
