import System.Environment (getArgs)
import Client

main :: IO ()
main = do
    args <- getArgs
    memosCLI args
