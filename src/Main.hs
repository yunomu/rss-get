module Main where

import System.Console.ParseArgs
    (Arg(..), Argtype(..), ArgsComplete(..))
import qualified System.Console.ParseArgs as Args

import Rss

options :: [Arg Int]
options = 
    [ Arg 1 Nothing Nothing (Args.argDataRequired "uri" ArgtypeString) "URI"
    ]

main :: IO ()
main = do
    as <- Args.parseArgsIO ArgsComplete options
    let uri = (Args.getRequiredArg as 1 :: String)
    getRss uri
