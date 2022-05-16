-- | Program to test parser.
module Main where

import Common (preprocess, mergePrograms)
import Grammar.Abs (Program)
import Grammar.Par (myLexer, pProgram)
import Grammar.Skel ()
import Interpreter (StateType, Value, interpret, newState)
import System.Environment (getArgs)
import System.IO (hPrint, stderr, hPutStrLn)
import TypeChecker (typeCheckProgram)
import ZoyaPrelude (prelude)

type Handler = Program -> ((Either String String, [String]), StateType)

runFile :: Handler -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

handleInterpret :: Handler
handleInterpret p =
  let (res, log) = typeCheckProgram p
   in case res of
        Left err -> ((Left $ "TYPE CHECKER ERROR:\n" ++ err, log), newState)
        Right _ -> interpret p

mergeWithPrelude :: Program -> Program
mergeWithPrelude = mergePrograms prelude

run :: Handler -> String -> IO ()
run p s =
  case tok of
    Left err -> handleErr err
    Right prog -> handleOk prog
  where
    lexed = myLexer s
    tok = pProgram lexed
    handleErr = hPutStrLn stderr
    handleOk program =
      let preprocessed = preprocess $ mergeWithPrelude program
          ((res, interpreterLog), state) = p preprocessed
       in do
            print interpreterLog
            case res of
              Left err -> handleErr err
              Right r -> putStrLn r

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  <filename>      Interpret given file",
        "  (no arugments)  Interpret stdin"
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run handleInterpret
    fs -> mapM_ (runFile handleInterpret) fs
