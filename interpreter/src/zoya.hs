-- | Program to test parser.
module Main where

import Grammar.Abs (Program)
import Grammar.Par (myLexer, pProgram)
import Grammar.Skel ()
import Interpreter (StateType (stack), Value, interpret, newState, Stack (st))
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

type Handler = Program -> ((Either String Value, [String]), StateType)

runFile :: Handler -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

handleInterpret :: Handler
handleInterpret = interpret

run :: Handler -> String -> IO ()
run p s =
  case tok of
    Left err -> handleErr err
    Right expr -> handleOk expr
  where
    lexed = myLexer s
    tok = pProgram lexed
    handleErr = hPrint stderr
    handleOk program = 
      let ((res, interpreterLog), state) = p program in
      do 
        -- print state
        print interpreterLog
        case res of
          Left err -> handleErr err
          Right r -> print r

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
