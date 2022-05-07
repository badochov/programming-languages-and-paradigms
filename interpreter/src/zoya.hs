-- | Program to test parser.
module Main where

import Control.Monad (when)
import Grammar.Abs (Program)
import Grammar.Lex (Token, mkPosToken)
import Grammar.Par (myLexer, pProgram)
import Grammar.Print (Print, printTree)
import Grammar.Skel ()
import Interpreter (StateType, Value, interpret)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

type Handler = Program -> IO ((Either String Value, [String]), StateType)

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
    lex = myLexer s
    tok = pProgram lex
    handleErr = hPrint stderr
    handleOk expr = do
      ((res, log), state) <- interpret expr
      print state
      print log
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
