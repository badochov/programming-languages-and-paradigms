module Interpreter where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (State, StateT (runStateT))
import Control.Monad.Writer (WriterT (runWriterT))
import qualified Data.Map as Map
import Data.Maybe ()
import Grammar.Abs (Expr, Program)
import qualified Grammar.Abs

type Name = String

type Env = Map.Map Name Value

type StateType = Integer

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT StateType IO))) a

data Value = IntVal Integer | FunVal Env Name Expr | BoolVal Bool deriving (Show)

runEval :: Env -> StateType -> Eval a -> IO ((Either String a, [String]), StateType)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval :: Program -> Eval Value
eval expr = do
  return $ IntVal 42

interpret :: Program -> IO ((Either String Value, [String]), StateType)
interpret expr = runEval Map.empty 0 (eval expr)