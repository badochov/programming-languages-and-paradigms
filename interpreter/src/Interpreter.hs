module Interpreter where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.State (State, StateT (runStateT))
import Control.Monad.Writer (WriterT (runWriterT))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Grammar.Abs

type ZoyaType = String -- placeholder

data Env = Env
  { vars :: Map.Map VarName Expr,
    types :: Map.Map TypeName ZoyaType
  }
  deriving (Show)

type StateType = Integer

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT StateType IO))) a

data Value = IntVal Integer | FunVal Env VarName Expr | BoolVal Bool deriving (Show)

runEval :: Env -> StateType -> Eval a -> IO ((Either String a, [String]), StateType)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval :: Program -> Eval Value
eval (Program _ topDefs) = head $ map evalTopDef topDefs

evalTopDef :: TopDef -> Eval Value
evalTopDef (TopDefVar _ varDef) = evalVarDef varDef
evalTopDef (TopDefType _ _name _opts) = throwError "Not implemented"

evalVarDef :: VarDef -> Eval Value
evalVarDef (VarDef _ name expr) = do
  env <- ask
  return $ IntVal 0

interpret :: Program -> IO ((Either String Value, [String]), StateType)
interpret program = runEval newEnv 0 (eval program)

newEnv :: Env
newEnv = Env {vars = Map.empty, types = Map.empty}
