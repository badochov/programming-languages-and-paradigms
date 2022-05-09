module Interpreter where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask, asks)
import Control.Monad.State (MonadState (get, put), State, StateT (runStateT), modify)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Typeable (typeOf)
import Debug.Trace (trace)
import Distribution.ModuleName (main)
import Grammar.Abs

type ZoyaType = String -- placeholder

type Env = Map.Map VarName Int

data StateType = StateType
  { types :: Map.Map TypeName ZoyaType,
    stack :: Stack
  }
  deriving (Show)

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT StateType Identity))) a

data Value = IntVal Integer | FunVal Env VarName Expr | BoolVal Bool deriving (Show)

type StackValue = (Expr, BNFC'Position, Env)

data Stack = Stack
  { st :: [StackValue],
    top :: Int
  }
  deriving (Show)

runEval :: Env -> StateType -> Eval a -> ((Either String a, [String]), StateType)
runEval env st ev = runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

mainExpr :: Expr
mainExpr = EFApp Nothing (VarName "main") []

getDefs :: [TopDef] -> Eval Env
getDefs (h : t) = do
  env <- evalTopDef h
  local (const env) (getDefs t)
getDefs [] = ask

evalTopDef :: TopDef -> Eval Env
evalTopDef (TopDefVar _ varDef) = evalVarDef varDef
evalTopDef (TopDefType _ _name _opts) = throwError "Not implemented"

evalVarDef :: VarDef -> Eval Env
evalVarDef (VarDef pos name expr) = do
  state <- get
  let curStack = stack state
  env <- ask
  put $ state {stack = addToStack curStack expr pos env}
  asks (Map.insert name (top curStack))

evalExpr :: Expr -> Eval Value
evalExpr (LambdaExpr pos argName expr) = do
  env <- ask
  return $ FunVal env argName expr
evalExpr (MatchExpr pos match) = throwError "Not implemneted"
evalExpr (ELetIn pos varDef expr) = do
  env <- evalVarDef varDef
  local (const env) (evalExpr expr)
evalExpr (ECond pos stmt ifExpr elseExpr) = do
  val <- evalExpr stmt
  case val of
    BoolVal b -> evalExpr $ if b then ifExpr else elseExpr
    _ -> throwError $ typeErr pos
evalExpr (EVar pos varName) = evalExpr (EFApp pos varName [])
evalExpr (EType pos typeName) = evalExpr (ETApp pos typeName [])
evalExpr (EFApp pos fnName args) = do
  outerEnv <- ask
  case Map.lookup fnName outerEnv of
    Nothing -> throwError $ shows "function " . shows fnName . shows " not found " . posPart pos $ ""
    Just stackPos -> do
      state <- get
      let (expr, pos, env) = getFromStack (stack state) stackPos
      res <- local (const env) (evalExpr expr)
      let applyArgs val [] = return val
          applyArgs val (h : t) = case val of
                  FunVal fEnv argName expr -> do
                    state <- get
                    put $ state {stack = addToStack (stack state) h pos outerEnv}
                    val <- local (const (Map.insert argName (top $ stack state) fEnv)) (evalExpr expr)
                    applyArgs val t
                  _ -> throwError $ typeErr pos
      applyArgs res args
evalExpr (ETApp pos tName args) = throwError "Not implemneted"
evalExpr (ELitInt _ int) = return $ IntVal int
evalExpr (ELitList pos listArgs) = throwError "Not implemneted"
evalExpr (EBrackets pos expr) = evalExpr expr
evalExpr (Neg pos expr) = do
  val <- evalExpr expr
  case val of
    IntVal n -> return $ IntVal (- n)
    _ -> throwError $ typeErr pos
evalExpr (Not pos expr) = do
  val <- evalExpr expr
  case val of
    BoolVal b -> return $ BoolVal (not b)
    _ -> throwError $ typeErr pos
evalExpr (EListEx pos headExpr tailExpr) = throwError "Not implemneted"
evalExpr (EMul pos lExpr mulOp rExpr) = do
  lVal <- evalExpr lExpr
  rVal <- evalExpr rExpr
  case (lVal, rVal) of
    (IntVal l, IntVal r) -> return $ IntVal (getMulop mulOp l r)
    _ -> throwError $ typeErr pos
  where
    getMulop (Times _) = (*)
    getMulop (Div _) = div
    getMulop (Mod _) = mod
evalExpr (EAdd pos lExpr addOp rExpr) = do
  lVal <- evalExpr lExpr
  rVal <- evalExpr rExpr
  case (lVal, rVal) of
    (IntVal l, IntVal r) -> return $ IntVal (getAddOp addOp l r)
    _ -> throwError $ typeErr pos
  where
    getAddOp (Plus _) = (+)
    getAddOp (Minus _) = (-)
evalExpr (ERel pos lExpr relOp rExpr) = do
  lVal <- evalExpr lExpr
  rVal <- evalExpr rExpr
  case (lVal, rVal) of
    (IntVal l, IntVal r) -> return $ BoolVal (getRelOp relOp l r)
    _ -> throwError $ typeErr pos
  where
    getRelOp (EQU _) = (==)
    getRelOp (NE _) = (/=)
    getRelOp (LTH _) = (<=)
    getRelOp (LE _) = (<)
    getRelOp (GE _) = (>=)
    getRelOp (GTH _) = (>)
evalExpr (EAnd pos lExpr rExpr) = do
  lVal <- evalExpr lExpr
  rVal <- evalExpr rExpr
  case (lVal, rVal) of
    (BoolVal l, BoolVal r) -> return $ BoolVal (l && r)
    _ -> throwError $ typeErr pos
evalExpr (EOr pos lExpr rExpr) = do
  lVal <- evalExpr lExpr
  rVal <- evalExpr rExpr
  case (lVal, rVal) of
    (BoolVal l, BoolVal r) -> return $ BoolVal (l || r)
    _ -> throwError $ typeErr pos

typeErr :: BNFC'Position -> String
typeErr pos = shows "type error" . posPart pos $ ""

posPart :: BNFC'Position -> ShowS
posPart Nothing = ("" ++)
posPart (Just pos) = shows "at" . shows pos

eval :: Program -> Eval Value
eval (Program _ topDefs) = do
  env <- getDefs topDefs
  local (const env) (evalExpr mainExpr)

interpret :: Program -> ((Either String Value, [String]), StateType)
interpret program = runEval newEnv newState $ eval program

addToStack :: Stack -> Expr -> BNFC'Position -> Env -> Stack
addToStack stack expr pos env = Stack {st = (expr, pos, env) : st stack, top = top stack + 1}

getFromStack :: Stack -> Int -> StackValue
getFromStack stack n = st stack !! (top stack - 1 - n)

newEnv :: Env
newEnv = Map.empty

newState :: StateType
newState = StateType {stack = newStack, types = Map.empty}

newStack :: Stack
newStack = Stack {st = [], top = 0}
