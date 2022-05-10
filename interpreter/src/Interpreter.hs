module Interpreter where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask, asks)
import Control.Monad.State (MonadState (get, put, state), State, StateT (runStateT), modify)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Typeable (typeOf)
import Debug.Trace (trace)
import Distribution.ModuleName (main)
import Grammar.Abs
import Language.Haskell.TH (Dec (ValD))

type StackPosOrValue = Either Int Value

type Env = Map.Map VarName StackPosOrValue

type ZoyaTypes = Map.Map TypeName Int

data StateType = StateType
  { stack :: Stack,
    types :: ZoyaTypes
  }
  deriving (Show)

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT StateType Identity))) a

data Value = IntVal Integer | FunVal Env VarName Expr | BoolVal Bool | CustomType TypeName [StackPosOrValue] deriving (Show)

type StackValue = (Expr, BNFC'Position, Env)

data Stack = Stack
  { st :: [StackValue],
    top :: Int
  }
  deriving (Show)

runEval :: Env -> StateType -> Eval a -> ((Either String a, [String]), StateType)
runEval env st ev = runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

mainExpr :: Expr
mainExpr = EVar Nothing (VarName "main")

getDefs :: [TopDef] -> Eval Env
getDefs (h : t) = do
  env <- evalTopDef h
  local (const env) (getDefs t)
getDefs [] = ask

evalTopDef :: TopDef -> Eval Env
evalTopDef (TopDefVar _ varDef) = evalVarDef varDef
evalTopDef (TopDefType _ _ []) = ask
evalTopDef (TopDefType pos name (h : t)) = do
  evalVariantType h name
  evalTopDef $ TopDefType pos name t

evalVariantType :: VariantType -> TypeName -> Eval Env
evalVariantType (VariantType pos variantName vals) typeName = do
  state <- get
  env <- ask
  let s = stack state
  put $
    state
      { types = Map.insert variantName (top s) (types state),
        stack = addToStack s (createZoyaTypeConstructor pos variantName vals) pos env
      }
  return env

createZoyaTypeConstructor :: BNFC'Position -> TypeName -> [VariantTypeArgument] -> Expr
createZoyaTypeConstructor pos typeName l =
  let res = makeLambda' typeConstructor pos numArgs
   in trace (show res) res
  where
    numArgs = length l
    toVarName = VarName . show
    typeConstructor = ETypeHelper pos typeName (map toVarName $ consequtive numArgs)
    makeLambda' applyTo pos 0 = applyTo
    makeLambda' applyTo pos n = makeLambda' (ELambda pos (toVarName n) applyTo) pos (n -1)

consequtive :: Int -> [Int]
consequtive n =
  consequtive' [] n
  where
    consequtive' acc 0 = acc
    consequtive' acc n = consequtive' (n : acc) (n -1)

evalVarDef :: VarDef -> Eval Env
evalVarDef (VarDef pos name expr) = do
  state <- get
  let curStack = stack state
  env <- ask
  let env' = Map.insert name (Left $ top curStack) env
  put $ state {stack = addToStack curStack expr pos env'}
  return env'

evalExpr :: Expr -> Eval Value
evalExpr (ELambda pos argName expr) = do
  env <- ask
  return $ FunVal env argName expr
evalExpr (EMatch pos match) = evalMatch match
evalExpr (ELetIn pos varDef expr) = do
  env <- evalVarDef varDef
  local (const env) (evalExpr expr)
evalExpr (ECond pos stmt ifExpr elseExpr) = do
  val <- evalExpr stmt
  case val of
    BoolVal b -> evalExpr $ if b then ifExpr else elseExpr
    _ -> throwError $ typeErr pos
evalExpr (EVar pos varName) = do
  env <- ask
  case env ! varName of
    Left stackPos -> execExprFromStack stackPos
    Right val -> return val
evalExpr (EType pos typeName) = do
  state <- get
  execExprFromStack $ types state ! typeName
evalExpr (ETypeHelper pos typeName args) = do
  env <- ask
  return $ CustomType typeName $ map (env !) args
evalExpr (EFApp pos fnExpr argExpr) = do
  outerEnv <- ask
  fn <- evalExpr fnExpr
  case fn of
    FunVal fEnv argName expr -> do
      state <- get
      put $ state {stack = addToStack (stack state) argExpr pos outerEnv}
      local (const (Map.insert argName (Left $top $ stack state) fEnv)) (evalExpr expr)
    _ -> throwError $ typeErr pos
evalExpr (ELitInt _ int) = return $ IntVal int
evalExpr (ELitList pos listArgs) = throwError shouldHaveBeenProccessedError
evalExpr (EBrackets pos expr) = evalExpr expr
evalExpr (ENeg pos expr) = do
  val <- evalExpr expr
  case val of
    IntVal n -> return $ IntVal (- n)
    _ -> throwError $ typeErr pos
evalExpr (ENot pos expr) = do
  val <- evalExpr expr
  case val of
    BoolVal b -> return $ BoolVal (not b)
    _ -> throwError $ typeErr pos
evalExpr (EListEx pos headExpr tailExpr) = throwError shouldHaveBeenProccessedError
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

shouldHaveBeenProccessedError :: String
shouldHaveBeenProccessedError = "should have been preprocessed"

evalMatch :: Match -> Eval Value
evalMatch (Match pos expr arms) = do
  val <- evalExpr expr
  evalMatch' val arms
  where
    evalMatch' val [] = trace (show "ARMS " ++ show arms) throwError $ shows "couldn't match expression" . posPart pos $ ""
    evalMatch' val ((MatchArm pos specifier expr) : t) = do
      (isMatch, envChanger) <- trace ("PRE " ++ show val) checkMatch specifier val
      if isMatch then local envChanger (evalExpr expr) else evalMatch' val t
    checkMatch :: MatchArmSpecifier -> Value -> Eval (Bool, Env -> Env)
    checkMatch (MatchArmType pos typeName args) val = case val of
      CustomType tn ns -> if tn == typeName && length ns == length args then checkMatchTypeArgs $ zip args ns else return (False, id)
      _ -> trace (show val) throwError $ typeErr pos
    checkMatch (MatchArmVar pos varName) val = return (True, Map.insert varName $ Right val)
    checkMatch (MatchArmFallback pos) val = return (True, id)
    checkMatch MatchArmList {} val = throwError shouldHaveBeenProccessedError
    checkMatchTypeArgs :: [(MatchArmVariantTypeArgument, StackPosOrValue)] -> Eval (Bool, Env -> Env)
    checkMatchTypeArgs [] = return (True, id)
    checkMatchTypeArgs (h : t) = do
      (ok, fn) <- checkMatchTypeArg h
      if ok
        then do
          (ok2, fn2) <- checkMatchTypeArgs t
          return (ok2, fn . fn2)
        else return (False, id)
    checkMatchTypeArg :: (MatchArmVariantTypeArgument, StackPosOrValue) -> Eval (Bool, Env -> Env)
    checkMatchTypeArg (MatchArmVariantTypeArgumentNested _ specifier, sOrV) = do
      val <- case sOrV of
        Left stackPos -> execExprFromStack stackPos
        Right val -> return val
      checkMatch specifier val
    checkMatchTypeArg (MatchArmVariantTypeArgumentFallback _, _) = return (True, id)
    checkMatchTypeArg (MatchArmVariantTypeArgumentIdent _ varName, stackPos) = return (True, Map.insert varName stackPos)

typeErr :: BNFC'Position -> String
typeErr pos = shows "type error" . posPart pos $ ""

posPart :: BNFC'Position -> ShowS
posPart Nothing = shows ""
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

execExprFromStack :: Int -> Eval Value
execExprFromStack stackPos = do
  state <- get
  let (expr, pos, env) = getFromStack (stack state) stackPos
  local (const env) (evalExpr expr)

newEnv :: Env
newEnv = Map.empty

newZoyaTypes :: ZoyaTypes
newZoyaTypes = Map.empty

newState :: StateType
newState = StateType {stack = newStack, types = newZoyaTypes}

newStack :: Stack
newStack = Stack {st = [], top = 0}
