module Interpreter where

import Common (consecutive, posPart, showsS_, shows_, (<.>))
import Control.Monad.Except (ExceptT, MonadError (catchError), runExceptT, throwError)
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

type StackPosOrValue = Either Int Value

type Env = Map.Map VarName StackPosOrValue

type ZoyaTypes = Map.Map TypeName Value

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

prettyString :: Value -> Eval String
prettyString (FunVal env varName expr) = return $ shows_ "function with argument" . shows varName $ ""
prettyString (CustomType typeName args) =
  do
    mArgs <- mapM toVal args
    sArgs <- mapM prettyString mArgs
    return $ shows_ "type " . shows typeName . shows_ " " . showsS_ sArgs $ ""
  where
    toVal :: StackPosOrValue -> Eval Value
    toVal (Left pos) = execExprFromStack pos
    toVal (Right v) = return v
prettyString x = return $ show x

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
evalTopDef (TopDefType _ _ _ []) = ask
evalTopDef (TopDefType pos name _ (h : t)) = do
  evalVariantType h name
  evalTopDef $ TopDefType pos name [] t

evalVariantType :: VariantType -> TypeName -> Eval Env
evalVariantType (VariantType pos variantName vals) typeName = do
  state <- get
  put $
    state
      { types = Map.insert variantName (createZoyaTypeConstructor pos variantName vals) (types state)
      }
  ask

createZoyaTypeConstructor :: BNFC'Position -> TypeName -> [Type] -> Value
createZoyaTypeConstructor pos typeName l = makeLambda typeConstructor pos numArgs
  where
    numArgs = length l
    toVarName = VarName . show
    typeConstructor = ETypeHelper pos typeName (map toVarName $ consecutive numArgs)
    makeLambda :: Expr -> BNFC'Position -> Int -> Value
    makeLambda applyTo pos 0 = CustomType typeName []
    makeLambda applyTo pos 1 = FunVal newEnv (toVarName 1) applyTo
    makeLambda applyTo pos n = makeLambda (ELambda pos (toVarName n) typePart applyTo) pos (n -1)
    typePart = TypeInt Nothing -- random type as it's not checked

evalVarDef :: VarDef -> Eval Env
evalVarDef (VarDef pos name _ expr) = do
  state <- get
  let curStack = stack state
  env <- ask
  let env' = Map.insert name (Left $ top curStack) env
  put $ state {stack = addToStack curStack expr pos env'}
  return env'

evalExpr :: Expr -> Eval Value
evalExpr (ELambda pos argName _ expr) = do
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
  return $ types state ! typeName
evalExpr (ETypeHelper pos typeName args) = do
  env <- ask
  return $ CustomType typeName $ map (env !) args
evalExpr (EFApp pos fnExpr argExpr) = handle `catchError` wrapError
  where
    handle = do
      outerEnv <- ask
      fn <- evalExpr fnExpr
      case fn of
        FunVal fEnv argName expr -> do
          state <- get
          put $ state {stack = addToStack (stack state) argExpr pos outerEnv}
          local (const (Map.insert argName (Left $top $ stack state) fEnv)) (evalExpr expr)
        _ -> throwError $ typeErr pos
    wrapError :: String -> Eval Value
    wrapError e = throwError $ shows_ "in function aplication" . posPart pos . shows_ ":\n" $ e
evalExpr (ELitInt _ int) = return $ IntVal int
evalExpr (ELitList pos listArgs) = throwError shouldHaveBeenProccessedError
evalExpr (ELitListEmpty pos _) = throwError shouldHaveBeenProccessedError
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
    (IntVal l, IntVal r) -> case mulOp of
      (Times _) -> return $ IntVal (l * r)
      (Div _) -> if r == 0 then throwError $ shows_ "division by zero" . posPart pos $ "" else return $ IntVal (l `div` r)
      (Mod _) -> if r == 0 then throwError $ shows_ "modulo by zero" . posPart pos $ "" else return $ IntVal (l `mod` r)
    _ -> throwError $ typeErr pos
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
    evalMatch' val [] = throwError $ shows_ "couldn't match expression" . posPart pos $ ""
    evalMatch' val ((MatchArm pos specifier expr) : t) = do
      envChangerM <- checkMatch specifier val
      case envChangerM of
        Just envChanger -> local envChanger (evalExpr expr)
        Nothing -> evalMatch' val t
    checkMatch :: MatchArmSpecifier -> Value -> Eval (Maybe (Env -> Env))
    checkMatch (MatchArmType pos typeName args) val = case val of
      CustomType tn ns -> if tn == typeName && length ns == length args then checkMatchTypeArgs $ zip args ns else return Nothing
      _ -> throwError $ typeErr pos
    checkMatch (MatchArmVar pos varName) val = return $ Just $ Map.insert varName $ Right val
    checkMatch (MatchArmFallback pos) val = return $ Just id
    checkMatch MatchArmList {} val = throwError shouldHaveBeenProccessedError
    checkMatch (MatchArmBrackets pos m) val = checkMatch m val
    checkMatchTypeArgs :: [(MatchArmVariantTypeArgument, StackPosOrValue)] -> Eval (Maybe (Env -> Env))
    checkMatchTypeArgs [] = return $ Just id
    checkMatchTypeArgs (h : t) = do
      fn <- checkMatchTypeArg h
      fn2 <- checkMatchTypeArgs t
      return $ fn <.> fn2
    checkMatchTypeArg :: (MatchArmVariantTypeArgument, StackPosOrValue) -> Eval (Maybe (Env -> Env))
    checkMatchTypeArg (MatchArmVariantTypeArgumentNested _ specifier, sOrV) = do
      val <- case sOrV of
        Left stackPos -> execExprFromStack stackPos
        Right val -> return val
      checkMatch specifier val
    checkMatchTypeArg (MatchArmVariantTypeArgumentFallback _, _) = return $ Just id
    checkMatchTypeArg (MatchArmVariantTypeArgumentIdent _ varName, stackPos) = return $ Just $ Map.insert varName stackPos

typeErr :: BNFC'Position -> String
typeErr pos = shows_ "type error" . posPart pos $ ""

eval :: Program -> Eval String
eval (Program _ topDefs) = do
  env <- getDefs topDefs
  res <- local (const env) (evalExpr mainExpr)
  prettyString res

interpret :: Program -> ((Either String String, [String]), StateType)
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
