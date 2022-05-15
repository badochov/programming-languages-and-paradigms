module TypeChecker where

import Common (consecutive, posPart, (<.>))
import Control.Monad (foldM, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask, asks)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import qualified Data.Bifunctor
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set (Set, empty, insert)
import Data.Typeable (typeOf)
import Debug.Trace (trace)
import Distribution.ModuleName (main)
import Distribution.PackageDescription (BuildType (Custom))
import Grammar.Abs

type PolyMap = Map.Map PolyIdentToken (ZoyaType, BNFC'Position)

type VariantT = (Set.Set TypeName, BNFC'Position)

type TypeVariant = (TypeName, ZoyaType, BNFC'Position)

data Env = Env
  { vars :: Map.Map VarName (ZoyaType, BNFC'Position),
    variantTypes :: Map.Map TypeName VariantT,
    typeVariants :: Map.Map TypeName TypeVariant,
    poly :: PolyMap
  }

type TypeCheck a = ReaderT Env (ExceptT String (WriterT [String] Identity)) a

data ZoyaType = IntType | FunType ZoyaType ZoyaType | BoolType | CustomType TypeName [ZoyaType] | PolyType PolyIdentToken deriving (Show, Eq)

runTypeCheck :: Env -> TypeCheck a -> (Either String a, [String])
runTypeCheck env ev = runIdentity (runWriterT (runExceptT (runReaderT ev env)))

typeCheckTopDefs :: [TopDef] -> TypeCheck Env
typeCheckTopDefs (h : t) = do
  env <- typeCheckTopDef h
  local (const env) (typeCheckTopDefs t)
typeCheckTopDefs [] = ask

typeCheckTopDef :: TopDef -> TypeCheck Env
typeCheckTopDef (TopDefVar _ varDef) = typeCheckVarDef varDef
typeCheckTopDef (TopDefType pos name variants) = do
  prevDef <- getVariantType name
  case prevDef of
    Nothing -> local (\e -> e {variantTypes = Map.insert name (Set.empty, pos) $ variantTypes e}) (handleVariantTypes variants)
    Just (t, pos) -> throwError $ variantTypeRedefinitionError pos
  where
    handleVariantTypes (h : t) = do
      env <- typeCheckVariantType h name
      local (const env) $ typeCheckTopDef $ TopDefType pos name t
    handleVariantTypes [] = ask
    variantTypeRedefinitionError orgPos = shows "redefinition of variant type " . shows name . posPart pos . shows ", previosly defined" . posPart orgPos $ ""

getVariantType :: TypeName -> TypeCheck (Maybe VariantT)
getVariantType t = asks (Map.lookup t . variantTypes)

getTypeVariant :: TypeName -> TypeCheck (Maybe TypeVariant)
getTypeVariant t = asks (Map.lookup t . typeVariants)

typeCheckVariantType :: VariantType -> TypeName -> TypeCheck Env
typeCheckVariantType (VariantType pos variantName vals) typeName = do
  prevDef <- getVariantType variantName
  case prevDef of
    Nothing -> do
      let t = createZoyaTypeConstructor variantName vals
      asks (\e -> e {typeVariants = Map.insert variantName (typeName, t, pos) $ typeVariants e, variantTypes = addVariant e})
    Just (t, pos) -> throwError $ typeVariantRedefinitionError pos
  where
    typeVariantRedefinitionError orgPos = shows "redefinition of type variant " . shows variantName . posPart pos . shows ", previosly defined" . posPart orgPos $ ""
    addVariant env = Map.adjust (Data.Bifunctor.first (Set.insert variantName)) typeName $ variantTypes env

createZoyaTypeConstructor :: TypeName -> [Type] -> ZoyaType
createZoyaTypeConstructor typeName l = makeLambda pL
  where
    pL = map parseType l
    makeLambda [] = CustomType typeName pL
    makeLambda (h : t) = FunType h $ makeLambda t

parseType :: Type -> ZoyaType
parseType (TypeInt pos) = IntType
parseType (TypeBool pos) = BoolType
parseType (TypePoly pos token) = PolyType token
parseType (TypeFn pos arg res) = FunType (parseType arg) (parseType res)
parseType (TypeCustom pos name args) = CustomType name (map parseType args)
parseType (TypeBrackets pos t) = parseType t

typeCheckVarDef :: VarDef -> TypeCheck Env
typeCheckVarDef (VarDef pos name t expr) = do
  let zoyaType = parseType t
  newEnv <- defineType name zoyaType pos
  exprType <- local (const newEnv) $ inferType expr
  if exprType /= zoyaType
    then throwError $ mismatchedTypeDeclaration zoyaType exprType
    else return newEnv
  where
    mismatchedTypeDeclaration zoyaType exprType = shows "mismatched type declaration of " . shows name . posPart pos . shows "declared " . shows zoyaType . shows "actual " . shows exprType $ ""

defineType :: VarName -> ZoyaType -> BNFC'Position -> TypeCheck Env
defineType name zoyaType pos = do
  env <- ask
  let tE = vars env
  case Map.lookup name tE of
    Just (_, prevPos) -> throwError $ redeclarationError prevPos
    Nothing -> return $ env {vars = Map.insert name (zoyaType, pos) tE}
  where
    redeclarationError prevPos = shows "redeclaration of " . shows name . posPart pos . shows "has differernt type than " . posPart prevPos $ ""

inferType :: Expr -> TypeCheck ZoyaType
inferType (ELambda pos argName t expr) = do
  let argType = parseType t
  env <- defineType argName argType pos
  exprType <- local (const env) (inferType expr)
  return $ FunType argType exprType
inferType (EMatch pos match) = inferTypeMatch match
inferType (ELetIn pos varDef expr) = do
  env <- typeCheckVarDef varDef
  local (const env) (inferType expr)
inferType (ECond pos stmt ifExpr elseExpr) = do
  _ <- checkType BoolType (inferType stmt) pos
  ifT <- inferType ifExpr
  elseT <- inferType elseExpr
  env <- asks poly
  if typesEqual ifT elseT env
    then return ifT
    else throwError $ mismatchedIfTypesError ifT elseT
  where
    mismatchedIfTypesError ifT elseT = shows "mismatched if vars, in true branch: " . shows ifT . shows ", in false branch: " . shows ifT . posPart pos $ ""
inferType (EVar pos varName) = do
  env <- ask
  case Map.lookup varName $ vars env of
    Just (t, pos) -> return t
    Nothing -> throwError $ shows "use of undeclared variable " . shows varName . posPart pos $ ""
inferType (EType pos typeName) = do
  env <- ask
  case Map.lookup typeName $ typeVariants env of
    Just (name, t, pos) -> return t
    Nothing -> throwError $ shows "use of undeclared type " . shows typeName . posPart pos $ ""
inferType (ETypeHelper pos typeName args) = do
  argsTypes <- mapM (inferType . EVar pos) args
  return $ CustomType typeName argsTypes
inferType (EFApp pos fnExpr argExpr) = do
  outerEnv <- ask
  fn <- inferType fnExpr
  case fn of
    FunType argType retType -> do
      throwError "Unimplemnted"
    -- if eq then return expected else throwError $ typeMismatchError expected actual pos
    -- checkType argType (inferType argExpr) pos >> return retType
    _ -> throwError $ shows "tried to call not function" . posPart pos $ ""
inferType (ELitInt _ int) = return IntType
inferType (ELitList pos listArgs) = throwError shouldHaveBeenProccessedError
inferType (EBrackets pos expr) = inferType expr
inferType (ENeg pos expr) = do
  checkType IntType (inferType expr) pos
inferType (ENot pos expr) = do
  checkType IntType (inferType expr) pos
inferType (EListEx pos headExpr tailExpr) = throwError shouldHaveBeenProccessedError
inferType (EMul pos lExpr mulOp rExpr) = checkType IntType (inferType lExpr) pos >> checkType IntType (inferType rExpr) pos >> return IntType
inferType (EAdd pos lExpr addOp rExpr) = checkType IntType (inferType lExpr) pos >> checkType IntType (inferType rExpr) pos >> return IntType
inferType (ERel pos lExpr relOp rExpr) = checkType IntType (inferType lExpr) pos >> checkType IntType (inferType rExpr) pos >> return BoolType
inferType (EAnd pos lExpr rExpr) = checkType BoolType (inferType lExpr) pos >> checkType BoolType (inferType rExpr) pos
inferType (EOr pos lExpr rExpr) = checkType BoolType (inferType lExpr) pos >> checkType BoolType (inferType rExpr) pos

checkType :: ZoyaType -> TypeCheck ZoyaType -> BNFC'Position -> TypeCheck ZoyaType
checkType expected actualM pos = do
  actual <- actualM
  eq <- asks (typesEqual expected actual . poly)
  if eq then return expected else throwError $ typeMismatchError expected actual pos

typesEqual :: ZoyaType -> ZoyaType -> PolyMap -> Bool
typesEqual expected actual polyM = do
  case expected of
    IntType -> canBe IntType actual polyM
    FunType argType retType -> case actual of
      FunType actArgType actRetType -> False -- not implemented
      _ -> False
    BoolType -> canBe BoolType actual polyM
    CustomType typeName zoyaTypes -> False -- not implemented
    PolyType name -> polyOk name actual polyM
  where
    polyOk :: PolyIdentToken -> ZoyaType -> PolyMap -> Bool
    polyOk name actual polyM = case Map.lookup name polyM of
      Nothing -> True
      Just (t, _) -> typesEqual t actual polyM
    canBe :: ZoyaType -> ZoyaType -> PolyMap -> Bool
    canBe ex act polyM = ex == act || exPoly ex act polyM || actPoly ex act polyM
    exPoly :: ZoyaType -> ZoyaType -> PolyMap -> Bool
    exPoly ex act polyM = case ex of
      PolyType name -> polyOk name act polyM
      _ -> False
    actPoly :: ZoyaType -> ZoyaType -> PolyMap -> Bool
    actPoly ex act polyM = False

typeMismatchError :: ZoyaType -> ZoyaType -> BNFC'Position -> String
typeMismatchError expected actual pos = shows "type mismatch, expected: " . shows expected . shows ", actual:" . shows actual . posPart pos $ ""

shouldHaveBeenProccessedError :: String
shouldHaveBeenProccessedError = "should have been preprocessed"

inferTypeMatch :: Match -> TypeCheck ZoyaType
inferTypeMatch (Match pos expr arms) = do
  val <- inferType expr
  inferTypeMatch' val arms
  where
    inferTypeMatch' val [] = throwError $ shows "couldn't match expression" . posPart pos $ ""
    inferTypeMatch' val ((MatchArm pos specifier expr) : t) = do
      envChangerM <- checkMatch specifier val
      case envChangerM of
        Just envChanger -> local envChanger (inferType expr)
        Nothing -> inferTypeMatch' val t
    checkMatch :: MatchArmSpecifier -> ZoyaType -> TypeCheck (Maybe (Env -> Env))
    checkMatch (MatchArmType pos typeName args) val = case val of
      CustomType tn ns -> if tn == typeName && length ns == length args then checkMatchTypeArgs $ zip args ns else return Nothing
      _ -> throwError $ shows "tried to match: " . shows val . posPart pos $ ""
    checkMatch (MatchArmVar pos varName) t = do
      env <- defineType varName t pos
      return $ Just $ const env
    checkMatch (MatchArmFallback pos) val = return $ Just id
    checkMatch MatchArmList {} val = throwError shouldHaveBeenProccessedError
    checkMatchTypeArgs :: [(MatchArmVariantTypeArgument, ZoyaType)] -> TypeCheck (Maybe (Env -> Env))
    checkMatchTypeArgs [] = return $ Just id
    checkMatchTypeArgs (h : t) = do
      fn <- checkMatchTypeArg h
      fn2 <- checkMatchTypeArgs t
      return $ fn <.> fn2
    checkMatchTypeArg :: (MatchArmVariantTypeArgument, ZoyaType) -> TypeCheck (Maybe (Env -> Env))
    checkMatchTypeArg (MatchArmVariantTypeArgumentNested _ specifier, t) = checkMatch specifier t
    checkMatchTypeArg (MatchArmVariantTypeArgumentFallback _, _) = return $ Just id
    checkMatchTypeArg (MatchArmVariantTypeArgumentIdent pos varName, t) = return $ Just $ \e -> e {vars = Map.insert varName (t, pos) $ vars e}

typeCheck :: Program -> TypeCheck Env
typeCheck (Program _ topDefs) = do
  typeCheckTopDefs topDefs

typeCheckProgram :: Program -> (Either String (), [String])
typeCheckProgram program = runTypeCheck newEnv (void $ typeCheck program)

newEnv :: Env
newEnv = Env {vars = Map.empty, poly = Map.empty, variantTypes = Map.empty, typeVariants = Map.empty}
