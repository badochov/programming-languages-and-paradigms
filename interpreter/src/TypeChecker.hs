{-# LANGUAGE TupleSections #-}

module TypeChecker where

import Common (consecutive, posPart, shows_, (<.>))
import Control.Exception (throw)
import Control.Monad (foldM, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask, asks, when)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import qualified Data.Bifunctor
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set (Set, empty, insert, member)
import Debug.Trace (trace)
import Grammar.Abs

type PolyId = Int

type PolyMap = Map.Map PolyId (ZoyaType, BNFC'Position)

type VariantT = (Set.Set TypeName, [PolyId], BNFC'Position)

type TypeVariant = (TypeName, ZoyaType, [ZoyaType], BNFC'Position)

data Env = Env
  { vars :: Map.Map VarName (ZoyaType, BNFC'Position),
    variantTypes :: Map.Map TypeName VariantT,
    typeVariants :: Map.Map TypeName TypeVariant,
    polyIdToType :: PolyMap,
    polyToId :: Map.Map PolyIdentToken PolyId
  }
  deriving (Show)

type TypeCheck a = ReaderT Env (ExceptT String (WriterT [String] Identity)) a

data ZoyaType
  = IntType
  | FunType ZoyaType ZoyaType
  | BoolType
  | CustomType TypeName [ZoyaType] -- FIXME handle polyIdToType
  | PolyType PolyIdentToken PolyId
  deriving (Show, Eq)

runTypeCheck :: Env -> TypeCheck a -> (Either String a, [String])
runTypeCheck env ev = runIdentity (runWriterT (runExceptT (runReaderT ev env)))

typeCheckTopDefs :: [TopDef] -> TypeCheck Env
typeCheckTopDefs (h : t) = do
  env <- typeCheckTopDef h
  local (const env) (typeCheckTopDefs t)
typeCheckTopDefs [] = ask

typeCheckTopDef :: TopDef -> TypeCheck Env
typeCheckTopDef (TopDefVar _ varDef) = typeCheckVarDef varDef
typeCheckTopDef (TopDefType pos name polies variants) = do
  prevDef <- getVariantType name
  case prevDef of
    Nothing -> do
      envChanger <- getEnvChanger
      local envChanger (handleVariantTypes variants)
    Just (t, tids, pos) -> throwError $ variantTypeRedefinitionError pos
  where
    getEnvChanger = do
      (env, pids) <- updatePolies polies pos
      let newVariantTypes = Map.insert name (Set.empty, pids, pos) $ variantTypes env
      return $ const env {variantTypes = newVariantTypes}
    handleVariantTypes (h : t) = do
      env <- typeCheckVariantType h name
      local (const env) $ handleVariantTypes t
    handleVariantTypes [] = ask
    variantTypeRedefinitionError orgPos = shows_ "redefinition of variant type " . shows name . posPart pos . shows_ ", previosly defined" . posPart orgPos $ ""

updatePolies :: [PolyIdentToken] -> BNFC'Position -> TypeCheck (Env, [PolyId])
updatePolies toks pos = updatePolies' toks []
  where
    updatePolies' :: [PolyIdentToken] -> [PolyId] -> TypeCheck (Env, [PolyId])
    updatePolies' [] ids = asks (,ids)
    updatePolies' (h : t) ids =
      do
        env <- ask
        let pid = polyToId env
        let tid = Map.size pid
        if Map.member h pid
          then throwError $ shows h . shows "already defined" . posPart pos $ ""
          else
            let newEnv = env {polyToId = Map.insert h tid pid}
             in local (const newEnv) $ updatePolies' t (tid : ids)

getVariantType :: TypeName -> TypeCheck (Maybe VariantT)
getVariantType t = asks (Map.lookup t . variantTypes)

getTypeVariant :: TypeName -> TypeCheck (Maybe TypeVariant)
getTypeVariant t = asks (Map.lookup t . typeVariants)

typeCheckVariantType :: VariantType -> TypeName -> TypeCheck Env
typeCheckVariantType (VariantType pos variantName vals) typeName = do
  prevDef <- getTypeVariant variantName
  case prevDef of
    Nothing -> do
      vT <- mapM parseType vals
      let t = createZoyaTypeConstructor typeName vT
      asks (\e -> e {typeVariants = Map.insert variantName (typeName, t, vT, pos) $ typeVariants e, variantTypes = addVariant e})
    Just (_, _, _, pos) -> throwError $ typeVariantRedefinitionError pos
  where
    typeVariantRedefinitionError orgPos = shows_ "redefinition of type variant " . shows variantName . posPart pos . shows_ ", previosly defined" . posPart orgPos $ ""
    addVariant env = Map.adjust (\(vs, tids, pos) -> (Set.insert variantName vs, tids, pos)) typeName $ variantTypes env

createZoyaTypeConstructor :: TypeName -> [ZoyaType] -> ZoyaType
createZoyaTypeConstructor typeName ts = makeLambda ts
  where
    makeLambda [] = CustomType typeName ts
    makeLambda (h : t) = FunType h $ makeLambda t

parseType :: Type -> TypeCheck ZoyaType
parseType (TypeInt pos) = return IntType
parseType (TypeBool pos) = return BoolType
-- parseType (PolyId pos token) = return $ PolyType token -- FIXME polyIdToType types should be mapped to unique ids
parseType (TypeFn pos arg res) = FunType <$> parseType arg <*> parseType res
parseType (TypeCustom pos name ts) = do
  vT <- asks variantTypes
  if Map.member name vT
    then do
      parsedTs <- mapM parseType ts
      return $ CustomType name parsedTs
    else throwError $ shows_ "unknown type " . shows name . posPart pos $ ""
parseType (TypeBrackets pos t) = parseType t
parseType (TypePoly pos t) = return $ PolyType t 0 -- polyId is filled in defineType function

typeCheckVarDef :: VarDef -> TypeCheck Env
typeCheckVarDef (VarDef pos name t expr) = do
  zoyaType <- parseType t
  newEnv <- defineType name zoyaType pos
  exprType <- local (const newEnv) $ inferType expr
  if exprType /= zoyaType
    then throwError $ mismatchedTypeDeclaration zoyaType exprType
    else return newEnv
  where
    mismatchedTypeDeclaration zoyaType exprType = shows_ "mismatched type declaration of " . shows name . posPart pos . shows_ "declared: " . shows zoyaType . shows_ ", actual: " . shows exprType $ ""

defineType :: VarName -> ZoyaType -> BNFC'Position -> TypeCheck Env
defineType name zoyaType pos = do
  env <- ask
  let (zoyaType, env) = fillPolyType env zoyaType
  let tE = vars env
  case Map.lookup name tE of
    Just (pT, prevPos) -> if pT == zoyaType then ask else throwError $ redeclarationError prevPos
    Nothing -> return $ env {vars = Map.insert name (zoyaType, pos) tE}
  where
    redeclarationError prevPos = shows_ "redeclaration of " . shows name . posPart pos . shows_ ", has different type than " . posPart prevPos $ ""
    fillPolyType env (PolyType pit 0) =
      let pid = polyToId env
       in case Map.lookup pit pid of
            Nothing ->
              let tid = Map.size pid
                  env = env {polyToId = Map.insert pit tid pid}
               in (PolyType pit tid, env)
            Just n -> (PolyType pit n, env)
    fillPolyType env t = (t, env)

inferType :: Expr -> TypeCheck ZoyaType
inferType (ELambda pos argName t expr) = do
  argType <- parseType t
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
  env <- asks polyIdToType
  if typesEqual ifT elseT env
    then return ifT
    else throwError $ mismatchedIfTypesError ifT elseT
  where
    mismatchedIfTypesError ifT elseT = shows_ "mismatched if vars, in true branch: " . shows ifT . shows_ ", in false branch: " . shows ifT . posPart pos $ ""
inferType (EVar pos varName) = do
  env <- ask
  case Map.lookup varName $ vars env of
    Just (t, pos) -> return t
    Nothing -> throwError $ shows_ "use of undeclared variable " . shows varName . posPart pos $ ""
inferType (EType pos typeName) = do
  env <- ask
  case Map.lookup typeName $ typeVariants env of
    Just (name, t, vT, pos) -> return t
    Nothing -> throwError $ shows_ "use of undeclared type " . shows typeName . posPart pos $ ""
inferType (ETypeHelper pos typeName varNames) = do
  ts <- asks $ getTypes varNames . vars
  return $ CustomType typeName ts
  where
    getTypes :: [VarName] -> Map.Map VarName (ZoyaType, BNFC'Position) -> [ZoyaType]
    getTypes varNames varsMap = map (fst . (varsMap !)) varNames
inferType (EFApp pos fnExpr argExpr) = do
  outerEnv <- ask
  fn <- inferType fnExpr
  case fn of
    FunType argType retType -> checkType argType (inferType argExpr) pos >> return retType -- FIXME polyIdToType types
    _ -> throwError $ shows_ "tried to call not function" . posPart pos $ ""
inferType (ELitInt _ int) = return IntType
inferType (ELitList pos listArgs) = throwError shouldHaveBeenProccessedError
inferType (ELitListEmpty pos _) = throwError shouldHaveBeenProccessedError
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
  eq <- asks (typesEqual expected actual . polyIdToType)
  if eq then return expected else throwError $ typeMismatchError expected actual pos

typesEqual :: ZoyaType -> ZoyaType -> PolyMap -> Bool
typesEqual expected actual polyM = expected == actual -- FIXME polyIdToType types.

-- case expected of
--   IntType -> canBe IntType actual polyM
--   FunType argType retType -> case actual of
--     FunType actArgType actRetType -> False -- not implemented
--     _ -> False
--   BoolType -> canBe BoolType actual polyM
--   CustomType typeName zoyaTypes -> False -- not implemented
--   PolyType name -> polyOk name actual polyM
-- where
--   polyOk :: PolyIdentToken -> ZoyaType -> PolyMap -> Bool
--   polyOk name actual polyM = case Map.lookup name polyM of
--     Nothing -> True
--     Just (t, _) -> typesEqual t actual polyM
--   canBe :: ZoyaType -> ZoyaType -> PolyMap -> Bool
--   canBe ex act polyM = ex == act || exPoly ex act polyM || actPoly ex act polyM
--   exPoly :: ZoyaType -> ZoyaType -> PolyMap -> Bool
--   exPoly ex act polyM = case ex of
--     PolyType name -> polyOk name act polyM
--     _ -> False
--   actPoly :: ZoyaType -> ZoyaType -> PolyMap -> Bool
--   actPoly ex act polyM = False

typeMismatchError :: ZoyaType -> ZoyaType -> BNFC'Position -> String
typeMismatchError expected actual pos = shows_ "type mismatch, expected: " . shows expected . shows_ ", actual: " . shows actual . posPart pos $ ""

shouldHaveBeenProccessedError :: String
shouldHaveBeenProccessedError = "should have been preprocessed"

inferTypeMatch :: Match -> TypeCheck ZoyaType
inferTypeMatch (Match pos expr arms) = do
  val <- inferType expr
  inferTypeMatch' val arms Nothing
  where
    inferTypeMatch' :: ZoyaType -> [MatchArm] -> Maybe ZoyaType -> TypeCheck ZoyaType
    inferTypeMatch' val [] (Just t) = return t
    inferTypeMatch' val [] Nothing = throwError $ shows_ "empty match" . posPart pos $ ""
    inferTypeMatch' val ((MatchArm pos specifier expr) : t) prevType = do
      newEnv <- checkMatch specifier val
      exprType <- local (const newEnv) $ inferType expr
      case prevType of
        Nothing -> inferTypeMatch' val t (Just exprType)
        Just pT -> if typesEqual pT exprType $ polyIdToType newEnv then inferTypeMatch' val t (Just exprType) else throwError $ matchArmTypeMismatch pT exprType pos
    matchArmTypeMismatch :: ZoyaType -> ZoyaType -> BNFC'Position -> String
    matchArmTypeMismatch expected actual pos = shows_ "mismatched match arm type, expected: " . shows expected . shows_ ", actual: " . shows actual . posPart pos $ ""
    wrongNumberOfTypeArgs :: TypeName -> BNFC'Position -> String
    wrongNumberOfTypeArgs tn pos = shows_ "wrong number of arguemnts to type variant " . shows tn . posPart pos $ ""
    wrongVariantError :: TypeName -> TypeName -> BNFC'Position -> String
    wrongVariantError variantType typeVariant pos = shows typeVariant . shows_ " is not variant of " . shows variantType . posPart pos $ ""
    checkMatch :: MatchArmSpecifier -> ZoyaType -> TypeCheck Env
    checkMatch (MatchArmType pos typeName args) val = case val of
      CustomType tn _ -> do
        -- handle types
        env <- ask
        let (s, _, _) = (Map.! tn) . variantTypes $ env
        if Set.member typeName s
          then
            let vT = typeVariants env
             in case Map.lookup typeName vT of
                  Nothing -> throwError $ shows_ "unknown variant type " . shows typeName . posPart pos $ ""
                  Just (_, _, vT, _) ->
                    if length vT == length args
                      then checkMatchTypeArgs $ zip args vT
                      else throwError $ wrongNumberOfTypeArgs tn pos
          else throwError $ wrongVariantError tn typeName pos
      _ -> throwError $ shows_ "tried to match: " . shows val . posPart pos $ ""
    checkMatch (MatchArmVar pos varName) t = defineType varName t pos
    checkMatch (MatchArmFallback pos) val = ask
    checkMatch MatchArmList {} val = throwError shouldHaveBeenProccessedError
    checkMatch (MatchArmBrackets pos m) val = checkMatch m val
    checkMatchTypeArgs :: [(MatchArmVariantTypeArgument, ZoyaType)] -> TypeCheck Env
    checkMatchTypeArgs [] = ask
    checkMatchTypeArgs (h : t) = do
      env <- checkMatchTypeArg h
      local (const env) $ checkMatchTypeArgs t
    checkMatchTypeArg :: (MatchArmVariantTypeArgument, ZoyaType) -> TypeCheck Env
    checkMatchTypeArg (MatchArmVariantTypeArgumentNested _ specifier, t) = checkMatch specifier t
    checkMatchTypeArg (MatchArmVariantTypeArgumentFallback _, _) = ask
    checkMatchTypeArg (MatchArmVariantTypeArgumentIdent pos varName, t) = defineType varName t pos

typeCheck :: Program -> TypeCheck Env
typeCheck (Program _ topDefs) = typeCheckTopDefs topDefs

typeCheckProgram :: Program -> (Either String (), [String])
typeCheckProgram program = runTypeCheck newEnv (void $ typeCheck program)

newEnv :: Env
newEnv =
  Env
    { vars = Map.empty,
      polyIdToType = Map.empty,
      polyToId = Map.empty,
      variantTypes = Map.empty,
      typeVariants = Map.empty
    }
