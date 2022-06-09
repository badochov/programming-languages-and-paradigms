{-# LANGUAGE TupleSections #-}

module TypeChecker where

import Common (consecutive, posPart, shows_, (<.>))
import Control.Exception (throw)
import Control.Monad (foldM, void)
import Control.Monad.Except (ExceptT, MonadTrans (lift), runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask, asks, when)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.Function (on)
import Data.List (sortBy)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set (Set, empty, insert, member)
import Grammar.Abs

type PolyId = Int

type CustomT = (Set.Set TypeName, [PolyId], BNFC'Position)

type TypeVariant = (TypeName, ZoyaType, [ZoyaType], BNFC'Position)

data Env = Env
  { vars :: Map.Map VarName (ZoyaType, BNFC'Position),
    customTypes :: Map.Map TypeName CustomT,
    typeVariants :: Map.Map TypeName TypeVariant,
    polyToId :: Map.Map PolyIdentToken PolyId,
    counter :: Int
  }
  deriving (Show)

type TypeCheck a = ReaderT Env (ExceptT String (WriterT [String] Identity)) a

data ZoyaType
  = IntType
  | FunType ZoyaType ZoyaType
  | BoolType
  | CustomType TypeName [ZoyaType]
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
  prevDef <- getCustomType name
  case prevDef of
    Nothing -> do
      envChanger <- getEnvChanger
      prevPolyToId <- asks polyToId
      env <- local envChanger (handleVariantTypes variants)
      return $ env {polyToId = prevPolyToId}
    Just (t, tids, pos) -> throwError $ variantTypeRedefinitionError pos
  where
    getEnvChanger = do
      (env, pids) <- updatePolies polies pos
      let newVariantTypes = Map.insert name (Set.empty, pids, pos) $ customTypes env
      return $ const env {customTypes = newVariantTypes}
    handleVariantTypes (h : t) = do
      env <- typeCheckVariantType h name
      local (const env) $ handleVariantTypes t
    handleVariantTypes [] = ask
    variantTypeRedefinitionError orgPos = shows_ "redefinition of variant type " . shows name . posPart pos . shows_ ", previosly defined" . posPart orgPos $ ""

updatePolies :: [PolyIdentToken] -> BNFC'Position -> TypeCheck (Env, [PolyId])
updatePolies toks pos = updatePolies' toks []
  where
    updatePolies' :: [PolyIdentToken] -> [PolyId] -> TypeCheck (Env, [PolyId])
    updatePolies' [] ids = asks (,reverse ids)
    updatePolies' (h : t) ids =
      do
        env <- ask
        let pid = polyToId env
        let tid = counter env
        if Map.member h pid
          then throwError $ shows h . shows "already defined" . posPart pos $ ""
          else
            let newEnv = env {polyToId = Map.insert h tid pid, counter = tid + 1}
             in local (const newEnv) $ updatePolies' t (tid : ids)

getCustomType :: TypeName -> TypeCheck (Maybe CustomT)
getCustomType t = asks (Map.lookup t . customTypes)

getTypeVariant :: TypeName -> TypeCheck (Maybe TypeVariant)
getTypeVariant t = asks (Map.lookup t . typeVariants)

typeCheckVariantType :: VariantType -> TypeName -> TypeCheck Env
typeCheckVariantType (VariantType pos variantName vals) typeName = do
  prevDef <- getTypeVariant variantName
  case prevDef of
    Nothing -> do
      (vT, e) <- parseTypes vals []
      () <- checkVT vT
      t <- local (const e) (createZoyaTypeConstructor typeName vT)
      return $e {typeVariants = Map.insert variantName (typeName, t, vT, pos) $ typeVariants e, customTypes = addVariant e}
    Just (_, _, _, pos) -> throwError $ typeVariantRedefinitionError pos
  where
    typeVariantRedefinitionError orgPos = shows_ "redefinition of type variant " . shows variantName . posPart pos . shows_ ", previosly defined" . posPart orgPos $ ""
    addVariant env = Map.adjust (\(vs, tids, pos) -> (Set.insert variantName vs, tids, pos)) typeName $ customTypes env
    checkVT :: [ZoyaType] -> TypeCheck ()
    checkVT [] = return ()
    checkVT ((PolyType name 0) : t) = throwError $ shows "type " . shows name . shows " hasn't been defined" . posPart pos $ ""
    checkVT (h : t) = checkVT t

createZoyaTypeConstructor :: TypeName -> [ZoyaType] -> TypeCheck ZoyaType
createZoyaTypeConstructor typeName ts = makeLambda ts <$> makeConstructor
  where
    makeLambda [] cst = cst
    makeLambda (h : t) cst = FunType h $ makeLambda t cst
    makeConstructor :: TypeCheck ZoyaType
    makeConstructor = do
      polies <- asks polyToId
      let p = Map.assocs polies
      let sP = sortBy (compare `on` snd) p
      let lst = map (uncurry PolyType) sP
      return $ CustomType typeName lst

parseType :: Type -> TypeCheck (ZoyaType, Env)
parseType (TypeInt pos) = asks (IntType,)
parseType (TypeBool pos) = asks (BoolType,)
parseType (TypeFn pos arg res) = do
  (a, aEnv) <- parseType arg
  (r, rEnv) <- local (const aEnv) (parseType res)
  return (FunType a r, rEnv)
parseType (TypeCustom pos name ts) = do
  vT <- asks customTypes
  if Map.member name vT
    then do
      (parsedTs, e) <- parseTypes ts []
      return (CustomType name parsedTs, e)
    else throwError $ shows_ "unknown type " . shows name . posPart pos $ ""
parseType (TypeBrackets pos t) = parseType t
parseType (TypePoly pos t) = do
  env <- ask
  case Map.lookup t $ polyToId env of
    Nothing ->
      let tid = counter env
       in return
            ( PolyType t tid,
              env
                { counter = tid + 1,
                  polyToId = Map.insert t tid $ polyToId env
                }
            )
    Just n -> return (PolyType t n, env)

parseTypes :: [Type] -> [ZoyaType] -> TypeCheck ([ZoyaType], Env)
parseTypes [] acc = asks (reverse acc,)
parseTypes (h : t) acc = do
  (zt, e) <- parseType h
  local (const e) (parseTypes t (zt : acc))

typeCheckVarDef :: VarDef -> TypeCheck Env
typeCheckVarDef (VarDef pos name t expr) = do
  (zoyaType, nEnv) <- parseType t
  prevPolyToId <- asks polyToId
  newEnv <- local (const nEnv) (defineType name zoyaType pos)
  exprType <-local (const newEnv) $ inferType expr
  if typesEqual prevPolyToId zoyaType exprType
    then return newEnv {polyToId = prevPolyToId}
    else throwError $ mismatchedTypeDeclaration zoyaType exprType
  where
    mismatchedTypeDeclaration zoyaType exprType = shows_ "mismatched type declaration of " . shows name . posPart pos . shows_ " declared: " . shows zoyaType . shows_ ", actual: " . shows exprType $ ""

defineType :: VarName -> ZoyaType -> BNFC'Position -> TypeCheck Env
defineType name zoyaType pos = do
  env <- ask
  let tE = vars env
  case Map.lookup name tE of
    Just (pT, prevPos) -> if pT == zoyaType then return env else throwError $ redeclarationError prevPos
    Nothing -> return $ env {vars = Map.insert name (zoyaType, pos) tE}
  where
    redeclarationError prevPos = shows_ "redeclaration of " . shows name . posPart pos . shows_ ", has different type than " . posPart prevPos $ ""

inferType :: Expr -> TypeCheck ZoyaType
inferType (ELambda pos argName t expr) = do
  (argType, nEnv) <- parseType t
  env <- local (const nEnv) (defineType argName argType pos)
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
  pid <- asks polyToId
  if typesEqual pid ifT elseT
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
    FunType argType retType -> do
      let actM = inferType argExpr
      act <- actM
      pid <- asks polyToId
      if canBeOfType pid argType act
        then return $ updateRetType retType act argType
        else throwError $ wrongApplication argType act
    _ -> throwError $ shows_ "tried to call not function" . posPart pos $ ""
  where
    wrongApplication ex got = shows_ "tried to call function with wrong argument, expected: " . shows ex . shows_ ", got: " . shows got . posPart pos $ ""
inferType (ELitInt _ int) = return IntType
inferType (ELitList pos listArgs) = throwError shouldHaveBeenProccessedError
inferType (ELitListEmpty pos _) = throwError shouldHaveBeenProccessedError
inferType (EBrackets pos expr) = inferType expr
inferType (ENeg pos expr) = checkType IntType (inferType expr) pos
inferType (ENot pos expr) = checkType BoolType (inferType expr) pos
inferType (EListEx pos headExpr tailExpr) = throwError shouldHaveBeenProccessedError
inferType (EMul pos lExpr mulOp rExpr) = checkType IntType (inferType lExpr) pos >> checkType IntType (inferType rExpr) pos >> return IntType
inferType (EAdd pos lExpr addOp rExpr) = checkType IntType (inferType lExpr) pos >> checkType IntType (inferType rExpr) pos >> return IntType
inferType (ERel pos lExpr relOp rExpr) = checkType IntType (inferType lExpr) pos >> checkType IntType (inferType rExpr) pos >> return BoolType
inferType (EAnd pos lExpr rExpr) = checkType BoolType (inferType lExpr) pos >> checkType BoolType (inferType rExpr) pos
inferType (EOr pos lExpr rExpr) = checkType BoolType (inferType lExpr) pos >> checkType BoolType (inferType rExpr) pos

updateRetType :: ZoyaType -> ZoyaType -> ZoyaType -> ZoyaType
updateRetType r (CustomType _ exTypes) (CustomType _ types) = updateRetTypeList r exTypes types
updateRetType r e (PolyType _ pid) = updatePid pid e r
updateRetType r (FunType exArg exRetType) (FunType arg retType) = updateRetTypeList r [exArg, exRetType] [arg, retType]
updateRetType r _ _ = r

updateRetTypeList :: ZoyaType -> [ZoyaType] -> [ZoyaType] -> ZoyaType
updateRetTypeList r exL l = foldl (\r (ex, a) -> updateRetType r ex a) r $ zip exL l

updatePid :: PolyId -> ZoyaType -> ZoyaType -> ZoyaType
updatePid pid t (CustomType name types) = CustomType name (map (updatePid pid t) types)
updatePid pid t pt@(PolyType _ pidC) = if pid == pidC then t else pt
updatePid pid t (FunType arg retType) = FunType (updatePid pid t arg) (updatePid pid t retType)
updatePid _ _ ret = ret

checkType :: ZoyaType -> TypeCheck ZoyaType -> BNFC'Position -> TypeCheck ZoyaType
checkType expected actualM pos = do
  actual <- actualM
  () <- checkType' expected actual pos
  return actual

checkType' :: ZoyaType -> ZoyaType -> BNFC'Position -> TypeCheck ()
checkType' expected actual pos = do
  pid <- asks polyToId
  if typesEqual pid expected actual
    then return ()
    else throwError $ typeMismatchError expected actual pos

isBound :: Map.Map PolyIdentToken PolyId -> ZoyaType -> Bool
isBound pid (PolyType name tid) =
  case Map.lookup name pid of
    Nothing -> False
    Just n -> n == tid
isBound _ _ = error "unexpected type"

typesEqual :: Map.Map PolyIdentToken PolyId -> ZoyaType -> ZoyaType -> Bool
typesEqual pid (CustomType exName exTypes) (CustomType name types) = name == exName && typeListEqual pid exTypes types
typesEqual pid (FunType exArg exRetType) (FunType arg retType) = typeListEqual pid [exArg, exRetType] [arg, retType]
typesEqual pid (PolyType _ exId) act@(PolyType _ tid) = tid == exId || not (isBound pid act)
typesEqual pid ex@(PolyType _ _) _ = not (isBound pid ex)
typesEqual pid expected actual = expected == actual

typeListEqual :: Map.Map PolyIdentToken PolyId -> [ZoyaType] -> [ZoyaType] -> Bool
typeListEqual pid [] [] = True
typeListEqual pid (IntType : exT) (IntType : t) = typeListEqual pid exT t
typeListEqual pid (BoolType : exT) (BoolType : t) = typeListEqual pid exT t
typeListEqual pid (ex : exT) (act@(PolyType _ _) : t) =
  if ex == act
    then typeListEqual pid exT t
    else unify
  where
    unify = not (isBound pid act) && applyUnification
    applyUnification =
      let newT = map (\h -> updateRetType h ex act) t
       in typeListEqual pid exT newT
typeListEqual pid (FunType exArg exRet : exT) (FunType arg ret : t) = typeListEqual pid ([exArg, exRet] ++ exT) ([arg, ret] ++ t)
typeListEqual pid (CustomType exName exTypes : exT) (CustomType name types : t) = exName == name && typeListEqual pid (exTypes ++ exT) (types ++ t)
typeListEqual _ _ _ = False

canBeOfType :: Map.Map PolyIdentToken PolyId -> ZoyaType -> ZoyaType -> Bool
canBeOfType pid (CustomType exName exTypes) (CustomType name types) = name == exName && canBeOfTypeList pid exTypes types
canBeOfType pid (FunType exArg exRetType) (FunType arg retType) = canBeOfTypeList pid [exArg, exRetType] [arg, retType]
canBeOfType pid ex@(PolyType _ exId) act@(PolyType _ tid) = tid == exId || not (isBound pid ex) || not (isBound pid act)
canBeOfType pid ex@(PolyType _ _) _ = not (isBound pid ex)
canBeOfType pid expected actual = expected == actual

canBeOfTypeList :: Map.Map PolyIdentToken PolyId -> [ZoyaType] -> [ZoyaType] -> Bool
canBeOfTypeList pid [] [] = True
canBeOfTypeList pid (IntType : exT) (IntType : t) = canBeOfTypeList pid exT t
canBeOfTypeList pid (BoolType : exT) (BoolType : t) = canBeOfTypeList pid exT t
canBeOfTypeList pid (ex@(PolyType _ exPid) : exT) (act@(PolyType _ aPid) : t) =
  if exPid == aPid
    then canBeOfTypeList pid exT t
    else unify
  where
    unify = (not (isBound pid ex) && applyUnificationEx) || (not (isBound pid act) && applyUnificationAct)
    applyUnificationAct =
      let newT = map (\h -> updateRetType h ex act) t
       in canBeOfTypeList pid exT newT
    applyUnificationEx =
      let newExT = map (\h -> updateRetType h act ex) exT
       in canBeOfTypeList pid newExT t
canBeOfTypeList pid (ex@(PolyType _ _) : exT) (act : t) = unify
  where
    unify = not (isBound pid ex) && applyUnification
    applyUnification =
      let newExT = map (\h -> updateRetType h act ex) exT
       in canBeOfTypeList pid newExT t
canBeOfTypeList pid (ex : exT) (act@(PolyType _ aPid) : t) =
  unify
  where
    unify = not (isBound pid act) && applyUnificationAct
    applyUnificationAct =
      let newT = map (\h -> updateRetType h ex act) t
       in canBeOfTypeList pid exT newT
canBeOfTypeList pid (FunType exArg exRet : exT) (FunType arg ret : t) = canBeOfTypeList pid ([exArg, exRet] ++ exT) ([arg, ret] ++ t)
canBeOfTypeList pid (CustomType exName exTypes : exT) (CustomType name types : t) = exName == name && canBeOfTypeList pid (exTypes ++ exT) (types ++ t)
canBeOfTypeList _ _ _ = False

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
        Just pT ->
          if typesEqual (polyToId newEnv) pT exprType
            then inferTypeMatch' val t (Just exprType)
            else throwError $ matchArmTypeMismatch pT exprType pos
    matchArmTypeMismatch :: ZoyaType -> ZoyaType -> BNFC'Position -> String
    matchArmTypeMismatch expected actual pos = shows_ "mismatched match arm type, expected: " . shows expected . shows_ ", actual: " . shows actual . posPart pos $ ""
    wrongNumberOfTypeArgs :: TypeName -> BNFC'Position -> String
    wrongNumberOfTypeArgs tn pos = shows_ "wrong number of arguemnts to type variant " . shows tn . posPart pos $ ""
    wrongVariantError :: TypeName -> TypeName -> BNFC'Position -> String
    wrongVariantError variantType typeVariant pos = shows typeVariant . shows_ " is not variant of " . shows variantType . posPart pos $ ""
    checkMatch :: MatchArmSpecifier -> ZoyaType -> TypeCheck Env
    checkMatch (MatchArmType pos typeName args) (CustomType tn ts) = do
      env <- ask
      let (s, ids, _) = (Map.! tn) . customTypes $ env
      if Set.member typeName s
        then
          let tv = typeVariants env
           in case Map.lookup typeName tv of
                Nothing -> throwError $ shows_ "unknown variant type " . shows typeName . posPart pos $ ""
                Just (_, _, vT, _) ->
                  if length vT == length args
                    then
                      let unifiedArgs = unifyTypeArgs ids ts vT
                       in checkMatchTypeArgs $ zip args unifiedArgs
                    else throwError $ wrongNumberOfTypeArgs tn pos
        else throwError $ wrongVariantError tn typeName pos
      where
        unifyTypeArgs :: [PolyId] -> [ZoyaType] -> [ZoyaType] -> [ZoyaType]
        unifyTypeArgs pids acts =  map mapR
          where
            zipped = zip pids acts
            foldPid r (pid, act) = updatePid pid act r
            mapR r = foldl foldPid r zipped
    checkMatch (MatchArmType pos typeName args) val = throwError $ shows_ "tried to match: " . shows val . posPart pos $ ""
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
      polyToId = Map.empty,
      customTypes = Map.empty,
      typeVariants = Map.empty,
      counter = 1
    }