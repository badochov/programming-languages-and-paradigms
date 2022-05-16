module Common where

import Grammar.Abs

listNodeTypeName :: TypeName
listNodeTypeName = TypeName "Node"

listEmptyTypeName :: TypeName
listEmptyTypeName = TypeName "Empty"

preproccesExpr :: Expr -> Expr
preproccesExpr (ELitListEmpty pos _) = EType pos listEmptyTypeName
preproccesExpr (ELitList pos []) = EType pos listEmptyTypeName
preproccesExpr (ELitList pos ((ListArg argPos h) : t)) = preproccesExpr $ makeTApp argPos listNodeTypeName [h, ELitList pos t]
preproccesExpr (EListEx pos headExpr tailExpr) = preproccesExpr $ makeTApp pos listNodeTypeName [headExpr, tailExpr]
preproccesExpr (EMatch pos matchExpr) = EMatch pos (preprocessMatchExpr matchExpr)
preproccesExpr (EBrackets pos expr) = EBrackets pos (preproccesExpr expr)
preproccesExpr (EFApp pos expr1 expr2) = EFApp pos (preproccesExpr expr1) (preproccesExpr expr2)
preproccesExpr (ELambda pos varName t expr) = ELambda pos varName (preprocessType t) (preproccesExpr expr)
preproccesExpr (ELetIn pos varDef expr) = ELetIn pos (preprocessVarDef varDef) (preproccesExpr expr)
preproccesExpr (ECond pos stmt ifExpr elseExpr) = ECond pos (preproccesExpr stmt) (preproccesExpr ifExpr) (preproccesExpr elseExpr)
preproccesExpr (ENeg pos expr) = ENeg pos (preproccesExpr expr)
preproccesExpr (ENot pos expr) = ENot pos (preproccesExpr expr)
preproccesExpr (EMul pos expr1 op expr2) = EMul pos (preproccesExpr expr1) op (preproccesExpr expr2)
preproccesExpr (EAdd pos expr1 op expr2) = EAdd pos (preproccesExpr expr1) op (preproccesExpr expr2)
preproccesExpr (ERel pos expr1 op expr2) = ERel pos (preproccesExpr expr1) op (preproccesExpr expr2)
preproccesExpr (EAnd pos expr1 expr2) = EAnd pos (preproccesExpr expr1) (preproccesExpr expr2)
preproccesExpr (EOr pos expr1 expr2) = EOr pos (preproccesExpr expr1) (preproccesExpr expr2)
preproccesExpr expr = expr

makeTApp :: BNFC'Position -> TypeName -> [Expr] -> Expr
makeTApp pos tName = makeFApp' (EType pos tName) pos
  where
    makeFApp' applyTo pos [] = applyTo
    makeFApp' applyTo pos (h : t) = EBrackets pos (makeFApp' (EFApp pos applyTo h) pos t)

preprocessMatchExpr :: Match -> Match
preprocessMatchExpr (Match pos expr arms) = Match pos (preproccesExpr expr) (map preprocessArm arms)

preprocessArm :: MatchArm -> MatchArm
preprocessArm (MatchArm pos spec expr) = MatchArm pos (preproccesArmSpec spec) (preproccesExpr expr)

preproccesArmSpec :: MatchArmSpecifier -> MatchArmSpecifier
preproccesArmSpec (MatchArmList pos listSpec) = preprocessListSpec listSpec
preproccesArmSpec (MatchArmType pos typename args) = MatchArmType pos typename (map preprocessVariantTypeArg args)
preproccesArmSpec (MatchArmBrackets pos m) = MatchArmBrackets pos (preproccesArmSpec m)
preproccesArmSpec armSpec = armSpec

preprocessVariantTypeArg :: MatchArmVariantTypeArgument -> MatchArmVariantTypeArgument
preprocessVariantTypeArg (MatchArmVariantTypeArgumentNested pos armSpec) = MatchArmVariantTypeArgumentNested pos (preproccesArmSpec armSpec)
preprocessVariantTypeArg vTypeArg = vTypeArg

preprocessListSpec :: MatchArmSpecifierList -> MatchArmSpecifier
preprocessListSpec (MatchArmListEmpty pos _) = MatchArmType pos listEmptyTypeName []
preprocessListSpec (MatchArmListHeadTail pos head tail) = MatchArmType pos listNodeTypeName (map toVariantTypeArg [head, tail])
  where
    toVariantTypeArg armSpec = MatchArmVariantTypeArgumentNested pos (preproccesArmSpec armSpec)

preprocessTopDefs :: [TopDef] -> [TopDef]
preprocessTopDefs = map preprocessTopDef

preprocessVarDef :: VarDef -> VarDef
preprocessVarDef (VarDef pos varName t expr) = VarDef pos varName (preprocessType t) (preproccesExpr expr)

preprocessTopDef :: TopDef -> TopDef
preprocessTopDef (TopDefVar posTop vDef@VarDef {}) = TopDefVar posTop (preprocessVarDef vDef)
preprocessTopDef topDef = topDef

preprocessType :: Type -> Type
preprocessType = id

preprocess :: Program -> Program
preprocess (Program pos topDefs) = Program pos $ preprocessTopDefs topDefs

(<.>) :: Maybe (a -> a) -> Maybe (a -> a) -> Maybe (a -> a)
(<.>) fn1 fn2 = (.) <$> fn1 <*> fn2

shows_ :: String -> ShowS
shows_ = (++)

showsS_ :: [String] -> ShowS
showsS_ s = shows_ "[" . args s . shows_ "]"
  where
    args [] = shows_ ""
    args [h] = shows_ h
    args (h : t) = shows_ h . shows_ ", " . args t

posPart :: BNFC'Position -> ShowS
posPart Nothing = shows_ " in prelude"
posPart (Just (line, col)) = shows_ " at line " . shows line . shows_ " column " . shows col

mergePrograms :: Program -> Program -> Program
mergePrograms (Program _ topDefs) (Program pos topDefs2) = Program pos $ topDefs ++ topDefs2

consecutive :: Int -> [Int]
consecutive n =
  consecutive' [] n
  where
    consecutive' acc 0 = acc
    consecutive' acc n = consecutive' (n : acc) (n -1)