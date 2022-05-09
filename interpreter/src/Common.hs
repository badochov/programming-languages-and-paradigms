module Common where

import Grammar.Abs

listNodeTypeName :: TypeName
listNodeTypeName = TypeName "Node"

listEmptyTypeName :: TypeName
listEmptyTypeName = TypeName "Empty"

preproccesExpr :: Expr -> Expr
preproccesExpr (ELitList pos []) = ETApp pos listEmptyTypeName []
preproccesExpr (ELitList pos ((ListArg argPos h) : t)) = ETApp argPos listEmptyTypeName [h, preproccesExpr $ ELitList pos t]
preproccesExpr (EListEx pos headExpr tailExpr) = ETApp pos listNodeTypeName [headExpr, tailExpr]
preproccesExpr expr = expr

preprocess :: Program -> Program
preprocess (Program pos topDefs) = Program pos $ preprocessTopDefs topDefs

preprocessTopDefs :: [TopDef] -> [TopDef]
preprocessTopDefs = map preprocessTopDef

preprocessTopDef :: TopDef -> TopDef
preprocessTopDef (TopDefVar posTop (VarDef pos varName expr)) = TopDefVar posTop (VarDef pos varName $ preproccesExpr expr)
preprocessTopDef topDef = topDef