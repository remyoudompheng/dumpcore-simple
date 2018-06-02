{-# LANGUAGE OverloadedStrings #-}

-- | Our custom pretty-printer for STG syntax trees.
module Compiler.Plugin.DumpSTG.Pretty
  ( prettySTG ) where

import qualified Data.ByteString as B
import Data.List (intersperse)
import Name (NamedThing, getName)
import StgSyn
import Text.PrettyPrint
import Outputable (Outputable, ppr, pprHsBytes, showSDocUnsafe)
import qualified Var as V

prettySTG :: String -> [StgTopBinding] -> String
prettySTG modname binds = render $ vcat (title:intersperse "" (map prTopBind binds))
    where title = text ("module " ++ modname)

prTopBind :: StgTopBinding -> Doc
prTopBind (StgTopLifted (StgNonRec v rhs)) = _prTopBind v rhs
prTopBind (StgTopLifted (StgRec l)) = vcat $ intersperse "" $ map pr l
  where pr (v, rhs) = _prTopBind v rhs
prTopBind (StgTopStringLit b s) = prGeneric b <+> "=" <+> prBytes s

-- | Prints a top-level declaration (name with type and binding).
_prTopBind :: V.Id -> StgRhs -> Doc
_prTopBind v rhs = title $+$ defin
  where title = prGeneric v <+> "::" <+> prGeneric (V.varType v)
        defin = prGeneric v <+> "=" <+> prRhs rhs

-- | Prints generically like GHC does.
prGeneric :: (Outputable a) => a -> Doc
prGeneric x = text (showSDocUnsafe $ ppr $ x)

prNamed :: (NamedThing a) => a -> Doc
prNamed n = prGeneric $ getName n

-- | Prints an array of bytes
prBytes :: B.ByteString -> Doc
prBytes s = text (showSDocUnsafe $ pprHsBytes s)

-- | Expressions
prRhs :: StgRhs -> Doc
prRhs (StgRhsClosure _ _ _ _ fs e) = prExpr (StgLam fs e)
prRhs (StgRhsCon _ c args) = prNamed c <+> hsep (map prGeneric args)

prExpr :: StgExpr -> Doc
prExpr (StgApp _ _) = text "apply"
prExpr (StgLit l) = prGeneric l
prExpr (StgConApp c as _) = prNamed c <+> hsep (map prGeneric as)
prExpr (StgOpApp _ _ _) = text "op apply"
prExpr (StgLam _ _) = text "lambda"
prExpr (StgCase _ _ _ _) = text "case"
prExpr (StgLet _ _) = text "let"
prExpr (StgLetNoEscape _ _) = text "NOESCAPE let"
prExpr (StgTick _ _) = empty -- don't render ticks

