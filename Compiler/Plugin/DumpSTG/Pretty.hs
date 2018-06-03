{-# LANGUAGE OverloadedStrings #-}

-- | Our custom pretty-printer for STG syntax trees.
module Compiler.Plugin.DumpSTG.Pretty
  ( prettySTG ) where

import qualified Data.ByteString as B
import Data.List (intersperse)
import Literal (Literal(..))
import Name (NamedThing, getName)
import StgSyn
import Text.PrettyPrint
import Outputable (Outputable, ppr, pprHsBytes, showSDocUnsafe)
import qualified Var as V

prettySTG :: String -> [StgTopBinding] -> String
prettySTG modname binds = render $ vcat (title:intersperse "" (map prTopBind binds))
    where title = text ("module " ++ modname)

prTopBind :: StgTopBinding -> Doc
prTopBind (StgTopLifted b) = prBinding b
prTopBind (StgTopStringLit b s) = prGeneric b <+> "=" <+> prBytes s

prBinding :: StgBinding -> Doc
prBinding (StgNonRec v rhs) = _prTopBind v rhs
prBinding (StgRec l) = vcat $ intersperse "" $ map pr l
  where pr (v, rhs) = _prTopBind v rhs

-- | Prints a top-level declaration (name with signature and binding).
_prTopBind :: V.Id -> StgRhs -> Doc
_prTopBind v rhs = _prSig v $+$ prBind v rhs

_prSig :: V.Id -> Doc
_prSig v = prGeneric v <+> "::" <+> prGeneric (V.varType v)

-- | Prints generically like GHC does.
prGeneric :: (Outputable a) => a -> Doc
prGeneric x = text (showSDocUnsafe $ ppr $ x)

prNamed :: (NamedThing a) => a -> Doc
prNamed n = prGeneric $ getName n

-- | Prints an array of bytes
prBytes :: B.ByteString -> Doc
prBytes s = text (showSDocUnsafe $ pprHsBytes s)

-- | Bindings
-- A RHS value is either
-- * a constructor (StgRhsCon)
--   v = C a1 a2
-- * a reentrant closure (ordinary function or closure)
--   v x y = <expr>
-- * an updatable closure (lazy thunk value)
--   it cannot have arguments, as it must be evaluated in a deterministic way.
--   v = THUNK <expr>
-- * a single entry closure (procedure)
--   PROX v x y = <expr>
prBind :: V.Id -> StgRhs -> Doc
prBind v (StgRhsCon _ c args) = prNamed v <+> "=" <+> prNamed c <+> hsep (map prGeneric args)
prBind v rhs@(StgRhsClosure _ _ caps ReEntrant [] e) = prNamed v <+> "=" <+> prExpr e
prBind v rhs@(StgRhsClosure ccs info caps ReEntrant (a:as) e) = prNamed v <+> prBind a rhs'
    where rhs' = StgRhsClosure ccs info caps ReEntrant as e
prBind v rhs@(StgRhsClosure ccs info caps SingleEntry args e) = prNamed v <+> "=" <+>
    "PROC" <+> prBind v (StgRhsClosure ccs info caps ReEntrant args e)
prBind v rhs@(StgRhsClosure ccs info caps Updatable [] e) = prNamed v <+> "=" <+>
    "THUNK" <+> captures caps <+> prExpr e
  where
    captures [] = empty
    captures caps =  "{captures=" <> hsep ( punctuate comma $ map prNamed caps ) <> "}"
prBind v rhs@(StgRhsClosure ccs info caps Updatable _ e) = error "encountered updatable thunk with args"

-- | Expressions
prExpr :: StgExpr -> Doc
-- StgApp is ordinary application of a function.
-- A function without arguments is just itself.
prExpr (StgApp f []) = prNamed f
prExpr (StgApp f args) = text "apply" <+> prNamed f <+> hsep (map prArg args)
prExpr (StgLit l) = "LITERAL" -- prLiteral l
prExpr (StgConApp c as _) = "CON" -- prNamed c <+> hsep (map prArg as)
prExpr (StgOpApp _ _ _) = text "op apply"
-- StgLam is not supposed to be seen at this stage.
prExpr (StgLam [] e) = "STGLAM??" <+> prExpr e -- FIXME ?
prExpr (StgLam vs e) = "STGLAM??" <+> "\\" <> hsep (map prNamed vs) <+> "->" <+> prExpr e -- FIXME ?
-- StgCase is evaluation of expression 'e', followed by
-- usage/deconstruction of v
prExpr (StgCase e v _ _) = text "let" <+> (eval $$ cas)
   where eval = prNamed v <+> "=" <+> "eval" <+> prExpr e
         cas  = "case" <+> prNamed v <+> "of ..." -- TODO
prExpr (StgLet bind e) = (text "let" <+> prBinding bind) $$ (text "in" <+> prExpr e)
prExpr (StgLetNoEscape _ _) = text "NOESCAPE let"
prExpr (StgTick _ _) = empty -- don't render ticks

prArg :: StgArg -> Doc
prArg (StgVarArg v) = prNamed v
prArg (StgLitArg l) = prLiteral l

prLiteral :: Literal -> Doc
prLiteral l = "LITERAL" -- prGeneric -- BROKEN
