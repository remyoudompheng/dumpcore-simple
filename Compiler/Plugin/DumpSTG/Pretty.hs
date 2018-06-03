{-# LANGUAGE OverloadedStrings #-}

-- | Our custom pretty-printer for STG syntax trees.
module Compiler.Plugin.DumpSTG.Pretty
  ( prettySTG ) where

import qualified Data.ByteString as B
import CoreSyn (AltCon (..))
import Data.List (intersperse)
import Data.Maybe (isJust)
import Literal (Literal(..))
import Name (NamedThing, getName)
import StgSyn
import Text.PrettyPrint
import Outputable (Outputable, ppr, pprHsBytes, showSDocUnsafe)
import PrimOp (primOpFixity)
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
--   v x y = (CLOSURE captures)? <expr>
-- * an updatable closure (lazy thunk value)
--   it cannot have arguments, as it must be evaluated in a deterministic way.
--   v = THUNK <expr>
-- * a single entry closure (procedure)
--   PROX v x y = <expr>
prBind :: V.Id -> StgRhs -> Doc
prBind v (StgRhsCon _ c args) = prNamed v <+> "=" <+> prNamed c <+> hsep (map prGeneric args)
prBind v (StgRhsClosure _ _ [] ReEntrant [] e) = prNamed v <+> "=" <+> prTopExpr e
prBind v (StgRhsClosure _ _ caps ReEntrant [] e) = (prNamed v <+> "=" <+> captures) $$ nest 4 (prTopExpr e)
  where
    captures = "CLOSURE {captures=" <> hsep (punctuate comma $ map prNamed caps) <> "}"
prBind v (StgRhsClosure ccs info caps ReEntrant (a:as) e) = prNamed v <+> prBind a rhs'
    where rhs' = StgRhsClosure ccs info caps ReEntrant as e
prBind v (StgRhsClosure ccs info caps SingleEntry args e) = prNamed v <+> "=" <+>
    "PROC" <+> prBind v (StgRhsClosure ccs info caps ReEntrant args e)
prBind v (StgRhsClosure _ _ caps Updatable [] e) = prNamed v <+> "=" <+>
    "THUNK" <+> captures $$ nest 4 (prTopExpr e)
  where
    captures | caps == [] = empty
             | otherwise  = "{captures=" <> hsep (punctuate comma $ map prNamed caps) <> "}"
prBind _ (StgRhsClosure _ _ _ Updatable _ _) = error "encountered updatable thunk with args"

-- | Expressions
-- To reduce indentation, we put all bindings at the same level if
-- possible.
prTopExpr :: StgExpr -> Doc
prTopExpr e = case prExpr e of
                  ([], e') -> e'
                  (binds, e') -> ("let" <+> vcat binds) $$ ("in" <+> e')

prExpr :: StgExpr -> ([Doc], Doc)
-- StgApp is ordinary application of a function.
-- A function without arguments is just itself.
prExpr (StgApp f []) = ([], prNamed f)
prExpr (StgApp f args) = ([], text "apply" <+> prNamed f <+> hsep (map prArg args))
prExpr (StgLit l) = ([], prLiteral l)
prExpr (StgConApp c as _) = ([], prNamed c <+> hsep (map prArg as))
prExpr (StgOpApp (StgPrimOp op) [x,y] _)
    -- known infix operators are shown infix
    | isJust (primOpFixity op) = ([], prArg x <+> prGeneric op <+> prArg y)
    | otherwise = ([], prGeneric op <+> prArg x <+> prArg y)
prExpr e@(StgOpApp _ _ _) = ([], prGeneric e) -- FIXME? delegate to GHC
-- StgLam is not supposed to be seen at this stage.
prExpr (StgLam _ _) = ([], "STGLAM??")
-- StgCase is evaluation of expression 'e', followed by
-- usage/deconstruction of v
prExpr (StgCase e v _ alts) = prCase e v alts
prExpr (StgLet bind e) = let (bs, e') = prExpr e in ((prBinding bind):bs, e')
prExpr (StgLetNoEscape bind e) = let (bs, e') = prExpr e in (("NOESCAPE" <+> prBinding bind):bs, e')
prExpr (StgTick _ e) = prExpr e -- don't render ticks

-- | Case
-- We render them as "v = eval e; case v of ..."
-- To reduce indentation, prCase returns the list of bindings, then the
-- final case.
prCase :: StgExpr -> V.Var -> [StgAlt] -> ([Doc], Doc)
prCase e v alts =
   let eval = prNamed v <+> "=" <+> "eval" <+> prTopExpr e
       prAlt (kind, binds, e') = prGeneric kind <+> hsep (map prNamed binds) <+> "->" <+> prTopExpr e'
       cases = nest 4 $ vcat $ map prAlt alts
   in case alts of
         [] -> ([], eval)
         [(DEFAULT, _, e')] -> let (bs, e'') = prExpr e' in (eval:bs, e'')
         _ -> ([eval], ("case" <+> prNamed v <+> "of") $$ cases)

prArg :: StgArg -> Doc
prArg (StgVarArg v) = prNamed v
prArg (StgLitArg l) = prLiteral l

prLiteral :: Literal -> Doc
prLiteral = prGeneric
