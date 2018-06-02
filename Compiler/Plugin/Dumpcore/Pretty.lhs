| This module implements a simplified, approximate syntax for GHC Core
The syntax tree is rendered using the pretty package.

> {-# LANGUAGE OverloadedStrings #-}
> module Compiler.Plugin.Dumpcore.Pretty (prettySimple) where
>
> import CoreSyn
> import Text.PrettyPrint
> import Name (NamedThing, getName)
> import qualified Type as T
> import qualified Var as V
> import Outputable (Outputable, ppr, showSDocUnsafe)

> prettySimple :: String -> CoreProgram -> String
> prettySimple modname prog = render (prProg modname prog)

A program is a sequence of bindings.
A binding is rendered as:

    ident :: T
    ident = expr

> prProg :: String -> CoreProgram -> Doc
> prProg modname binds = foldl s ("module" <+> text modname) (map prBind binds)
>   where s x y = x $+$ "" $+$ y

Bindings are either standalone or come in cycles of mutually
recursive definitions.

> prBind :: CoreBind -> Doc
> prBind (NonRec b e) = typ $+$ _prBind b e
>     where typ = prVar b <+> "::" <+> prType (V.varType b)
> prBind (Rec l) = foldl ($+$) empty (map p l)
>     where p (b, e) = prBind (NonRec b e)

Lambdas are a special case where we render:

   f = \x -> \y -> z

as

   f x y = z

> _prBind :: V.Var -> CoreExpr -> Doc
> _prBind v (Lam w e) = prVar v <+> _prBind w e
> _prBind v e = prVar v <+> "=" <+> prExpr e

Identifiers are just rendered as "themselves".

> prNamed :: (NamedThing a) => a -> Doc
> prNamed n = prGeneric $ getName n

> prGeneric :: (Outputable a) => a -> Doc
> prGeneric x = text (showSDocUnsafe $ ppr $ x)

> prVar :: V.Var -> Doc
> prVar = prNamed

The helper 'indent' will help avoiding excessive indentation.

> infixl 1 `indent`
> indent :: Doc -> Doc -> Doc
> indent x y = hang x 4 y

> prType :: T.Type -> Doc
> prType t = text (showSDocUnsafe $ T.pprType t)

Expression formatting:
- nested apply are rendered as a simple spineless apply
- nested lambdas are rendered as a single spineless lambda

> prExpr :: CoreExpr -> Doc
> prExpr (Var v) = prVar v
> prExpr (Lit l) = text (showSDocUnsafe $ ppr l)
> prExpr (App f@(App _ _) x) = prApp f <+> prExpr x <> ")"
>    where prApp (App g y) = prApp g <+> prExpr y
>          prApp g = "(" <> prExpr g
> prExpr (App f x) = "(" <> prExpr f <+> prExpr x <> ")"
> prExpr (Lam v e@(Lam _ _)) = "\\" <> prVar v <+> prExpr e
> prExpr (Lam v e) = "\\" <> prVar v <+> "->" <+> prExpr e
> prExpr (Let b e) = "let" <+> prBind b <+> prExpr e
> prExpr (Case e v t cases) = prCase e v t cases
> prExpr (Cast e _) = "cast" <+> prExpr e

We don't render ticks, types or coercions.

> prExpr (Tick _ _) = empty
> prExpr (Type _) = empty
> prExpr (Coercion _) = empty

> prCase :: CoreExpr -> V.Var -> T.Type -> [Alt CoreBndr] -> Doc
> prCase expr _ _ [] = "case" <+> prExpr expr
> prCase expr _ _ [(DEFAULT, [], e)] = ("case" <+> prExpr expr) $$ prExpr e
> prCase expr _ _ l = ("case" <+> prExpr expr <+> "of") $$ vcat (map prAlt l)
>    where prAlt (DataAlt c, vs, e) = prNamed c <+> hsep (map prVar vs)
>                                    <+> "->" `indent` prExpr e
>          prAlt (LitAlt x, _, e) = prGeneric x <+> "->" <+> prExpr e
>          prAlt (DEFAULT, _, e) = "_ ->" <+> prExpr e
