> module Happy.Indentation (
>       IndentRel(..),
>       LookaheadRel(..),
>       composeIndentRel,
>       composeLookaheadRel,
>       ) where

I will split these into different files later on

There are more relations, but these will do for now

An IndentRel is attached to every symbol on the RHS of a grammar rule.

> data IndentRel
>       = Eq
>       | Geq
>       | Gt Int
>       | Splash
>       deriving (Eq)

> instance Show IndentRel where
>   show Eq = "="
>   show Geq = ">="
>   show (Gt n) = concat (replicate n ">")
>   show Splash = "*"

> composeIndentRel :: IndentRel -> IndentRel -> IndentRel
> composeIndentRel Splash _ = Splash
> composeIndentRel _ Splash = Splash
> composeIndentRel Eq r = r
> composeIndentRel r Eq = r
> composeIndentRel Geq r = r
> composeIndentRel r Geq = r
> composeIndentRel (Gt n) (Gt m) = Gt (n + m)

A LookaheadRel consists of two relations.

> data LookaheadRel = LookaheadRel IndentRel IndentRel 
>                   deriving (Eq)

> instance Show LookaheadRel where
>   show (LookaheadRel parentRel childRel) = "<" ++ show parentRel ++ " " ++ show childRel ++ ">"

> composeLookaheadRel :: LookaheadRel -> LookaheadRel -> LookaheadRel
> composeLookaheadRel (LookaheadRel p1 c1) (LookaheadRel p2 c2) =
>   LookaheadRel (composeIndentRel p1 p2) (composeIndentRel c1 c2)