> module Happy.Indentation (
>       IndentRel(..),
>       LookaheadRel(..),
>       ) where

I will split these into different files later on

There are more relations, but these will do for now

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

> data LookaheadRel = LookaheadRel IndentRel IndentRel