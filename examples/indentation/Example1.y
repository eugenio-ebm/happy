{
module Main where
}

%name parse
%tokentype { Char }
%error { parseError }

%token
  'a' { 'a' }
  'b' { 'b' }

%%

S : [A, >] [A, >]     { S $1 $2 }

A : ['a', =] [A, >]   { Aa $2 }
  | ['b', >]          { Ab }

{
data S = S A A
  deriving Show

data A = Aa A | Ab
  deriving Show

parseError :: [Char] -> a
parseError []      = error "Parse error: unexpected end of input"
parseError (c : _) = error ("Parse error: unexpected character " ++ show c)

main :: IO ()
main = getLine >>= print . parse
}