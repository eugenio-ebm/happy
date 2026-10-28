{
module Main where
}

%name parse
%tokentype { Char }
%error { parseError }

%token
  'i' { 'i' }   -- if
  't' { 't' }   -- then
  'e' { 'e' }   -- else
  'x' { 'x' }
  'y' { 'y' }

%%

S : [E, >]                                         { S $1 }

E : ['i', =] [E, >] ['t', =] [E, >] ['e', =] [E, >]  { IfThenElse $2 $4 $6 }
  | ['i', =] [E, >] ['t', =] [E, >]                  { IfThen $2 $4 }
  | ['x', =]                                         { X }
  | ['y', =]                                         { Y }

{
data S = S E
  deriving Show

data E
  = IfThenElse E E E
  | IfThen E E
  | X
  | Y
  deriving Show

parseError :: [Char] -> a
parseError []      = error "Parse error: unexpected end of input"
parseError (c : _) = error ("Parse error: unexpected character " ++ show c)

main :: IO ()
main = getLine >>= print . parse
}