{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c) 

charw :: Char -> Parser ()
charw c = char c <* ws 

ws :: Parser ()
ws = () <$ many (char ' ')

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

string :: String -> Parser ()
string str = mapM_ char str

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

--A cél egyszerű funkcionális nyelv parsolása. A nyelv tartalmaz párokat, lambda kifejezéseket és let definíciókat. A következő a szintaxisfa:

type Name = String

data Exp
  = Var Name          -- változó
  | App Exp Exp       -- e1 e2
  | Lam Name Exp      -- \x -> e
  | Let Name Exp Exp  -- let x = e1 in e2
  | Pair Exp Exp      -- (e1, e2)
  | Fst               -- fst
  | Snd               -- snd
  deriving (Eq, Show)

{-
A következő nevek kulcsszavak: let, in, fst, snd.
A változónevek első karaktere betű (kis vagy nagybetű), a többi karakter lehet betű vagy számjegy. Változó nem lehet kulcsszó.
Whitespace karakternek számít szóköz és új sor, ezek bárhol szerepelhetnek nevek és szimbólumok között.
Zárójelezés lehetséges.
Precedenciák csökkenő erősségi sorrendben:
Atomi kifejezések: változók, fst, snd, zárójelezett kifejezések, pár kifejezések. A pár kifejezések szintaxisa (e1, e2), ahol e1 és e2 tetszőleges kifejezések. Figyelem: a zárojelezett kifejezések és a pár kifejezések is (-el kezdődnek, az ezek közötti választást kezeljük itt le.
Függvényalkalmazás: balra asszociál. A függvényalkalmazást tekintsük bináris operátornak, ahol az operátor az üres String. Azaz atomi kifejezéseket egyszerűen egymás után írva kapunk App kifejezést.
Lambda kifejezés: szintaxisa \x -> e ahol x változónév, e pedig újra lehet lambda kifejezés. Tehát a lambda kifejezés prefix, jobbra asszociáló operátor.
let kifejezés: szintaxisa let x = e1 in e2, ahol x változónév, e1 tetszőleges kifejezés, e2 pedig újra lehet let kifejezés. Tehát a let szintén jobbra asszociáló prefix operátorként viselkedik; viszont a belső e1 kifejezés bármi lehet, mivel két oldalról az = és in tokenek elhatárolják.
A top level parser olvassa be a kezdő whitespace-t és illeszkedjen az input végére.
Definiáljuk a parser-t!
-}


a :: Name 
a = "XX" 

digit :: Parser Char
digit = satisfy (\c -> c `elem` (['0'..'9']++['a'..'z']++['A'..'Z']))

varName :: Parser Exp 
varName = Var "XX" <$ ((:) <$> (satisfy (\c -> c `elem` ['a'..'z']++['A'..'Z'])) <*> some digit)

pairs :: Parser (Exp, Exp)
pairs = (,) <$> (e, e1)

kulcssz :: [String]
kulcssz = ["let", "in", "snd", "fst"]

fstsnd :: Parser Exp
fstsnd = (Fst <$ (char 'f' >> char 's' >> char 't')) <|> (Snd <$ (char 's' >> char 'n' >> char 'd')) 


pExp :: Parser Exp
pExp = undefined

-- rövid segédfüggvény
parse :: String -> Maybe (Exp, String)
parse sts = runParser fstsnd sts

--Példák a működésre:

{-
parse "x" == Just (Var "x","")
parse "x1" == Just (Var "x1","")
parse "1x" == Nothing

parse "f x" == Just (App (Var "f") (Var "x"),"")
parse "f x y" == Just (App (App (Var "f") (Var "x")) (Var "y"),"")
parse "f x y z" == Just (App (App (App (Var "f") (Var "x")) (Var "y")) (Var "z"),"")
parse "f (g x)" == Just (App (Var "f") (App (Var "g") (Var "x")),"")

parse "fst" == Just (Fst,"")
parse "snd" == Just (Snd,"")
parse "f fst snd" == Just (App (App (Var "f") Fst) Snd,"")
parse "fst (x, y)" == Just (App Fst (Pair (Var "x") (Var "y")),"")

parse "(x)" == Just (Var "x","")
parse "(f x)" == Just (App (Var "f") (Var "x"),"")
parse "(x, y)" == Just (Pair (Var "x") (Var "y"),"")
parse "(f x, g y)" == Just (Pair (App (Var "f") (Var "x")) (App (Var "g") (Var "y")),"")
parse "((x, y))" == Just (Pair (Var "x") (Var "y"),"")
parse "f (x, y) (z, z)" == Just (App (App (Var "f") (Pair (Var "x") (Var "y"))) (Pair (Var "z") (Var "z")),"")

parse "\\x -> x" == Just (Lam "x" (Var "x"),"")
parse "\\x -> f x" == Just (Lam "x" (App (Var "f") (Var "x")),"")
parse "\\x -> \\y -> x" == Just (Lam "x" (Lam "y" (Var "x")),"")
parse "\\x -> (x, y)" == Just (Lam "x" (Pair (Var "x") (Var "y")),"")
parse "(\\x -> x) y" == Just (App (Lam "x" (Var "x")) (Var "y"),"")

parse "let x = y in x" == Just (Let "x" (Var "y") (Var "x"),"")
parse "let f = \\x -> x in f" == Just (Let "f" (Lam "x" (Var "x")) (Var "f"),"")
parse "let f = let x = y in x in f" == Just (Let "f" (Let "x" (Var "y") (Var "x")) (Var "f"),"")
parse "let foo = x in let bar = y in foo" == Just (Let "foo" (Var "x") (Let "bar" (Var "y") (Var "foo")),"")
-}

