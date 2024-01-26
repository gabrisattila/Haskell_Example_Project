{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

import Control.Monad (ap)
import Data.String
import System.Exit

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--Egy egyszerű “while” nyelv szintaxisát definiáljuk:

type Name = String

data Exp
  = Add Exp Exp     -- e + e
  | Mul Exp Exp     -- e * e
  | Not Exp         -- not e     (Bool negáció)
  | IntLit Int      -- n        Int érték
  | BoolLit Bool    -- b        Bool érték
  | Eq Exp Exp      -- e == e   (egyenlőségvizsgálat, Int-re vagy Bool-ra)
  | Lt Exp Exp      -- e < e    (Int rendezés vizsgálata)
  | And Exp Exp     -- e && e   (Bool és)
  | Or Exp Exp      -- e || w   (Bool vagy)
  | Var Name        --          (változónév)
  deriving Show


infix 3 :=
data Statement = Name := Exp              
-- értékadás mint infix konstruktor
  | If Exp Program Program
  | While Exp Program
  deriving Show



type Program = [Statement]


--Ahhoz, hogy tömör beágyazott programokat írjunk, segít, ha túlterheljük a számliterálokat és a String literálokat Exp-hez.
--A számliterálokat és az aritmetikai műveleteket túlterhelhetjük úgy, hogy Num instance-t adunk Exp-hez.


instance Num Exp where
  (+)         = Add
  (*)         = Mul
  fromInteger = IntLit . fromInteger
  negate e    = Mul e (IntLit (-1))
  abs         = undefined
  signum      = undefined

--A String literálok túlterheléséhez be kell kapcsolni a fájlunkban a {-# LANGUAGE OverloadedStrings #-} kiterjesztést, importálni kell Data.String-et, és meg kell adni egy IsString Exp instance-t.

instance IsString Exp where
  fromString = Var

--Figyelem: ha a ghci-ben is akarunk túlterhelt String-eket használni, akkor ott a következő paranccsal szintén be kell kapcsolnunk ezt:
-- :set -XOverloadedStrings
--További rövidítések:

true :: Exp
true = BoolLit True

false :: Exp
false = BoolLit False

--Példák; az evalExp-t és runProgram-t lásd lentebb e jelenlegi feladatleírásban.

exp1 :: Exp
exp1 = 123 + 432 * 54 -- Add (IntLit 123) (Mul (IntLit 432) (IntLit 54))
-- evalExp exp1 [] == Left 23451

exp2 :: Exp
exp2 = 20 + "x" -- Add (IntLit 20) (Var "x")
-- evalExp exp2 [("x", Left 10)] == Left 30

prog1 :: Program
prog1 = [
  "x"   := 0,
  "acc" := 0,
  While (Not (Eq "x" 20)) [
      "acc" := "acc" + 10,
      "x"   := "x" + 1
    ]
  ]
-- runProgram prog1 == [("x",Left 20),("acc",Left 200)]

prog2 :: Program
prog2 = [
  "b1" := true,
  "x"  := 10,
  If "b1" ["x" := 100] ["x" := 200]
  ]
-- runProgram prog2 == [("b1",Right True),("x",Left 100)]
--Faktoriális program: ha az “in” változó tartalmazza a függvény bemenetét, akkor az “out”-ban lesz a függvény kimenete futás után.

fact :: Int -> Program
fact n = [
  "in"  := IntLit n,
  "out" := 1,
  While (Lt 0 "in") [
      "out" := "out" * "in",
      "in"  := "in" - 1
    ]
  ]
-- runProgram (fact 5) == [("in",Left 0),("out",Left 120)]

{-
Írj interpretert a nyelvhez!

Specifikáció:

A kifejezések kiértékelése:
Típushibák kezelése: ha valamilyen művelet rossz típusú értéket kap, pl. Add (IntLit 0) (BoolLit True) esetén, akkor dobjunk hibát “undefined” vagy “error” segítségével.
Artitmetikai és logikai értékek kiértékelése értelemszerű.
Az Eq művelet működjön Bool és Int típusú értékekre is! Ha két oldal típusa különböző, az természetesen hiba.
A változók kiértékelése történhet a standard “lookup” függvénnyel. Ha nincs a változó a környezetben, az hiba.
Statement kiértékelése:
Az értékadás:
Ha a változó még nem szerepel a környezetben, akkor vegyük fel a név-érték párt a környezet elejére.
Ha a változó már szerepel a környezetben, akkor az annak megfelelő értéket módosítsuk az új értékre.
If: értékeljük ki az Exp feltételt, ha igaz, futtassuk az első ágat, egyébként a másodikat.
While: futtassuk ismételten a Program-ot amíg az Exp feltétel értéke igaz.
-}

type Val = Either Int Bool
type Env = [(Name, Val)] 

--változók kiértékelése
evalExp :: Exp -> State Env Val
evalExp (Add e1 e3) = do
  v1 <- evalExp e1  -- v1 :: Val
  v3 <- evalExp e3  -- v2 :: Val
  case (v1, v3) of
    (Left n1, Left n2) -> pure $ Left (n1 + n2)
    _                  -> undefined  
evalExp (Mul e1 e3) = do
  v1 <- evalExp e1  -- v1 :: Val
  v3 <- evalExp e3  -- v2 :: Val
  case (v1, v3) of
    (Left n1, Left n2) -> pure $ Left (n1 + n2)
    _                  -> undefined
evalExp (Lt e1 e3)  = do
  v1 <- evalExp e1  -- v1 :: Val
  v3 <- evalExp e3  -- v2 :: Val
  case (v1, v3) of
    (Left n1, Left n2) -> pure $ Left (n1 + n2)
    _                  -> undefined
evalExp (Not e1)    = do
  v <- evalExp e1 
  case v of 
    Right b -> pure $ Right (not b)
    _       -> undefined
evalExp (IntLit n)  = pure $ Left n
evalExp (BoolLit b) = pure $ Right b

evalExp (Eq e1 e3)  = do
   v1 <- evalExp e1 -- v1 :: Val
   v3 <- evalExp e3 -- v3 :: Val
   case (v1, v3) of
    (Right m1, Right m2) -> pure $ Right (m1 == m2)
    _                    -> error "you can't compare a bool and an int value"

evalExp (Eq e4 e5) = do
  v1 <- evalExp e4 
  v3 <- evalExp e5 
  case (v1, v3) of 
    (Left n1, Left n2)   -> pure $ Right (n1 == n2)

evalExp (And e1 e3) = do
   v1 <- evalExp e1 -- v1 :: Val
   v3 <- evalExp e3 -- v3 :: Val
   case (v1, v3) of
    (Right n1, Right n2) -> pure $ Right (n1 && n2)
    _                    -> undefined
evalExp (Or e1 e3)  = do
   v1 <- evalExp e1 -- v1 :: Val
   v3 <- evalExp e3 -- v3 :: Val
   case (v1, v3) of
    (Right n1, Right n2) -> pure $ Right (n1 || n2)
    _                    -> undefined
evalExp (Var x)     = do
  env <- get
  case lookup x env of
    Just v -> pure v 
    Nothing -> undefined -- a változó nincs a környezetben    
--amely kiértékelés, és, lehetséges mellékhatás kezelés is  

e1' :: Exp
e1' = Mul (IntLit 30) (IntLit 2)

--segédek az evalStatmenthez
upEnv :: Name -> Val -> Env -> Env
upEnv x v [] = (x , v) : [] -- felvesszük az új változót, a környezet lista, remélhetőleg elejére
upEnv x v ((x' , v'):env) 
  | x == x'   = (x', v):env --megtaláltuk, hogy van ilyen nevű változó és, megváltoztatjuk a értékét 
  | otherwise = (x' , v') : upEnv x v env -- keressük tovább 

valToBool :: Val -> Bool
valToBool (Right r) = r 
valToBool (Left l) = undefined

valToInt :: Val -> Int
valToInt (Left l) = l 
valToInt (Right r) = undefined

envToNV :: Env -> [(Name, Val)]
envToNV [] = []
envToNV ((x, v):env) = (x, v) : envToNV env 

nvToProgram :: [(Name, Val)] -> Program
nvToProgram a = map (\(x, v) -> (x := (if valToBool v /= undefined then BoolLit (valToBool v) else IntLit (valToInt v)))) a

evalStatement :: Statement -> State Env ()
evalStatement (x := e) = do
  v <- evalExp e 
  modify (upEnv x v) -- megváltoztatja az e értékét 
evalStatement (If e p1 p2) = do
  v <- evalExp e 
  if valToBool v then mapM_ evalStatement p1 else mapM_ evalStatement p2
--evalStatement (While e p) = do
  --v < evalExp e
  --if valToBool v then evalStatement (While e (nvToProgram (map envToNV (map evalState (mapM_ evalStatement p))))) else p

--Rákérdezni hogy hogy kéne megoldani

evalProgram :: Program -> State Env ()
evalProgram sts = mapM_ evalStatement sts

-- Program futtatása üres környezetből indulva:
runProgram :: Program -> Env
runProgram prog = execState (evalProgram prog) []
