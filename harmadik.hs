{-# language OverloadedStrings, DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Traversable
import Control.Applicative
import Control.Monad
import Data.String
import Data.Maybe
import Debug.Trace

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

{-
A program állapota tartalmaz:

Három regiszert (r1-r3), amelyekben Int tárolható.
Memóriaterületet (memory), amit egy [Int] lista reprezentál.
Egy regisztert (cmp), ami Ordering típusú értéket tárol, ahová az összehasonlítások eredménye kerül.
-}

data ProgState = ProgState {
  r1     :: Int,
  r2     :: Int,
  r3     :: Int,
  cmp    :: Ordering,
  memory :: [Int]
  } deriving (Eq, Show)


--A futás kezdésekor legyen a következő az állapot:

startState :: ProgState
startState = ProgState 0 0 0 EQ (replicate 10 0)

--Az elérhető utasításokat a következő típusok adják meg:


type Label = String  -- címke a programban, ahová ugrani lehet

data Register
  = R1
  | R2
  | R3
  deriving (Eq, Show)

data Destination
  = DstReg Register     -- regiszterbe írunk
  | DstDeref Register   -- memóriába írunk, az adott regiszterben tárolt index helyére
  deriving (Eq, Show)

data Source
  = SrcReg Register     -- regiszterből olvasunk
  | SrcDeref Register   -- memóriából olvasunk, az adott regiszterben tárolt index helyéről
  | SrcLit Int          -- szám literál
  deriving (Eq, Show)

data Instruction
  = Mov Destination Source   -- írjuk a Destination-be a Source értékét
  | Add Destination Source   -- adjuk a Destination-höz a Source értékét
  | Mul Destination Source   -- szorozzuk a Destination-t a Source értékével
  | Sub Destination Source   -- vonjuk ki a Destination-ből a Source értékét
  | Cmp Source Source        -- hasonlítsunk össze két Source értéket `compare`-el, az eredményt
                             -- írjuk a `cmp` regiszterbe

  | Jeq Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `EQ` van
  | Jlt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `LT` van
  | Jgt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `GT` van
  deriving (Eq, Show)

--Egy “nyers” program egy lista, amiben minden elem lehet vagy egy címke, vagy egy utasítás.

type RawProgram = [Either Label Instruction]
--Nézzünk néhány példa programot. A konvenció az, hogy minden program egy Left "start" címkével kezdődik.

a :: ProgState -> Instruction -> Destination -> Source -> ProgState
--Mov
a ProgState (rr1, rr2, rr3, o, m) (Mov d s) (DstReg r) (SrcLit i) 
  | r == R1 = rr1-rr1+i 
  | r == R2 = rr2-rr2+i
  | r == R3 = rr3-rr3+i 
a ProgState (rr1, rr2, rr3, o, m) (Mov d s) (DstReg r) (SrcReg i)
  | r == R1 && i == R1 = rr1  
  | r == R1 && i == R2 = rr1 - rr1 + rr2
  | r == R1 && i == R3 = rr1 - rr1 + rr3 
  | r == R2 && i == R1 = rr2 - rr2 + rr1
  | r == R2 && i == R2 = rr2 
  | r == R2 && i == R3 = rr2 - rr2 + rr3 
  | r == R3 && i == R1 = rr3 - rr3 + rr1
  | r == R3 && i == R2 = rr3 - rr3 + rr2
  | r == R3 && i == R3 = rr3 
a ProgState (rr1, rr2, rr3, o, m) (Mov d s) (DstReg r) (SrcDeref i)
  | r == R1 && i == R1 = rr1 - rr1 + m!!rr1
  | r == R1 && i == R2 = rr1 - rr1 + m!!rr2
  | r == R1 && i == R3 = rr1 - rr1 + m!!rr3 
  | r == R2 && i == R1 = rr2 - rr2 + m!!rr1
  | r == R2 && i == R2 = rr2 - rr2 + m!!rr2
  | r == R2 && i == R3 = rr2 - rr2 + m!!rr3 
  | r == R3 && i == R1 = rr3 - rr3 + m!!rr1
  | r == R3 && i == R2 = rr3 - rr3 + m!!rr2
  | r == R3 && i == R3 = rr3 - rr3 + m!!rr3 
a ProgState (rr1, rr2, rr3, o, m) (Mov d s) (DstDeref r) (SrcLit i)
  | r == R1 = m !! rr1 - m !! rr1 + i
  | r == R2 = m !! rr2 - m !! rr2 + i
  | r == R3 = m !! rr3 - m !! rr3 + i
a ProgState (rr1, rr2, rr3, o, m) (Mov d s) (DstDeref r) (SrcDeref i) 
  | r == R1 && i == R1 = m!!rr1
  | r == R1 && i == R2 = m!!rr1 - m!!rr1 + m!!rr2
  | r == R1 && i == R3 = m!!rr1 - m!!rr1 + m!!rr3 
  | r == R2 && i == R1 = m!!rr2 - m!!rr2 + m!!rr1
  | r == R2 && i == R2 = m!!rr2 - m!!rr2 + m!!rr2
  | r == R2 && i == R3 = m!!rr2 - m!!rr2 + m!!rr3 
  | r == R3 && i == R1 = m!!rr3 - m!!rr3 + m!!rr1
  | r == R3 && i == R2 = m!!rr3 - m!!rr3 + m!!rr2
  | r == R3 && i == R3 = m!!rr3 - m!!rr3 + m!!rr3 
a ProgState (rr1, rr2, rr3, o, m) (Mov d s) (DstDeref r) (SrcReg i) 
  | r == R1 && i == R1 = m!!rr1 - m!!rr1 + rr1
  | r == R1 && i == R2 = m!!rr1 - m!!rr1 + rr2
  | r == R1 && i == R3 = m!!rr1 - m!!rr1 + rr3 
  | r == R2 && i == R1 = m!!rr2 - m!!rr2 + rr1
  | r == R2 && i == R2 = m!!rr2 - m!!rr2 + rr2
  | r == R2 && i == R3 = m!!rr2 - m!!rr2 + rr3 
  | r == R3 && i == R1 = m!!rr3 - m!!rr3 + rr1
  | r == R3 && i == R2 = m!!rr3 - m!!rr3 + rr2
  | r == R3 && i == R3 = m!!rr3 - m!!rr3 + rr3 
--Add
a ProgState (rr1, rr2, rr3, o, m) (Add d s) (DstDeref r) (SrcLit i) 
  | r == R1 = m !! rr1 + i
  | r == R2 = m !! rr2 + i
  | r == R3 = m !! rr3 + i
a ProgState (rr1, rr2, rr3, o, m) (Add d s) (DstDeref r) (SrcReg i) 
  | r == R1 && i == R1 = m!!rr1 + rr1
  | r == R1 && i == R2 = m!!rr1 + rr2
  | r == R1 && i == R3 = m!!rr1 + rr3 
  | r == R2 && i == R1 = m!!rr2 + rr1
  | r == R2 && i == R2 = m!!rr2 + rr2
  | r == R2 && i == R3 = m!!rr2 + rr3 
  | r == R3 && i == R1 = m!!rr3 + rr1
  | r == R3 && i == R2 = m!!rr3 + rr2
  | r == R3 && i == R3 = m!!rr3 + rr3 
a ProgState (rr1, rr2, rr3, o, m) (Add d s) (DstDeref r) (SrcDeref i) 
  | r == R1 && i == R1 = m!!rr1 + m!!rr1
  | r == R1 && i == R2 = m!!rr1 + m!!rr2
  | r == R1 && i == R3 = m!!rr1 + m!!rr3 
  | r == R2 && i == R1 = m!!rr2 + m!!rr1
  | r == R2 && i == R2 = m!!rr2 + m!!rr2
  | r == R2 && i == R3 = m!!rr2 + m!!rr3 
  | r == R3 && i == R1 = m!!rr3 + m!!rr1
  | r == R3 && i == R2 = m!!rr3 + m!!rr2
  | r == R3 && i == R3 = m!!rr3 + m!!rr3 
a ProgState (rr1, rr2, rr3, o, m) (Add d s) (DstReg r) (SrcLit i) 
  | r == R1 = rr1 + i 
  | r == R2 = rr2 + i
  | r == R3 = rr3 + i 
a ProgState (rr1, rr2, rr3, o, m) (Add d s) (DstReg r) (SrcDeref i) 
  | r == R1 && i == R1 = 2*m!!rr1
  | r == R1 && i == R2 = m!!rr1 + m!!rr2
  | r == R1 && i == R3 = m!!rr1 + m!!rr3 
  | r == R2 && i == R1 = m!!rr2 + m!!rr1
  | r == R2 && i == R2 = m!!rr2 + m!!rr2
  | r == R2 && i == R3 = m!!rr2 + m!!rr3 
  | r == R3 && i == R1 = m!!rr3 + m!!rr1
  | r == R3 && i == R2 = m!!rr3 + m!!rr2
  | r == R3 && i == R3 = m!!rr3 + m!!rr3 
a ProgState (rr1, rr2, rr3, o, m) (Add d s) (DstReg r) (SrcReg i) 
  | r == R1 && i == R1 = m!!rr1 + rr1
  | r == R1 && i == R2 = m!!rr1 + rr2
  | r == R1 && i == R3 = m!!rr1 + rr3 
  | r == R2 && i == R1 = m!!rr2 + rr1
  | r == R2 && i == R2 = m!!rr2 + rr2
  | r == R2 && i == R3 = m!!rr2 + rr3 
  | r == R3 && i == R1 = m!!rr3 + rr1
  | r == R3 && i == R2 = m!!rr3 + rr2
  | r == R3 && i == R3 = m!!rr3 + rr3 
--Mull
a ProgState (rr1, rr2, rr3, o, m) (Mul d s) (DstReg r) (SrcLit i) 
  | r == R1 = rr1 * i 
  | r == R2 = rr2 * i
  | r == R3 = rr3 * i 
a ProgState (rr1, rr2, rr3, o, m) (Mul d s) (DstReg r) (SrcReg i) 
  | r == R1 && i == R1 = rr1 * rr1
  | r == R1 && i == R2 = rr1 * rr2
  | r == R1 && i == R3 = rr1 * rr3 
  | r == R2 && i == R1 = rr2 * rr1
  | r == R2 && i == R2 = rr2 * rr2
  | r == R2 && i == R3 = rr2 * rr3 
  | r == R3 && i == R1 = rr3 * rr1
  | r == R3 && i == R2 = rr3 * rr2
  | r == R3 && i == R3 = rr3 * rr3 
a ProgState (rr1, rr2, rr3, o, m) (Mul d s) (DstReg r) (SrcDeref i) 
  | r == R1 && i == R1 = rr1 * m!!rr1
  | r == R1 && i == R2 = rr1 * m!!rr2
  | r == R1 && i == R3 = rr1 * m!!rr3 
  | r == R2 && i == R1 = rr2 * m!!rr1
  | r == R2 && i == R2 = rr2 * m!!rr2
  | r == R2 && i == R3 = rr2 * m!!rr3 
  | r == R3 && i == R1 = rr3 * m!!rr1
  | r == R3 && i == R2 = rr3 * m!!rr2
  | r == R3 && i == R3 = rr3 * m!!rr3 
a ProgState (rr1, rr2, rr3, o, m) (Mul d s) (DstDeref r) (SrcLit i) 
  | r == R1 = m !! rr1 * i
  | r == R2 = m !! rr2 * i
  | r == R3 = m !! rr3 * i
a ProgState (rr1, rr2, rr3, o, m) (Mul d s) (DstDeref r) (SrcReg i) 
  | r == R1 && i == R1 = m!!rr1 * rr1
  | r == R1 && i == R2 = m!!rr1 * rr2
  | r == R1 && i == R3 = m!!rr1 * rr3 
  | r == R2 && i == R1 = m!!rr2 * rr1
  | r == R2 && i == R2 = m!!rr2 * rr2
  | r == R2 && i == R3 = m!!rr2 * rr3 
  | r == R3 && i == R1 = m!!rr3 * rr1
  | r == R3 && i == R2 = m!!rr3 * rr2
  | r == R3 && i == R3 = m!!rr3 * rr3 
a ProgState (rr1, rr2, rr3, o, m) (Mul d s) (DstDeref r) (SrcDeref i) 
  | r == R1 && i == R1 = m!!rr1 * m!!rr1
  | r == R1 && i == R2 = m!!rr1 * m!!rr2
  | r == R1 && i == R3 = m!!rr1 * m!!rr3 
  | r == R2 && i == R1 = m!!rr2 * m!!rr1
  | r == R2 && i == R2 = m!!rr2 * m!!rr2
  | r == R2 && i == R3 = m!!rr2 * m!!rr3 
  | r == R3 && i == R1 = m!!rr3 * m!!rr1
  | r == R3 && i == R2 = m!!rr3 * m!!rr2
  | r == R3 && i == R3 = m!!rr3 * m!!rr3 
--Sub
a ProgState (rr1, rr2, rr3, o, m) (Sub d s) (DstReg r) (SrcLit i) 
  | r == R1 = rr1 - i 
  | r == R2 = rr2 - i
  | r == R3 = rr3 - i 
a ProgState (rr1, rr2, rr3, o, m) (Sub d s) (DstReg r) (SrcReg i) 
  | r == R1 && i == R1 = rr1 - rr1
  | r == R1 && i == R2 = rr1 - rr2
  | r == R1 && i == R3 = rr1 - rr3 
  | r == R2 && i == R1 = rr2 - rr1
  | r == R2 && i == R2 = rr2 - rr2
  | r == R2 && i == R3 = rr2 - rr3 
  | r == R3 && i == R1 = rr3 - rr1
  | r == R3 && i == R2 = rr3 - rr2
  | r == R3 && i == R3 = rr3 - rr3 
a ProgState (rr1, rr2, rr3, o, m) (Sub d s) (DstReg r) (SrcDeref i) 
  | r == R1 && i == R1 = rr1 - m!!rr1
  | r == R1 && i == R2 = rr1 - m!!rr2
  | r == R1 && i == R3 = rr1 - m!!rr3 
  | r == R2 && i == R1 = rr2 - m!!rr1
  | r == R2 && i == R2 = rr2 - m!!rr2
  | r == R2 && i == R3 = rr2 - m!!rr3 
  | r == R3 && i == R1 = rr3 - m!!rr1
  | r == R3 && i == R2 = rr3 - m!!rr2
  | r == R3 && i == R3 = rr3 - m!!rr3 
a ProgState (rr1, rr2, rr3, o, m) (Sub d s) (DstDeref r) (SrcLit i) 
  | r == R1 = m !! rr1 - i
  | r == R2 = m !! rr2 - i
  | r == R3 = m !! rr3 - i
a ProgState (rr1, rr2, rr3, o, m) (Sub d s) (DstDeref r) (SrcReg i) 
  | r == R1 && i == R1 = m!!rr1 - rr1
  | r == R1 && i == R2 = m!!rr1 - rr2
  | r == R1 && i == R3 = m!!rr1 - rr3 
  | r == R2 && i == R1 = m!!rr2 - rr1
  | r == R2 && i == R2 = m!!rr2 - rr2
  | r == R2 && i == R3 = m!!rr2 - rr3 
  | r == R3 && i == R1 = m!!rr3 - rr1
  | r == R3 && i == R2 = m!!rr3 - rr2
  | r == R3 && i == R3 = m!!rr3 - rr3 
a ProgState (rr1, rr2, rr3, o, m) (Sub d s) (DstDeref r) (SrcDeref i) 
  | r == R1 && i == R1 = m!!rr1 - m!!rr1
  | r == R1 && i == R2 = m!!rr1 - m!!rr2
  | r == R1 && i == R3 = m!!rr1 - m!!rr3 
  | r == R2 && i == R1 = m!!rr2 - m!!rr1
  | r == R2 && i == R2 = m!!rr2 - m!!rr2
  | r == R2 && i == R3 = m!!rr2 - m!!rr3 
  | r == R3 && i == R1 = m!!rr3 - m!!rr1
  | r == R3 && i == R2 = m!!rr3 - m!!rr2
  | r == R3 && i == R3 = m!!rr3 - m!!rr3 



-- Beírunk r1-be 10-et, r2-be 20-at
p1 :: RawProgram
p1 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Left "l1",                            -- tehetünk bárhova címkét, nem muszáj használni a programban
  Right $ Mov (DstReg R2) (SrcLit 20)
  ]

-- Kiszámoljuk 10 faktoriálisát, az eredményt r2-ben tároljuk
p2 :: RawProgram
p2 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Right $ Mov (DstReg R2) (SrcLit 1),
  Left "loop",
  Right $ Mul (DstReg R2) (SrcReg R1),
  Right $ Sub (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 0),
  Right $ Jgt "loop"
  ]

-- Feltöltjük 0-9-el a memóriát
p3 :: RawProgram
p3 = [
  Left "start",
  Right $ Mov (DstDeref R1) (SrcReg R1),
  Right $ Add (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 10),
  Right $ Jlt "start"
  ]
--A kiértékeléshez kényelmesebb, ha nem a RawProgram típust használjuk, hanem a következőt:





type Program = [(Label, [Instruction])]

--A Program megadása RawProgram-ból a következő: minden input-beli címkéhez párosítsuk az összes címke után levő Instruction listáját. Definiáljuk ezt a függvényt!

segR :: (Either a b) -> b 
segR (Right r) = r

segL :: (Either a b) -> a 
segL (Left l) = l 

toProgram :: RawProgram -> Program
toProgram [Left l, Right r] = [(y, [x]) | x <- (segR (Right r)), y <- (segL (Left l))]

--Példák a működésre:

{-
toProgram p1 == [("start",[Mov (DstReg R1) (SrcLit 10),Mov (DstReg R2) (SrcLit 20)]),("l1",[Mov (DstReg R2) (SrcLit 20)])]

toProgram [Left "start", Left "l1", Right $ Mov (DstReg R1) (SrcLit 10)] == [("start",[Mov (DstReg R1) (SrcLit 10)]),("l1",[Mov (DstReg R1) (SrcLit 10)])]

toProgram [Left "start", Right $ Mov (DstReg R1) (SrcLit 10),  Left "l1"] == [("start",[Mov (DstReg R1) (SrcLit 10)]),("l1",[])]
-}

--Írjunk kiértékelőt! Használjuk a State monádot a következőképpen:

type M a = State ProgState a
--A kiértékelő függvény legyen a következő típusú. Adott továbbá egy segédfüggvény, amivel a példákat lehet tömören futtatni.

eval :: Program -> [Instruction] -> M ()
eval = undefined

-- futtatunk egy nyers programot a startState-ből kiindulva
runProgram :: RawProgram -> ProgState
runProgram rprog = case toProgram rprog of
  []                  -> startState
  prog@((_, start):_) -> execState (eval prog start) startState

--Az eval függvény működése a következő: a kapott Program tárolja az összes Label-[Instruction] párosítást, a kapott [Instruction] pedig a jelenleg feldolgozandó utasításokat. Az utasításokat sorban értelmezzük, viszont ha ugrás (Jmp, Jeq, Jlt, Jgt) utasítást kapunk, és teljesül az ugrás feltétele, akkor az adott címkének megfelelő utasítás listát kinézzük a Program-ból, és azt értékeljük tovább.

--Példák a működésre:

{-
runProgram p1 == ProgState {r1 = 10, r2 = 20, r3 = 0, cmp = EQ, memory = [0,0,0,0,0,0,0,0,0,0]}
runProgram p2 == ProgState {r1 = 0, r2 = 3628800, r3 = 0, cmp = EQ, memory = [0,0,0,0,0,0,0,0,0,0]}
runProgram p3 == ProgState {r1 = 10, r2 = 0, r3 = 0, cmp = EQ, memory = [0,1,2,3,4,5,6,7,8,9]}
-}

--Az eval függvényhez tetszőleges segédfüggvények definiálhatók (és érdemes definiálni néhányat).

