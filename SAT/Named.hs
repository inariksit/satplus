module SAT.Named where

import SAT ( Solver, newSolver )
import SAT.Unary ( Unary )
import qualified SAT as S
import qualified SAT.Bool as B
import qualified SAT.Unary as U
import qualified SAT.Optimize as O

import Data.List
--------------------------------------------------------------------------------
-- Add name for variables, for nicer looking output

data Lit = Lit {name :: String, literal :: S.Lit} -- deriving (Ord,Eq)

instance Show Lit where
  show (Lit "" x)           = show x
  show (Lit nm l@(S.Lit _)) = if S.pos l then nm else '~':nm
  show (Lit nm (S.Bool _)) = nm

instance Ord Lit where
  compare p q = compare (literal p) (literal q)

instance Eq Lit where
  p == q = literal p == literal q

----

nameLits :: Int -> [Lit] -> [String]
nameLits n lits = if length lits < n 
                     then map show lits 
                     else map show (take 3 lits) ++ ["..."]

newLit :: Solver -> String -> IO Lit
newLit s nm = Lit nm `fmap` S.newLit s

true :: Lit
true = Lit "true" S.true

false :: Lit
false = Lit "false" S.false

neg :: Lit -> Lit
neg (Lit nm lit) = Lit nm (S.neg lit)

pos :: Lit -> Bool
pos = S.pos . literal

addClause :: Solver -> [Lit] -> IO ()
addClause s  = S.addClause s . map literal 

solve :: Solver -> [Lit] -> IO Bool
solve s = S.solve s . map literal 

modelValue :: Solver -> Lit -> IO Bool
modelValue s = S.modelValue s . literal

--------------------------------------------------------------------------------

equiv :: Solver -> String -> Lit -> Lit -> IO Lit
equiv s nm lit1 lit2 = Lit nm `fmap` B.equiv s (literal lit1) (literal lit2)

implies :: Solver -> String -> Lit -> Lit -> IO Lit
implies s nm lit1 lit2 = Lit nm `fmap` B.implies s (literal lit1) (literal lit2)

andl' :: Solver -> [Lit] -> IO Lit
andl' s lits = let nm = intercalate " & " (nameLits 4 lits)
              in Lit nm `fmap` B.andl s (map literal lits)

andl :: Solver -> String -> [Lit] -> IO Lit
andl s nm lits = Lit nm `fmap` B.andl s (map literal lits)

orl' :: Solver -> [Lit] -> IO Lit
orl' s lits = let nm = intercalate " | " (nameLits 4 lits)
              in Lit nm `fmap` B.orl s (map literal lits)

orl :: Solver -> String -> [Lit] -> IO Lit
orl s nm lits = Lit nm `fmap` B.orl s (map literal lits)

xorl :: Solver -> [Lit] -> IO Lit
xorl s lits = let nm = intercalate " # " (nameLits 4 lits)
               in Lit nm `fmap` B.xorl s (map literal lits)

--------------------------------------------------------------------------------

count :: Solver -> [Lit] -> IO Unary
count s = U.count s . map literal

solveMaximize :: Solver -> [Lit] -> Unary -> IO Bool
solveMaximize s ass obj = O.solveMaximize s (map literal ass) obj
