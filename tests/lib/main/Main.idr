module Main

import System
import System.File
import Data.List
import Data.String
import Text.PrettyPrint.Prettyprinter

import Control.Monad.Free
import Control.Monad.Identity
import Control.Monad.Writer

import SMTLib.AST
import SMTLib.DSL
import SMTLib.Print

ex1 : SMTScript ()
ex1 = do ds <- traverse declareReal ["x", "y", "z"]
         let [x, y, z] = map toTerm ds | _ => pure ()
         assert $ (3.0*x + 2.0*y - z) `eq` 1.0
         assert $ (2.0*x - 2.0*y + 4.0*z) `eq` (-2.0)
         assert $ ((-x) + (d 0.5)*y - z) `eq` 0.0
         checkSat
         getModel

ex2 : SMTScript ()
ex2 = do ds <- traverse declareInt ["circle", "triangle", "square"]
         let [c, t, s] = map toTermI ds | _ => pure ()
         assert $ (c + c) `eq` 10
         assert $ (c*s + s) `eq` 12
         assert $ (c*s - t*c) `eq` c
         checkSat
         getModel

isDigit : Term -> Term
isDigit x = (0 `le` x) `and` (x `le` 9)

constructWord : List Term -> Term
constructWord xs = sum $ zipWith (*) (reverse xs) $ map fromInteger $ take (length xs) (iterate (*10) 1)
  where
    iterate : (a -> a) -> a -> Stream a
    iterate f x = x :: iterate f (f x)

ex3 : SMTScript ()
ex3 = do ds' <- traverse declareInt ["D", "E", "M", "N", "O", "R", "S", "Y"]
         let ds = map toTermI ds'
         assert $ distinct ds
         traverse_ (assert . isDigit) ds
         let [d,e,m,n,o,r,s,y] = ds | _ => pure ()
         let send  = constructWord [s,e,n,d]
         let more  = constructWord [m,o,r,e]
         let money = constructWord [m,o,n,e,y]
         assert $ (send + more) `eq` money
         checkSat
         getModel

ex4 : SMTScript ()
ex4 = do ds' <- traverse declareInt ["A", "I", "L", "N", "O", "R", "S", "T", "V"]
         let ds = map toTermI ds'
         assert $ distinct ds
         traverse_ (assert . isDigit) ds
         let [a,i,l,n,o,r,s,t,v] = ds | _ => pure ()
         let violin = constructWord [v,i,o,l,i,n]
         let viola  = constructWord [v,i,o,l,a]
         let sonata = constructWord [s,o,n,a,t,a]
         let trio   = constructWord [t,r,i,o]
         assert $ (violin + violin + viola) `eq` (trio + sonata)
         checkSat
         getModel

ex5 : SMTScript ()
ex5 = do ds' <- traverse declareInt ["H", "E", "L", "O", "W", "R", "D"]
         let ds = map toTermI ds'
         let isPos = \x => (1 `le` x) `and` (x `le` 9)
         assert $ distinct ds
         traverse_ (assert . isPos) ds
         let [h,e,l,o,w,r,d] = ds | _ => pure ()
         assert $ (h + e + l + l + o) `eq` 25
         assert $ (w + o + r + l + d) `eq` 25
         checkSat
         getModel

{-
ex6 : SMTScript ()
ex6 = do bv <- declareBV "out" 64
         checkSat
         getModel
-}

writeSMT : SMTScript a -> (fname : String) -> IO ()
writeSMT cmd fname = do ignore $ writeFile fname $ show $ ppScript $ renderCommands cmd
                        pure ()

renderSMT : SMTScript a -> IO ()
renderSMT cmd = putStrLn $ show $ ppScript $ renderCommands cmd

execZ3 : (infile, outfile : String) -> IO Int
execZ3 infile outfile = system $ "z3 -smt2 " ++ infile ++ " > " ++ outfile

main : IO ()
main = do
  putStrLn "--- Example 1 ---"
  renderSMT ex1
  putStrLn "--- Example 2 ---"
  renderSMT ex2
  putStrLn "--- Example 3 ---"
  renderSMT ex3
  let inf = "test5a.smt2"
  let outf = "out.smt2"
  writeSMT ex5 inf
  i <- execZ3 inf outf
  printLn i
  eos <- readFile outf
  case eos of
    Left err => printLn err
    Right str => for_ (lines str) putStrLn
