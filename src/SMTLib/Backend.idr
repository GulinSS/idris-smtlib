module SMTLib.Backend

import Data.String

import System.File
import System.File.Process

import Text.PrettyPrint.Prettyprinter

import SMTLib.AST
import SMTLib.Print

%default total

||| Configuration for an SMT solver process
public export
record SolverConfig where
  constructor MkSolverConfig
  ||| Path to the solver executable
  exe : String
  ||| Command-line arguments for the solver
  args : List String

||| Default configuration for Z3 solver
public export
defaultConfig : SolverConfig
defaultConfig = MkSolverConfig "z3" ["-in"]

||| A running solver process with bidirectional communication
public export
record Solver where
  constructor MkSolver
  ||| The underlying subprocess
  process : SubProcess

||| Error type for solver operations
public export
data SolverError : Type where
  ||| Failed to start the solver process
  StartError : FileError -> SolverError
  ||| Failed to write to solver's stdin
  WriteError : FileError -> SolverError
  ||| Failed to read from solver's stdout
  ReadError : FileError -> SolverError

export
Show SolverError where
  show (StartError err) = "Failed to start solver: \{show err}"
  show (WriteError err) = "Failed to write to solver: \{show err}"
  show (ReadError err) = "Failed to read from solver: \{show err}"

||| Start a solver process with the given configuration
||| Returns a Solver handle for bidirectional communication
public export
startSolver : HasIO io => SolverConfig -> io (Either SolverError Solver)
startSolver cfg = do
  let cmd = cfg.exe ++ " " ++ unwords cfg.args
  result <- popen2 cmd
  case result of
    Left err => pure $ Left $ StartError err
    Right proc => pure $ Right $ MkSolver proc

||| Send a raw string command to the solver and flush
sendCommandRaw : HasIO io => Solver -> String -> io (Either SolverError ())
sendCommandRaw solver cmd = do
  result <- fPutStr (input $ process solver) (cmd ++ "\n")
  case result of
    Left err => pure $ Left $ WriteError err
    Right () => do
      fflush (input $ process solver)
      pure $ Right ()

||| Read a line from the solver's stdout
readLine : HasIO io => Solver -> io (Either SolverError String)
readLine solver = do
  result <- fGetLine (output $ process solver)
  case result of
    Left err => pure $ Left $ ReadError err
    Right line => pure $ Right line

||| Read a complete S-expression from the solver
||| Handles nested parentheses, strings, and quoted symbols
public export
covering
readSExpr : HasIO io => Solver -> io (Either SolverError String)
readSExpr solver = readSExpr' 0 ""
  where
    ||| Count parentheses depth, handling strings and quoted symbols
    countParens : Int -> String -> Int
    countParens d s = countParens' d (unpack s)
      where
        mutual
          countParens' : Int -> List Char -> Int
          countParens' d [] = d
          countParens' d ('(' :: cs) = countParens' (d + 1) cs
          countParens' d (')' :: cs) = countParens' (d - 1) cs
          countParens' d ('"' :: cs) = skipString d cs
          countParens' d ('|' :: cs) = skipQuoted d cs
          countParens' d (';' :: _) = d  -- comment, rest of line ignored
          countParens' d (_ :: cs) = countParens' d cs

          skipString : Int -> List Char -> Int
          skipString d [] = d
          skipString d ('"' :: '"' :: cs) = skipString d cs  -- escaped quote
          skipString d ('"' :: cs) = countParens' d cs
          skipString d (_ :: cs) = skipString d cs

          skipQuoted : Int -> List Char -> Int
          skipQuoted d [] = d
          skipQuoted d ('|' :: cs) = countParens' d cs
          skipQuoted d (_ :: cs) = skipQuoted d cs

    covering
    readSExpr' : Int -> String -> io (Either SolverError String)
    readSExpr' depth acc =
      if depth <= 0 && acc /= ""
        then pure $ Right (trim acc)
        else do
          result <- readLine solver
          case result of
            Left err => pure $ Left err
            Right line => do
              let newDepth = countParens depth line
              let newAcc = acc ++ line
              if newDepth <= 0 && newAcc /= ""
                then pure $ Right (trim newAcc)
                else readSExpr' newDepth newAcc

||| Send a command and read the response
public export
covering
sendCommand : HasIO io => Solver -> Command -> io (Either SolverError String)
sendCommand solver cmd = do
  let cmdStr = show (toDoc cmd)
  result <- sendCommandRaw solver cmdStr
  case result of
    Left err => pure $ Left err
    Right () => readSExpr solver

||| Send multiple commands as a batch
public export
sendCommands : HasIO io => Solver -> List Command -> io (Either SolverError ())
sendCommands solver cmds = do
  let script = show (ppScript cmds)
  sendCommandRaw solver script

||| Close the solver process and wait for it to terminate
||| Returns the exit code of the solver process
public export
closeSolver : HasIO io => Solver -> io Int
closeSolver solver = do
  ignore $ closeFile (input $ process solver)
  popen2Wait (process solver)

||| Run a solver action with automatic resource management
||| Starts the solver, runs the action, and ensures cleanup
public export
covering
withSolver : HasIO io => SolverConfig -> (Solver -> io (Either SolverError a)) -> io (Either SolverError a)
withSolver cfg action = do
  result <- startSolver cfg
  case result of
    Left err => pure $ Left err
    Right solver => do
      res <- action solver
      ignore $ closeSolver solver
      pure res