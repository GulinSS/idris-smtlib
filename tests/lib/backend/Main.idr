module Main

import System
import System.File

import SMTLib.AST
import SMTLib.DSL
import SMTLib.Print
import SMTLib.Backend

||| Test basic solver startup and shutdown
testStartStop : IO ()
testStartStop = do
  putStrLn "=== Test: Start/Stop Solver ==="
  result <- startSolver defaultConfig
  case result of
    Left err => putStrLn $ "FAIL: " ++ show err
    Right solver => do
      putStrLn "Solver started successfully"
      exitCode <- closeSolver solver
      putStrLn $ "Solver closed with exit code: " ++ show exitCode

||| Test sending a simple command
testSimpleCommand : IO ()
testSimpleCommand = do
  putStrLn "\n=== Test: Simple Command ==="
  result <- startSolver defaultConfig
  case result of
    Left err => putStrLn $ "FAIL: " ++ show err
    Right solver => do
      -- Check sat with empty context (returns "sat")
      resp <- sendCommand solver CheckSat
      putStrLn $ "CheckSat response: " ++ show resp

      ignore $ closeSolver solver

||| Test declare-const and check-sat
testDeclareConst : IO ()
testDeclareConst = do
  putStrLn "\n=== Test: Declare Const ==="
  result <- startSolver defaultConfig
  case result of
    Left err => putStrLn $ "FAIL: " ++ show err
    Right solver => do
      -- Send commands in batch
      let cmds = [ DeclareConst (MkSymbol "x") (MkSort (MkIdentifier (MkSymbol "Bool") []) [])
                 , Assert (var "x")
                 , CheckSat
                 ]
      resp <- sendCommands solver cmds
      putStrLn $ "Commands sent: " ++ show resp

      -- Read check-sat response
      resp2 <- readSExpr solver
      putStrLn $ "CheckSat response: " ++ show resp2

      ignore $ closeSolver solver

||| Test withSolver helper
testWithSolver : IO ()
testWithSolver = do
  putStrLn "\n=== Test: withSolver ==="
  result <- withSolver defaultConfig $ \solver => do
    resp <- sendCommand solver CheckSat
    pure resp
  case result of
    Left err => putStrLn $ "FAIL: " ++ show err
    Right resp => putStrLn $ "Success! Response: " ++ show resp

||| Test batch commands
testBatchCommands : IO ()
testBatchCommands = do
  putStrLn "\n=== Test: Batch Commands ==="
  result <- startSolver defaultConfig
  case result of
    Left err => putStrLn $ "FAIL: " ++ show err
    Right solver => do
      -- Send multiple commands in batch
      let cmds = [ DeclareConst (MkSymbol "x") (MkSort (MkIdentifier (MkSymbol "Int") []) [])
                 , DeclareConst (MkSymbol "y") (MkSort (MkIdentifier (MkSymbol "Int") []) [])
                 , Assert (var "x" `eq` (10 - var "y"))
                 , CheckSat
                 ]
      resp <- sendCommands solver cmds
      putStrLn $ "Batch sent: " ++ show resp

      -- Read check-sat response
      resp1 <- readSExpr solver
      putStrLn $ "CheckSat response: " ++ show resp1

      ignore $ closeSolver solver

||| Test linear equations
testLinearEquations : IO ()
testLinearEquations = do
  putStrLn "\n=== Test: Linear Equations ==="
  result <- startSolver defaultConfig
  case result of
    Left err => putStrLn $ "FAIL: " ++ show err
    Right solver => do
      -- Declare x, y, z as Real
      let cmds = [ DeclareConst (MkSymbol "x") (MkSort (MkIdentifier (MkSymbol "Real") []) [])
                 , DeclareConst (MkSymbol "y") (MkSort (MkIdentifier (MkSymbol "Real") []) [])
                 , DeclareConst (MkSymbol "z") (MkSort (MkIdentifier (MkSymbol "Real") []) [])
                 , Assert $ (3.0 * var "x" + 2.0 * var "y" - var "z") `eq` 1.0
                 , Assert $ (2.0 * var "x" - 2.0 * var "y" + 4.0 * var "z") `eq` (-2.0)
                 , Assert $ ((-var "x") + 0.5 * var "y" - var "z") `eq` 0.0
                 , CheckSat
                 ]
      putStrLn $ "Sending commands..."
      resp <- sendCommands solver cmds
      putStrLn $ "Commands sent: " ++ show resp

      -- Read check-sat response
      resp1 <- readSExpr solver
      putStrLn $ "CheckSat response: " ++ show resp1

      ignore $ closeSolver solver

||| Test get-model
testGetModel : IO ()
testGetModel = do
  putStrLn "\n=== Test: Get Model ==="
  result <- startSolver defaultConfig
  case result of
    Left err => putStrLn $ "FAIL: " ++ show err
    Right solver => do
      let cmds = [ DeclareConst (MkSymbol "x") (MkSort (MkIdentifier (MkSymbol "Int") []) [])
                 , Assert (var "x" `eq` 42)
                 , CheckSat
                 ]
      resp <- sendCommands solver cmds
      putStrLn $ "Commands sent: " ++ show resp

      -- Read check-sat response
      resp1 <- readSExpr solver
      putStrLn $ "CheckSat response: " ++ show resp1

      -- Get model
      resp2 <- sendCommand solver GetModel
      putStrLn $ "GetModel response: " ++ show resp2

      ignore $ closeSolver solver

main : IO ()
main = do
  testStartStop
  testSimpleCommand
  testDeclareConst
  testWithSolver
  testBatchCommands
  testLinearEquations
  testGetModel
  putStrLn "\n=== All tests completed ==="