module SMTLib.DSL

import Control.Monad.Identity
import Control.Monad.Free
import Control.Monad.Writer

import SMTLib.AST

%default total

public export
declareConst : String -> String -> Command
declareConst name sort = DeclareConst (MkSymbol name) (MkSort (MkIdentifier (MkSymbol sort) []) [])

public export
var : String -> Term
var name = QI (MkQIdentifier (MkIdentifier (MkSymbol name) []) Nothing)

public export
app : String -> List Term -> Term
app nam ts = FunApp (MkQIdentifier (MkIdentifier (MkSymbol nam) []) Nothing) ts

-- TODO something more typesafe?
public export
eq : Term -> Term -> Term
eq t1 t2 = app "=" [t1, t2]

public export
le : Term -> Term -> Term
le t1 t2 = app "<=" [t1, t2]

public export
and : Term -> Term -> Term
and t1 t2 = app "and" [t1, t2]

public export
distinct : List Term -> Term
distinct = app "distinct"

public export
Num Term where
  fromInteger i = Lit (Numeral i)
  t1 + t2 = app "+" [t1, t2]
  t1 * t2 = app "*" [t1, t2]

public export
Neg Term where
  negate t = app "-" [0, t]
  t1 - t2 = app "-" [t1, t2]

public export
index : Term -> Term -> Term
index t1 t2 = app "_" [t1, t2]

public export
bvand : Term -> Term -> Term
bvand t1 t2 = app "bvand" [t1, t2]

public export
bvashr : Term -> Term -> Term
bvashr t1 t2 = app "bvashr" [t1, t2]

--bvlit : String -> Nat -> Term
--bvlit s n = index

public export
d : Double -> Term
d x = Lit (Decimal x)

public export
data SMTReal = RealVar String

public export
toTerm : SMTReal -> Term
toTerm (RealVar name) = var name

public export
data SMTInt = IntVar String

public export
toTermI : SMTInt -> Term
toTermI (IntVar name) = var name

public export
data SMTBV : Nat -> Type where
  BVVar : String -> (n : Nat) -> SMTBV n

public export
toTermBV : SMTBV n -> Term
toTermBV (BVVar name _) = var name

public export
data SMTScriptF : Type -> Type where
  SDeclareReal : String -> SMTScriptF SMTReal
  SDeclareInt : String -> SMTScriptF SMTInt
  SDeclareBV : String -> (n : Nat) -> SMTScriptF (SMTBV n)
  SAssert : Term -> SMTScriptF ()
  SCheckSat : SMTScriptF ()
  SGetModel : SMTScriptF ()

public export
SMTScript : Type -> Type
SMTScript = Free SMTScriptF

public export
declareReal : String -> SMTScript SMTReal
declareReal s = lift $ SDeclareReal s

public export
declareInt : String -> SMTScript SMTInt
declareInt s = lift $ SDeclareInt s

public export
declareBV : String -> (n : Nat) -> SMTScript (SMTBV n)
declareBV s n = lift $ SDeclareBV s n

public export
assert : Term -> SMTScript ()
assert t = lift $ SAssert t

public export
checkSat : SMTScript ()
checkSat = lift SCheckSat

public export
getModel : SMTScript ()
getModel = lift SGetModel

public export
writeCommands : SMTScript a -> Writer (List Command) a
writeCommands fr with (toView fr)
  writeCommands _ | (Pure a') = pure a'
  writeCommands _ | (Bind instruction k) = do
    res <- case instruction of
      SDeclareReal s => do tell [declareConst s "Real"]
                           pure $ RealVar s
      SDeclareInt s => do tell [declareConst s "Int"]
                          pure $ IntVar s
      SDeclareBV s n => do tell [declareConst s $ "(_ BitVec " ++ show n ++ ")"]
                           pure $ BVVar s n
      SAssert t => tell [Assert t]
      SCheckSat => tell [CheckSat]
      SGetModel => tell [GetModel]
    assert_total $ writeCommands (k res)

public export
renderCommands : SMTScript a -> List Command
renderCommands = snd . runIdentity . runWriterT . writeCommands