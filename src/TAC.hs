module TAC
  (compile)
where

import Syntax
import Semantic
import Prelude hiding (lookup)

data UniOp = Neg | Print
  deriving (Eq, Ord, Show)

data Instr
  = BInstr BinOp Addr Addr
  | UInstr UniOp Addr
  deriving (Eq, Ord, Show)

compile :: Prog -> Production
compile p = genTAC p (buildSymbolTable p)

data Production = Returns Addr [Instr] SymbolTable | Void [Instr] SymbolTable
  deriving (Eq, Ord, Show)

class GenTAC a where
  genTAC :: a -> SymbolTable -> Production

instance GenTAC Prog where
  genTAC (Prog _ stmnts ret) table1
    = let Void instrs1 table2 = genTAC stmnts table1
          Returns addr instrs2 table3 = genTAC ret table2
          in Returns addr (instrs1 ++ instrs2) table3

instance GenTAC Expr where
  genTAC (Var a) table = genTAC a table
  genTAC (Lit int) table = Returns (Const int) [] table
  genTAC (Op a name expr) table1 = let
    Returns saddr1 instrs1 table2 = genTAC name table1
    Returns saddr2 instrs2 table3 = genTAC expr table2
    (addr, table4) = newTemp Int table3 in
    Returns
      addr
      (instrs1 ++ instrs2 ++ [BInstr a saddr1 saddr2])
      table4

instance GenTAC Name where
  genTAC name table = Returns (lookup name table) [] table

instance GenTAC Statements where
  genTAC (Statements' stmnt) table = genTAC stmnt table
  genTAC (Statements stmnts stmnt) table1 = let
    (Void instrs1 table2) = genTAC stmnts table1
    (Void instrs2 table3) = genTAC stmnt table2 in
    Void (instrs1 ++ instrs2) table3

instance GenTAC Statement where
  genTAC (SAssign name expr) table1 =
    let Returns saddr instrs table2 = genTAC expr table1
        daddr = lookup name table2 in
      Void (instrs ++ [BInstr Assign daddr saddr]) table2
  genTAC (SExpr expr) table = genTAC expr table
  genTAC (SPrint expr) table1 =
    let Returns saddr instrs table2 = genTAC expr table1 in
      Void (instrs ++ [UInstr Print saddr]) table2

