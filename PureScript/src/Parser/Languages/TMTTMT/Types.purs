module Parser.Languages.TMTTMT.Types where

data Pattern
  = Var String
  | Scalar String
  | Vector (Array Pattern)
  | Macro Expr (Array Expr)

data Expr
  = Pattern Pattern
  | Lambda (Array Case)

-- One branch:
--   matching
--     cond1
--     cond2
data Case = Case Matching (Array Condition)
-- Head of one branch:
--   pattern1 pattern2 => expr
data Matching = Matching (Array Pattern) Expr
-- One statement, applying a function:
--   fn calling
data Condition = Condition Expr Calling
-- One calling:
--   … expr1 expr2 => pattern
data Calling = Calling (Array Expr) Pattern

data Declaration
  = DefineCase String (Array String) Case
  | Query Expr
  | Comment String
