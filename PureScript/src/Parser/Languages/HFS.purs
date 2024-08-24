module Parser.Languages.HFS where

import Parser.Parserlude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (isJust)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Semigroup.Foldable (foldl1)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Debug (spy)
import Effect.Exception (message, try)
import Effect.Unsafe (unsafePerformEffect)
import Parser.Comb as Comb
import Parser.Lexing (FailedStack(..))

-- | `HFList`: an `Array`-based representation for FFI
newtype HFList = HFList (Array HFList)

hfsToTree :: HFS -> HFList
hfsToTree x = HFList (hfsToTree <$> Array.reverse (Array.fromFoldable (hfs x)))

-- | `BigNat` = nonnegative `BigInt`s
foreign import data BigNat :: Type

instance monoidBigNat :: Monoid BigNat where
  mempty = bnZro
instance semigroupBigNat :: Semigroup BigNat where
  append = bnUni

instance semiringBigNat :: Semiring BigNat where
  zero = bnZro
  one = bnOne
  add = bnAdd
  mul = bnMul

instance eqBigNat :: Eq BigNat where
  eq = bnCEQ

instance ordBigNat :: Ord BigNat where
  compare x y | bnCLT x y = LT
  compare x y | bnCLT y x = GT
  compare _ _ = EQ

foreign import fromInt :: Int -> BigNat
foreign import toInt :: BigNat -> Int
foreign import mkBigNat :: String -> BigNat
foreign import bnBin :: BigNat -> String
foreign import bnDec :: BigNat -> String
foreign import bnHex :: BigNat -> String
foreign import bnZro :: BigNat
foreign import bnOne :: BigNat
foreign import bnCEQ :: BigNat -> BigNat -> Boolean
foreign import bnCLT :: BigNat -> BigNat -> Boolean
foreign import bnAdd :: BigNat -> BigNat -> BigNat
foreign import bnSub :: BigNat -> BigNat -> BigNat
foreign import bnMul :: BigNat -> BigNat -> BigNat
foreign import bnDiv :: BigNat -> BigNat -> BigNat
foreign import bnMod :: BigNat -> BigNat -> BigNat
foreign import bnPow :: BigNat -> BigNat -> BigNat
foreign import bnShL :: BigNat -> BigNat -> BigNat
foreign import bnShR :: BigNat -> BigNat -> BigNat
foreign import bnUni :: BigNat -> BigNat -> BigNat
foreign import bnInt :: BigNat -> BigNat -> BigNat
foreign import bnSym :: BigNat -> BigNat -> BigNat
foreign import bnWidth :: BigNat -> BigNat
foreign import bnCount :: BigNat -> BigNat
foreign import bnUnpack :: BigNat -> Array BigNat

-- | Preferred based for a `HFS` as a number
data Base = Bin | Dec | Hex
derive instance eqBase :: Eq Base

-- | Hereditarily Finite Sets
data HFS
  = NumLike Base BigNat
  | SetLike (Set HFS)

instance eqHFS :: Eq HFS where
  eq (NumLike _ n1) (NumLike _ n2) = n1 == n2
  eq l r = hfs l == hfs r

instance ordHFS :: Ord HFS where
  compare (NumLike _ n1) (NumLike _ n2) = compare n1 n2
  compare l0 r0 =
    -- Compare highest elements against highest elements
    -- (like a lexiocographic order)
    go
      (List.reverse $ Set.toUnfoldable (hfs l0))
      (List.reverse $ Set.toUnfoldable (hfs r0))
    where
    go = case _, _ of
      l1 : ls, r1 : rs -> case compare l1 r1 of
        EQ -> go ls rs
        notEqual -> notEqual
      Nil, _ : _ -> LT
      _ : _, Nil -> GT
      Nil, Nil -> EQ

instance monoidHFS :: Monoid HFS where
  mempty = SetLike Set.empty
instance semigroupHFS :: Semigroup HFS where
  append = hfsUni
instance semiringHFS :: Semiring HFS where
  zero = NumLike Hex zero
  one = NumLike Hex one
  add = hfsAdd
  mul = hfsMul

isEmpty :: HFS -> Boolean
isEmpty (NumLike _ n) = n == zero
isEmpty (SetLike members) = Set.isEmpty members

isZero :: HFS -> Boolean
isZero = isEmpty

isOne :: HFS -> Boolean
isOne (NumLike _ n) = n == one
isOne (SetLike members)
  | Set.size members == 1
  , all isEmpty members = true
isOne _ = false

isTwo :: HFS -> Boolean
isTwo (NumLike _ n) = n == one + one
isTwo (SetLike members)
  | Set.size members == 1
  , all isOne members = true
isTwo _ = false

-- | `isZero || isOne`
triv :: HFS -> Boolean
triv (NumLike _ n) = n <= one
triv (SetLike members) | Set.isEmpty members = true
triv (SetLike members)
  | Set.size members <= 1
  , all isEmpty members = true
triv _ = false

-- | Do the two `HFS`es have no members in common.
disjoint :: HFS -> HFS -> Boolean
disjoint (NumLike _ n1) (NumLike _ n2) = bnInt n1 n2 == zero
-- TODO: check bounds or something?
disjoint l r = Set.isEmpty (Set.intersection (hfs l) (hfs r))

isSubsetOf :: HFS -> HFS -> Boolean
isSubsetOf (NumLike _ n1) (NumLike _ n2) = bnInt n1 n2 == n2
isSubsetOf l r = Set.subset (hfs l) (hfs r)


-- | Deserialize from a simple `Int`.
hfsFromInt :: Int -> HFS
hfsFromInt = NumLike Dec <<< fromInt

hfsFromFoldable :: forall f. Foldable f => f HFS -> HFS
hfsFromFoldable = SetLike <<< Set.fromFoldable

-- | Number of members of the `HFS`, also: the number of bits set.
hfsCount :: HFS -> HFS
hfsCount (SetLike members) = NumLike Dec (fromInt (Set.size members))
hfsCount (NumLike _ n) = NumLike Dec (bnCount n)

-- | The bit width, number of bits to represent it. I think?
hfsWidth :: HFS -> HFS
hfsWidth (SetLike members) = sum (Set.findMax members)
hfsWidth (NumLike _ n) = NumLike Dec (bnWidth n)

-- | Recursive depth of the `HFS`. But also the bit width minus 1?
hfsDepth :: HFS -> HFS
hfsDepth = sum <<< Set.findMax <<< Set.map (\x -> one + hfsDepth x) <<< hfs

isSingleton :: HFS -> Maybe HFS
isSingleton (NumLike _ n1)
  | w <- bnWidth n1
  , n1 == bnShL one w
  = Just (NumLike Hex w)
isSingleton (SetLike members)
  | Set.size members == 1
  = Set.findMax members
isSingleton _ = Nothing

-- | Add as numbers. Optimized if they are disjoint sets.
hfsAdd :: HFS -> HFS -> HFS
hfsAdd l r | isZero r = l
hfsAdd l r | isZero l = r
hfsAdd l@(SetLike s1) r@(SetLike s2) | disjoint l r = SetLike (s1 <> s2)
-- TODO: add from low order bits
hfsAdd l r = hfsBase l r (bnAdd (bn l) (bn r))

-- | Union, bitwise OR.
hfsUni :: HFS -> HFS -> HFS
hfsUni l@(NumLike _ n1) r@(NumLike _ n2) = hfsBase l r (bnUni n1 n2)
hfsUni l r = SetLike (Set.union (hfs l) (hfs r))

-- | Intersection, bitwise AND.
hfsInt :: HFS -> HFS -> HFS
hfsInt l@(NumLike _ n1) r@(NumLike _ n2) = hfsBase l r (bnInt n1 n2)
-- TODO: truncate to number?
hfsInt l r = SetLike (Set.intersection (hfs l) (hfs r))

-- | Truncating division as numbers.
hfsDiv :: HFS -> HFS -> HFS
hfsDiv _ r | isEmpty r = zero
hfsDiv l r | isOne r = l
hfsDiv l r = hfsBase l r (bnDiv (bn l) (bn r))

-- | Modulus as numbers.
hfsMod :: HFS -> HFS -> HFS
hfsMod _ r | triv r = zero
hfsMod l r = hfsBase l r (bnMod (bn l) (bn r))

-- | Multiplication as numbers.
hfsMul :: HFS -> HFS -> HFS
hfsMul _ r | isZero r = r
hfsMul l r | isOne r = l
hfsMul l _ | isZero l = l
hfsMul l r | isOne l = r
hfsMul l r = hfsBase l r (bnMul (bn l) (bn r))

-- | Helper for choosing the base of the result.
hfsBase :: HFS -> HFS -> BigNat -> HFS
hfsBase (NumLike b1 _) (NumLike b2 _) n | b1 == b2 = NumLike b1 n
hfsBase l (NumLike b2 _) n | triv l = NumLike b2 n
hfsBase (NumLike b1 _) r n | triv r = NumLike b1 n
hfsBase (NumLike b1 _) (SetLike _) n = NumLike b1 n
hfsBase (SetLike _) (NumLike b2 _) n = NumLike b2 n
hfsBase _ _ n = NumLike Hex n

-- | `HFS` as `BigNat`.
bn :: HFS -> BigNat
bn (NumLike _ n) = n
bn (SetLike members) = members # foldMap (bn >>> bnShL one)

-- | `HFS` as `Set HFS`.
hfs :: HFS -> Set HFS
hfs (SetLike members) = members
hfs (NumLike _ n) = Set.fromFoldable $ NumLike Hex <$> bnUnpack n

-- | Create a singleton set.
hfsSingle :: HFS -> HFS
hfsSingle = SetLike <<< Set.singleton

-- | Pull out the items of the second that are a subset of the first. Maybe.
-- TODO: actually optimize
hfSubsetOf :: HFS -> HFS -> Set HFS
hfSubsetOf _ = hfs

-- | Difference of sets.
hfsDif :: HFS -> HFS -> HFS
hfsDif l@(NumLike _ n1) r@(NumLike _ n2) = hfsBase l r (bnSub n1 (bnInt n1 n2))
hfsDif l r = SetLike (Set.difference (hfs l) (hfSubsetOf l r))

-- | Subtract as numbers.
hfsSub :: HFS -> HFS -> HFS
hfsSub l@(SetLike _) r@(SetLike _) | isSubsetOf l r = hfsDif l r
-- TODO: sub from high order bits
hfsSub l r = hfsBase l r (bnSub (bn l) (bn r))

-- | Is the first a member of the second?
hfsMem :: HFS -> HFS -> Boolean
hfsMem l (SetLike members) = Set.member l members
hfsMem (NumLike _ n1) (NumLike _ n2)
  | n1 > bnWidth n2 = false
  | otherwise = bnInt (bnShL one n1) n2 /= zero
hfsMem l r = Set.member l (hfs r)

-- | Symmetric difference, bitwise XOR.
hfsSym :: HFS -> HFS -> HFS
hfsSym l@(NumLike _ n1) r@(NumLike _ n2) = hfsBase l r (bnSym n1 n2)
hfsSym l r = SetLike $
  (\x y -> Set.difference (Set.union x y) (Set.intersection x y)) (hfs l) (hfs r)

-- | Power (exponent).
hfsPow :: HFS -> HFS -> HFS
hfsPow l r | isOne r = l
hfsPow _ r | isZero r = one
hfsPow l _ | isZero l = l
hfsPow l r | isTwo l = hfsSingle r
hfsPow l r = hfsBase l l {- sic. -} (bnPow (bn l) (bn r))

-- | Bitshift left.
hfsShL :: HFS -> HFS -> HFS
hfsShL l _ | isZero l = l
hfsShL l r | isOne l = hfsSingle r
hfsShL l r = hfsBase l l {- sic. -} (bnShL (bn l) (bn r))

-- | Bitshift right.
hfsShR :: HFS -> HFS -> HFS
hfsShR l _ | isZero l = l
hfsShR l r | isOne l = hfsSingle r
hfsShR l r | hfsWidth l < r = hfsBase l l {- sic. -} zero
hfsShR l r = hfsBase l l {- sic. -} (bnShR (bn l) (bn r))

-- | Unpack function.
hfsUnpack :: HFS -> Array HFS
hfsUnpack (NumLike _ n) = bnUnpack n <#> NumLike Hex
hfsUnpack (SetLike members) = Set.toUnfoldable members

-- Functions to set preferred

hfsAsSet :: HFS -> HFS
hfsAsSet = SetLike <<< hfs
hfsAsNat :: HFS -> HFS
hfsAsNat h@(NumLike _ _) = h
hfsAsNat h = NumLike Hex (bn h)
hfsAsBin :: HFS -> HFS
hfsAsBin = NumLike Bin <<< bn
hfsAsDec :: HFS -> HFS
hfsAsDec = NumLike Dec <<< bn
hfsAsHex :: HFS -> HFS
hfsAsHex = NumLike Hex <<< bn
hfsAsHFS :: HFS -> HFS
hfsAsHFS = SetLike <<< Set.map (\x -> hfsAsHFS x) <<< hfs


--------------------------------------------------------------------------------
-- HatStack language design                                                   --
--------------------------------------------------------------------------------

stdlib :: String
stdlib = """
def single
  1#{}
end

def dup
  [.0]
end

def swap
  2#[.0 .1]
end

def rot
  3#[.1 .0 .2]
end

def drop
  1#[]
end

0 set false
1 set true

def not
  0 ==
end

## Unpack a singleton set
def unsingle
  $#
  1 != if
    throw
  end
end

## Kuratowski encoding of ordered pairs
def pair
  2#[ {{.0}, {.0, .1}} ]
end

## Unpack an ordered pair
def unpair
  $#
  dup 2 == if
    drop
    2#[
      .0 .1 .\ unsingle
      .0 unsingle
    ]
    return
  end
  dup 1 == if
    drop
    unsingle
    return
  end
  throw
end

## Project out the first element of the pair
## (the top of the stack)
def fst
  unpair 2#[ .0 ]
end

## Project out the second element of the pair
## (just below the top of the stack)
def snd
  unpair 2#[ .1 ]
end

## Maps are (finite) graphs of functions,
## a set of pairs
def apply
  2#[ .1# .0 ]
  begin
    $1 while
    3#[ .1-- , .0 , .2 ]
    unpair
    $2 == if
      $2 3+ #[ .0 ]
      return
    end
    drop
  end
  throw
end

## Domain of a map
def domain
  $#
  {}
  begin
    $1 while
    3#[ .1-- , .0 , .2 fst {.} | ]
  end
  swap drop
end

## Range of a map
def range
  $#
  {}
  begin
    $1 while
    3#[ .1-- , .0 , .2 snd {.} | ]
  end
  swap drop
end


## The Zermelo encoding of natural numbers
## as nested singleton sets
def Zermelo
  {} swap
  begin
    dup while
    2#[ {.1} , .0-- ]
  end
  drop
end

## The von Neumann ordinals
def vonNeumann
  {} swap
  begin
    dup while
    2#[ .1 {.1} | , .0-- ]
  end
  drop
end

## Truncating integer square root, adapted from
## https://github.com/waldemarhorwat/integer-roots
def sqrt
  ## Special case 0
  dup 0 == if
    return
  end
  ## Compute the initial estimate
  ## 2 ^ (log2 .0 / 2)
  ## (A lower bound, since both log2
  ## and 1>>. are truncating)
  [ .0 width-- 1>>. 1.<< ]
  ## Apply one step of Newton's method
  ## to get an overapproximation
  1#[ .0 .1 .0 /. + 1>>. ]
  begin
    ## Iterate downwards
    [ .0 .1 .0 /. + 1>>. ]
    [ .0 .1 < ]
    while
    2#[ .0 ]
  end
  ## Take previous approximation,
  ## not the last guess
  3#[ .1 ]
  Dec
end


"""

{-
def apply
  2#[ .1# .0 ]
  ⎛ $1 while
  ⎜ 3#[ .1-- .0  .2 ]
  ⎜ unpair
  ⎜ $2 == if
  ⎜   $2 3+ #[ .0 ]
  ⎜   return
  ⎜ end
  ⎝ drop
  throw
end
-}

stdenv :: Lazy Env
stdenv = defer \_ ->
  case String.trim stdlib # force theParser of
    Left _ -> emptyEnv
    Right instrs -> run (emptyEnv { instrs = instrs })

withStdenv :: Array Instr -> Env
withStdenv instrs =
  let env = force stdenv in
  env { instrs = env.instrs <> instrs }

-- | Parse a literal.
lit :: Comber HFS
lit = "lit" #-> \litRec -> oneOf
  [ rawr "\\d+" <#> mkBigNat >>> NumLike Dec
  , rawr "0b[01]+" <#> mkBigNat >>> NumLike Bin
  , rawr "0x[\\da-fA-F]+" <#> mkBigNat >>> NumLike Hex
  , zero <$ rawr "0b"
  , zero <$ rawr "0x"
  , mempty <$ token "∅"
  ]

-- | Binary operators.
data Op
  = Add
  | Sub
  | Bus
  | Mul
  | Div
  | Vid
  | Mod
  | Dom
  | Pow
  | Wop
  | Min
  | Max
  | ShL
  | ShR
  | LSh
  | RSh
  | Dis
  | Con
  | Imp
  | Pmi
  | Xor
  | Sel
  | Uni
  | Int
  | Dif
  | Fid
  | Sym
  | Mem
  | Rem
  | CEQ
  | CNE
  | CLT
  | CGT
  | CLE
  | CGE
  | PLT
  | PLE
  | PGT
  | PGE

-- | Data about each binary operator.
data OpMeta
  = Symm Op String (Array String) (Array String) String
  | Sided Op Op String (Array String) String
  | Order Op String (Array String) Op String (Array String) String

-- | Listing of the operators.
opMeta :: Array OpMeta
opMeta =
  -- Arithmetic
  [ Symm Add "+" [] ["\x2211"] "Addition as numbers"
  , Sided Sub Bus "-" ["\x2238", "\x2212"] "Monus (truncating minus) as numbers"
  , Symm Mul "*" ["\x00D7"] ["\x220F"] "Multiplication as numbers"
  , Sided Div Vid "/" ["\x00F7", "\x2215"] "Integer division (rounding down)"
  , Sided Mod Dom "%" [] "Integer modulus (remainder)"
  , Sided Pow Wop "^" [] "Integer power"
  , Symm Min "/\\" ["\x2227"] ["\x22C0"] "Minimum"
  , Symm Max "\\/" ["\x2228"] ["\x22C1"]"Maximum"
  -- Bitwise/set-theory operators
  , Sided ShL LSh "<<" ["\x226A"] "Left shift"
  , Sided ShR RSh ">>" ["\x226B"] "Right shift"
  , Symm Uni "|" ["\x222A"] ["\x22C3"] "Union (as sets) or bitwise OR"
  , Symm Int "&" ["\x2229"] ["\x22C2"] "Intersection (as sets) or bitwise AND"
  , Sided Dif Fid "\\" ["\x2216"] "Difference of sets, clear bits" -- \.#2->1 := 2#[.1 .1 .0] & -.
  , Symm Sym "<>" ["\x2296"] [] "Symmetric difference of sets or bitwise XOR" -- bitwise XOR
  , Sided Mem Rem "@" ["\x2208"] "Set membership" -- .@#2->1 := 1 .<< =|
  -- Boolean logic (not bitwise)
  , Symm Dis "||" [] [] "Boolean OR"
  , Symm Con "&&" [] [] "Boolean AND"
  , Sided Imp Pmi "=>" ["\x21D2"]
    "Boolean implies (false implies anything, and anything implies true)" -- (x => y) := (!x || y)
  , Symm Xor ">|<" ["\x22BB"] []
    "Boolean XOR (see, it is an X and an OR)" -- ((x || y) && !(x && y))
  , Symm Sel "><" ["\x22C8"] []
    "Select (unique non-zero operand)" -- (x ? y ? 0 : x : y)
  -- Comparisons: equality
  , Symm CEQ "==" [] [] "Equals"
  , Symm CNE "!=" ["/="] [] "Not equals"
  -- Comparisons: total order
  , Order CLT "<" [] CGT ">" [] "Strict greater/less than, as numbers (total order)"
  , Order CLE "<=" ["\x2264", "\x226F"] CGE ">=" ["\x2265", "\x226E"] "Weak greater/less than, as numbers (total order)"
  -- Comparisons: partial order
  , Order PLT "<|" ["\x228A", "\x2282"] PGT "|>" ["\x228B", "\x2283"]
    "Strict subset (partial order – the pipe is a hint that it involves bitwise OR)"
  , Order PLE "=|" ["\x2286", "\x2AE4"] PGE "|=" ["\x2287", "\x22A8"]
    "Weak subset (partial order – the pipe is a hint that it involves bitwise OR, also looks like the entails sign)" -- (x =| y) := (x == x & y)
  ]

data OpExec
  = NoId (HFS -> HFS -> HFS)
  | ZeroId (HFS -> HFS -> HFS)
  | OneId (HFS -> HFS -> HFS)
  | Transitively (HFS -> HFS -> Boolean)
  | NonAssoc (Array HFS -> HFS)

-- | Implementation of the binary operators, and the identity element to use
-- | for their N-ary versions.
chooseIdOp :: Op -> OpExec
chooseIdOp = case _ of
  -- Arithmetic
  Add -> ZeroId hfsAdd
  Sub -> NoId hfsSub
  Bus -> NoId (flip hfsSub)
  Mul -> OneId hfsMul
  Div -> NoId hfsDiv
  Vid -> NoId (flip hfsDiv)
  Mod -> NoId hfsMod
  Dom -> NoId (flip hfsMod)
  Pow -> NoId hfsPow
  Wop -> NoId (flip hfsPow)
  Min -> NoId min
  Max -> ZeroId max
  -- Bitwise/set-theory operators
  ShL -> NoId hfsShL
  ShR -> NoId hfsShR
  LSh -> NoId (flip hfsShL)
  RSh -> NoId (flip hfsShR)
  -- Boolean logic (not bitwise)
  Dis -> ZeroId (booly (||))
  Con -> OneId (booly (&&))
  Imp -> Transitively \x y -> (x /= zero) `implies` (y /= zero)
  Pmi -> Transitively \y x -> (x /= zero) `implies` (y /= zero)
  Xor -> ZeroId (booly (/=))
  Sel -> NonAssoc \xs ->
    case Array.filter (_ /= zero) xs of
      [x] -> x
      _ -> zero
  Uni -> ZeroId hfsUni
  Int -> NoId hfsInt
  Dif -> NoId \x y -> hfsSub x (hfsInt x y)
  Fid -> NoId \y x -> hfsSub x (hfsInt x y)
  Sym -> ZeroId hfsSym
  Mem -> Transitively \x y -> hfsInt (hfsShL one x) y /= zero
  Rem -> Transitively \y x -> hfsInt (hfsShL one x) y /= zero
  -- Comparisons: equality
  CEQ -> Transitively (==)
  CNE -> NonAssoc \xs ->
    if Array.length xs == Array.length (Array.nub xs) then one else zero
  -- Comparisons: total order
  CLT -> Transitively (<)
  CGT -> Transitively (>)
  CLE -> Transitively (<=)
  CGE -> Transitively (>=)
  -- Comparisons: partial order
  PLT -> Transitively \x y -> x /= y && x == hfsInt x y
  PLE -> Transitively \x y -> x == hfsInt x y
  PGT -> Transitively \y x -> x /= y && x == hfsInt x y
  PGE -> Transitively \y x -> x == hfsInt x y
  where
  booly f x y = if f (x /= zero) (y /= zero) then one else zero

-- | Parser for the n-ary and binary operators, respectively.
op :: Comber (Either Op Op)
op = "op"#: oneOf do
  opMeta >>= case _ of
    Symm r dflt toks bigops _doc -> (Array.cons dflt toks) >>= \tok ->
      [ Right r <$ token tok
      , Left r <$ (token ("#" <> tok) <|> token (tok <> "#"))
      , Left r <$ if tok == dflt then oneOfMap token bigops else empty
      ]
    Sided l r dflt toks _doc -> (Array.cons dflt toks) >>= \tok ->
      [ Right l <$ token (tok <> ".")
      , Left l <$ token ("#" <> tok)
      , Right r <$ token ("." <> tok)
      , Left r <$ token (tok <> "#")
      ]
    Order l tl tls r tr trs _doc ->
      [ Right l <$ oneOfMap token (Array.cons tl tls)
      , Left l <$ oneOfMap (token <<< ("#" <> _)) (Array.cons tl tls)
      , Right r <$ oneOfMap token (Array.cons tr trs)
      , Left r <$ oneOfMap (token <<< ("#" <> _)) (Array.cons tr trs)
      ]

-- | Builtin functions.
data Fn
  = Count
  | Width
  | Depth
  | Single
  | Unpack
  | Pack
  | ToSet
  | ToNat
  | ToBin
  | ToDec
  | ToHex
  | ToHFS
  | NoOp
  | Decr
  | Incr

-- | Parse for these builtins.
fn :: Comber Fn
fn = "fn"#: oneOf
  [ Count <$ token "count"
  , Width <$ token "width"
  , Single <$ token "{.}"
  , Unpack <$ do token "unpack" <|> token "$#"
  , Pack <$ do token "pack" <|> token "{#}"
  , ToSet <$ token "Set"
  , ToNat <$ (token "Nat" <|> token "Num")
  , ToBin <$ token "Bin"
  , ToDec <$ token "Dec"
  , ToHex <$ token "Hex"
  , ToHFS <$ token "HFS"
  , NoOp <$ token ","
  , NoOp <$ rawr "##[^\n]*(\n|$)"
  , Decr <$ token "--"
  , Incr <$ token "++"
  ]

data Braces
  = StartStack
  | NStartStack
  | EndStack
  | NEndStack
  | StartSet
  | NStartSet
  | EndSet
  | NEndSet

braces :: Comber Braces
braces = "braces"#: oneOf
  [ StartStack <$ token "["
  , NStartStack <$ token "#["
  , EndStack <$ token "]"
  , NEndStack <$ token "]#"
  , StartSet <$ token "{"
  , NStartSet <$ token "#{"
  , EndSet <$ token "}"
  , NEndSet <$ token "}#"
  ]

-- | Control flow!
data Ctrl
  = Try
  | Throw
  | Catch
  | Rethrow
  | Recover
  | End
  | Begin
  -- TODO: labeled
  | While
  | Continue
  | Break
  | If
  | Else
  | Def
  | Return
  | Exit

derive instance eqCtrl :: Eq Ctrl
derive instance ordCtrl :: Ord Ctrl
derive instance genericCtrl :: Generic Ctrl _
instance enumCtrl :: Enum Ctrl where
  succ = genericSucc
  pred = genericPred
instance boundedCtrl :: Bounded Ctrl where
  bottom = genericBottom
  top = genericTop
instance boundedEnumCtrl :: BoundedEnum Ctrl where
  toEnum = genericToEnum
  fromEnum = genericFromEnum
  cardinality = genericCardinality
instance showCtrl :: Show Ctrl where
  show = genericShow

ctrl :: Comber Ctrl
ctrl = "ctrl"#: do
  oneOf $ (identity :: Array ~> Array) $
    enumFromTo bottom top <#> \v ->
      v <$ token (String.toLower (show v))

-- | All instructions.
data Instr
  = Op Op
  | OpFold Op
  | Fn Fn
  | Braces Braces
  | Lit HFS
  | Ctrl Ctrl
  | PeekPrev Int Boolean
  | PeekThis Int Boolean
  | Var String
  | Set String

parser :: Comber (Array Instr)
parser = pure [] <|> do
  map NEA.toArray $
    many1SepBy "actions" ws $ oneOf
      [ either OpFold Op <$> op
      , Fn <$> fn
      , Lit <$> lit
      , Ctrl <$> ctrl
      , Braces <$> braces
      , token "." *> (PeekPrev <$> int <*> do isJust <$> optional (token "#"))
      , token "$" *> (PeekThis <$> int <*> do isJust <$> optional (token "#"))
      , Var <$> rawr "[a-zA-Z_-]+"
      , Set <$> do token "set" *> ws *> rawr "[a-zA-Z_-]+"
      ]

showHFS :: HFS -> String
showHFS (NumLike b n) = case b of
  Bin -> bnBin n
  Dec -> bnDec n
  Hex -> bnHex n
showHFS (SetLike members) = (\m -> "{" <> m <> "}") $
  intercalateMap ", " showHFS (Array.reverse (Array.fromFoldable members))

data StackKind
  = BaseStack
  | MkListDrop Int
  | MkSetDrop Int

type Stack = List HFS

theParser :: Lazy (String -> Either FullParseError (Array Instr))
theParser = defer \_ ->
  Comb.parseRegex topName (unwrap parser)

parseAndRun :: String -> Either String String
parseAndRun = parseAndRun' >>> bimap (either convertParseError identity)
  (intercalateMap "\n---\n" (intercalateMap "\n" showHFS))

parseAndRun' :: String -> Either (Either FullParseError String)
  (NonEmptyArray (Array HFS))
parseAndRun' s = case String.trim s # force theParser of
  Left err -> Left (Left err)
  Right instrs -> case run (withStdenv instrs) of
    { error: Just err } -> Left (Right err)
    { stacks } -> Right $ NEA.fromFoldable1 stacks <#> Array.fromFoldable

-- | This used to be a `foldMap`, but then I added functions and looping.
run :: Env -> Env
run env
  | Just instr <- env.instrs Array.!! env.instruction =
    run (interpret instr env)
run env = env

-- | The runtime environment. Everything lives here, and each instruction is
-- | an endomorphism `Env -> Env`.
type Env =
  -- The error message if an error has occurred
  { error :: Maybe String
  -- A map of global variables set to values
  , vars :: Map String HFS
  -- A map of procedures (not closures)
  , funs :: Map String Pointer
  -- The stack of stacks
  , stacks :: NonEmpty List Stack
  -- Which stack is referenced by `.0` et al.
  -- (Indexing into `NonEmpty List`)
  , pointed :: Int
  -- Frames of control flow
  , flow :: List { here :: Flow, prev :: Boolean }
  -- The list of instructions currently running
  , instrs :: Array Instr
  -- Whether it is currently running, or not
  , running :: Boolean
  -- The current instruction pointer
  , instruction :: Pointer
  }

-- | A pointer to an instruction (just an `Int`)
type Pointer = Int

-- | Control flow frames.
data Flow
  -- Whether the condition was true
  = InIf Boolean
  -- Whether the condition was false
  | InElse Boolean
  -- (No data, just a marker)
  | InTry
  -- The exception, if there was one
  | InCatch (Maybe String)
  -- Pointer to itself
  | InBegin Pointer
  -- Pointer to the previous begin
  | InWhile (Maybe Pointer)
  -- (No data, just a marker)
  | InFunDef
  -- Pointer to return to
  | InFunCall Pointer
  | BuildingSet Int HFS
  | BuildingStack Int Int

-- | Start from nothing.
emptyEnv :: Env
emptyEnv =
  { error: Nothing
  , vars: Map.empty
  , funs: Map.empty
  , stacks: Nil :| Nil
  , pointed: 0
  , flow: Nil
  , running: true
  , instrs: []
  , instruction: 0
  }

-- | Run a single instruction. (Assumed to be the instruction that
-- | `env.instruction` is pointing at.)
interpret :: Instr -> Env -> Env
interpret instr original = case instr, env of
  -- Control flow is implemented in the laziest, most inefficient way possible:
  -- there are no jump aheads, only jump behinds, and you just have to keep
  -- fake-running code (keeping track of control flow constructs). This does have
  -- the benefit of not having to fix the control flow stack when exceptions
  -- are thrown.
  Ctrl c, _ -> case c of
    -- Try just pushes itself onto the stack
    Try -> env { flow = { prev: env.running, here: InTry } : env.flow }
    -- Throw sets an error if we are actually running
    Throw | env.running -> env { error = Just "Throw" }
    Throw -> env
    -- Catch will catch an error
    Catch ->
      case env.flow of
        { here: InTry, prev: wasRunning } : flows ->
          case wasRunning of
            true | Just msg <- env.error -> env
              { error = Nothing
              , flow = { prev: true, here: InCatch (Just msg) } : flows
              , running = true
              }
            _ -> env { flow = { prev: true, here: InCatch Nothing } : flows, running = false }
        _ -> env { error = Just "Catch without matching try" }
    Rethrow | not env.running -> env
    -- Rethrow will rethrow the error (since strings cannot be represented
    -- on the stack)
    Rethrow ->
      case env.flow of
        { prev: true, here: InCatch (Just msg) } : _ ->
          env { error = Just msg }
        _ -> env { error = Just "Rethrow outside of catch block" }
    -- Recover is unscoped catch?? idk
    Recover | env.running -> env
      { error = Nothing
      , flow = { prev: env.running, here: InCatch env.error } : env.flow
      , running = isJust env.error
      }
    Recover -> env { flow = { prev: false, here: InCatch Nothing } : env.flow }
    -- Begin saves its own pointer onto the stack, so while…end can loop back to it
    Begin -> env
      { flow = { prev: env.running, here: InBegin original.instruction } : env.flow }
    -- While pops a condition off the stack, preserves the return address from
    -- Begin if it is true
    While ->
      case env of
        -- Track scope without affecting the stack
        { flow: { here: InBegin _ } : flows } | not env.running -> env
          { flow = { prev: false, here: InWhile Nothing } : flows
          }
        -- Pop the condition, replace the InBegin with InWhile
        { stacks: (cond : stack) :| stacks, flow: { here: InBegin jmpStart } : flows } ->
          case cond /= zero of
            true -> env
              { stacks = stack :| stacks
              , flow = { prev: true, here: InWhile (Just jmpStart) } : flows
              , running = true
              }
            false -> env
              { stacks = stack :| stacks
              , flow = { prev: env.running, here: InWhile Nothing } : flows
              , running = false
              }
        _ -> env
          { flow = { prev: env.running, here: InWhile Nothing } : env.flow
          , error = Just "Bad while"
          , running = false
          }
    -- If pops a condition off the stack
    If ->
      case env of
        -- Track scope without affecting the stack
        _ | not env.running -> env
          { flow = { prev: false, here: InIf false } : env.flow
          }
        -- Pop and use that to take this branch (or wait for Else to resume it)
        { stacks: (cond : stack) :| stacks } ->
          let branchChosen = cond /= zero in env
          { stacks = stack :| stacks
          , flow = { prev: env.running, here: InIf branchChosen } : env.flow
          , running = branchChosen
          }
        _ -> env
          { flow = { prev: env.running, here: InIf false } : env.flow
          , error = Just $ underflow 1
          , running = false
          }
    -- Else flips the condition from If, but only if it was running in the first place
    Else ->
      case env of
        { flow: { prev: wasRunning, here: InIf cond } : flows } -> env
          { flow = { prev: wasRunning, here: InElse (wasRunning && not cond) } : flows
          , running = wasRunning && not cond
          }
        _ -> env
          { flow = { prev: env.running, here: InElse false } : env.flow
          , error = Just "Bad else"
          , running = false
          }
    Def ->
      case env.instrs Array.!! env.instruction of
        _ | not env.running -> env
          { flow = { prev: false, here: InFunDef } : env.flow
          }
        -- Def immediately adds itself, skips over the name, and waits until the
        -- end to resume execution
        Just (Var name) -> env
          { flow = { prev: env.running, here: InFunDef } : env.flow
          , funs = Map.insert name (env.instruction + 1) env.funs
          , instruction = env.instruction + 1
          , running = false
          }
        _ -> env
          { flow = { prev: env.running, here: InFunDef } : env.flow
          , running = false
          }
    -- Return just stops running until the end of the function
    Return -> returnTo case _ of
      r@(InFunCall _) -> Just r
      _ -> Nothing
    -- Continue stops running until the while loop loops
    Continue -> returnTo case _ of
      r@(InWhile (Just _)) -> Just r
      _ -> Nothing
    -- Break stops running and makes the while loop stop
    Break -> returnTo case _ of
      InWhile _ -> Just (InWhile Nothing)
      _ -> Nothing
    End ->
      case env.flow of
        Nil -> env
          { error = Just "End without any control flow"
          }
        -- begin…while…end will jump back to the begin
        { prev: true, here: InWhile (Just jmp) } : flows -> env
          { flow = flows
          , instruction = jmp
          , running = true
          }
        -- return to the caller
        { prev: true, here: InFunCall jmp } : flows -> env
          { flow = flows
          , instruction = jmp
          , running = true
          }
        flow : flows -> env
          { flow = flows
          , running = flow.prev
          }
    Exit | env.running -> env { instruction = Array.length env.instrs }
    Exit -> env
  -- These conditions mean that normal instructions will not run
  -- (Control flow needs to run always!)
  _, { running: false } -> env
  _, { error: Just _ } -> env
  -- Look up variables as values or functions.
  Var name, _ ->
    case Map.lookup name env.vars, Map.lookup name env.funs of
      Nothing, Nothing -> env { error = Just $ "Unknown name " <> show name }
      Just _, Just _ -> env { error = Just $ "Name cannot be a variable and function " <> show name }
      Just val, Nothing | stack :| stacks <- env.stacks ->
        env { stacks = val : stack :| stacks }
      Nothing, Just jmp -> env
        { flow = { prev: true, here: InFunCall env.instruction } : env.flow
        , instruction = jmp
        }
  Set name, { stacks: (val : stack) :| stacks } -> env
    { vars = Map.insert name val env.vars
    , stacks = stack :| stacks
    }
  Set _, _ -> env { error = Just $ underflow 1 }
  PeekPrev idx unpack, _ ->
    case env.stacks of
      this :| stacks | Just prev <- stacks List.!! (env.pointed - 1) ->
        case prev List.!! idx of
          Just x | unpack ->
            let xs = hfsUnpack x in
            env { stacks = ((hfsFromInt (Array.length xs) : Array.toUnfoldable xs) <> this) :|  stacks }
          Just x -> env { stacks = x : this :| stacks }
          Nothing -> env { error = Just $ underflow (idx+1) }
      _ -> env { error = Just ("." <> show idx <> " needs a previous stack, use $" <> show idx <> " if you want to target the current stack") }
  PeekThis idx unpack, _ ->
    case env.stacks of
      this :| stacks ->
        case this List.!! idx of
          Just x | unpack ->
            let xs = hfsUnpack x in
            env { stacks = ((hfsFromInt (Array.length xs) : Array.toUnfoldable xs) <> this) :| stacks }
          Just x -> env { stacks = x : this :| stacks }
          Nothing -> env { error = Just $ underflow (idx+1) }
  -- Binary operators
  Op o, _ -> onStack $ stack2 $ chooseOp o
  -- N-ary operators
  OpFold o, _ -> onStack $ stackN $ chooseOpFold o
  -- Other miscellaneous functions
  Fn f, _ -> onStack case f of
    Count -> stack1 hfsCount
    Width -> stack1 hfsWidth
    Depth -> stack1 hfsDepth
    Single -> stack1 hfsSingle
    Unpack -> stacking hfsUnpack
    Pack -> stackN $ Right <<< foldMap (hfsShL one)
    ToSet -> stack1 hfsAsSet
    ToNat -> stack1 hfsAsNat
    ToBin -> stack1 hfsAsBin
    ToDec -> stack1 hfsAsDec
    ToHex -> stack1 hfsAsHex
    ToHFS -> stack1 hfsAsHFS
    NoOp -> Right
    Decr -> stack1 (hfsSub <@> one)
    Incr -> stack1 (hfsAdd <@> one)
  Braces b, _ -> case b of
    StartStack -> env
      { stacks = Nil :| List.fromFoldable env.stacks
      , pointed = 1
      , flow = { prev: true, here: BuildingStack env.pointed 0 } : env.flow
      }
    NStartStack ->
      case env.stacks of
        hd : prev :| stacks | len <- toInt (bn hd) ->
          case List.length prev >= len of
            true -> env
              { stacks = Nil :| prev : stacks
              , pointed = 1
              , flow = { prev: true, here: BuildingStack env.pointed len } : env.flow
              }
            false -> env { error = Just $ underflow (len+1) }
        _ -> env { error = Just $ underflow (-1) }
    EndStack ->
      case env.stacks, env.flow of
        this :| prev : stacks, { here: BuildingStack ptd len } : flows -> env
          { stacks = (this <> List.drop len prev) :| stacks
          , pointed = ptd
          , flow = flows
          }
        _, _ -> env { error = Just "Bad EndStack" }
    NEndStack ->
      case env.stacks, env.flow of
        this :| prev : stacks, { here: BuildingStack ptd len } : flows -> env
          { stacks = hfsFromInt (List.length this) : (this <> List.drop len prev) :| stacks
          , pointed = ptd
          , flow = flows
          }
        _, _ -> env { error = Just "Bad NEndStack" }
    StartSet -> env
      { stacks = Nil :| List.fromFoldable env.stacks
      , pointed = env.pointed + 1
      , flow = { prev: true, here: BuildingSet env.pointed mempty } : env.flow
      }
    NStartSet ->
      case env.stacks of
        hd : prev :| stacks | len <- toInt (bn hd) ->
          if List.length prev >= len then env
            { stacks = Nil :| List.drop len prev : stacks
            , pointed = env.pointed + 1
            , flow = { prev: true, here: BuildingSet env.pointed (hfsFromFoldable (List.take len prev)) } : env.flow
            }
          else env { error = Just $ underflow (len+1) }
        _ -> env { error = Just $ underflow (-1) }
    EndSet ->
      case env.stacks, env.flow of
        this :| prev : stacks, { here: BuildingSet ptd more } : flows -> env
          { stacks = (more <> hfsFromFoldable this) : prev :| stacks
          , pointed = ptd
          , flow = flows
          }
        _, _ -> env { error = Just "Bad EndSet" }
    NEndSet ->
      case env.stacks, env.flow of
        this :| prev : stacks, { here: BuildingSet ptd more } : flows -> env
          { stacks = hfsFromInt (List.length this) : (more <> hfsFromFoldable this) : prev :| stacks
          , pointed = ptd
          , flow = flows
          }
        _, _ -> env { error = Just "Bad NEndSet" }
  -- And of course, the lowly literals (nullary operations)
  Lit l, _ -> onStack do stack0 l
  where
  env = original { instruction = original.instruction + 1 }

  -- For `Return`/`Continue`/`Break`: stop running control frames up to the
  -- most recent running `InFunCall`/`InWhile` control frame (which it can modify)
  returnTo matching = fromMaybe env do
    let
      matching' { prev: false } = Nothing
      matching' { here } = matching here
    guard env.running
    -- TODO: error if not found
    i <- List.findIndex (isJust <<< matching') env.flow
    modified <- matching' =<< env.flow List.!! i
    pure $ env
      { flow =
          map (_ { prev = false }) (List.take i env.flow) <>
          ({ prev: true, here: modified } : List.drop (i+1) env.flow)
      , running = false
      }

  underflow i = fold
    [ "Stack too small, present: "
    , show (List.length (NE.head env.stacks))
    , ", required: "
    , show i
    -- , ", on instruction "
    -- , showInstr instr
    ]

  -- Manipulate the stack.
  onStack f | { stacks: stack :| stacks } <- env =
    case unsafeCatch f stack of
      Left message -> env { error = Just message }
      Right stack' -> env { stacks = stack' :| stacks }
  stack0 val = Right <<< List.Cons val
  stack1 f (top : stack) = Right (f top : stack)
  stack1 _ _ = Left $ underflow 1
  stack2 f (y : x : stack) = Right (f x y : stack)
  stack2 _ _ = Left $ underflow 2
  -- Add a length-headed list
  stacking f (top : stack) =
    let r = f top in Right $
      NumLike Dec (fromInt (Array.length r)) : Array.toUnfoldable r <> stack
  stacking _ _ = Left $ underflow 1
  -- Pop a length-headed list
  stackN f (top : stack)
    | len <- toInt (bn top) =
      case List.length stack >= len of
        true -> f (List.take len stack) <#> \hd -> (hd : List.drop len stack)
        false -> Left $ underflow (len+1)
  stackN _ _ = Left $ underflow (-1)

  chooseOp = chooseIdOp >>> case _ of
    NoId f -> f
    ZeroId f -> f
    OneId f -> f
    Transitively f -> \x y -> if f x y then one else zero
    NonAssoc f -> \x y -> f [x, y]
  chooseOpFold o = case chooseIdOp o, _ of
    NoId _, Nil -> Left "No identity for 0-ary operation"
    ZeroId _, Nil -> Right zero
    OneId _, Nil -> Right one
    Transitively f, xs ->
      -- Reversed for good reason, to get the n-ary versions to match with the
      -- binary versions
      Right if transitively f (Array.reverse (Array.fromFoldable xs)) then one else zero
    NonAssoc f, xs -> Right (f (Array.fromFoldable xs))
    _, hd : tl -> Right (foldl1 (chooseOp o) (hd :| tl))

transitively :: forall a. (a -> a -> Boolean) -> Array a -> Boolean
transitively f [x, y] = f x y
transitively f xs = and (Array.dropEnd 1 xs `Array.zipWith f` Array.drop 1 xs)

-- | Catch runtime errors, e.g. from `BigInt` operations in JS.
unsafeCatch :: forall i o.
  (i -> Either String o) ->
  (i -> Either String o)
unsafeCatch f i = unsafePerformEffect do
  r <- try do
    pure unit
    pure (f i)
  pure case r of
    Left e -> Left ((message e))
    Right x -> x
