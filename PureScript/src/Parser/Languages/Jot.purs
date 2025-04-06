module Parser.Languages.Jot where

import Prelude

import Control.Monad.Free as Free
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Unsafe.Reference (unsafeRefEq)

{-
0b00000000111000000000111110000000001111100000001110000000001111100000000011111 Hex
0b010001010101100101010110111 Hex
-}

{-

“quapteryx”
quaternary combinators = quaternator

> By analogy with *byte* and *nybble*, a quaternary digit is sometimes called a *crumb*.

[] = I
[w0] = B[w] = [0302w]
[w1] = [w]I
[w2] = [w]K
[w3] = [w]S

01x = x
002xy = x
0003xyz = 00xz0yz
x,y,z ::= '1' | '2' | '3' | '0' x y

expecting = 1
while expecting:
  expecting += crumbstring[ptr++] ? -1 : 1

# WASM has popcount, clz and ctz (leading and trailing zeros)

const word_max = ~0;
const word_size;

const lower  = word_max / 0b11   # 0x55…
const upper  = lower * 0b10      # 0xAA…
const filler = word_max / 0b1111 # 0x11…
# ^ this is a filler pattern of 0q01, which is the identity combinator
# applied to, well, anything

# `crumbs`: a word worth of crumbs encoding SKI combinators and composition
# `nonzeros`: 0b01 for each nonzero crumb of `input=crumbs`
#   0b00 => 0b00
#   0b__ => 0b01
nonzeros(input=crumbs) = (w | (w >> 1)) & (word_max / 0b11)
# This is the delta of expected arguments over the word, as a signed int
# (not bad for 6 instructions!). -w_s/2 <= deltaExpecting <= +w_s/2
deltaExpecting = 2 * popcount(nonzeros) - (word_size / 2)

no zero crossing if
  expecting + clz > popcount
  wrong: expecting > deltaExpecting + ctz(crumbs)/2
    (each trailing zero either means that nonzeros
    started earlier or was less spread out)
otherwise:
  expecting <= popcount(nonzero) <= w_s/2
  need to check crumbs indices between
    2*(clz/2) + expecting and w_s/2 - ctz/2
  in general these bounds do not help much (clz, ctz <= 6), so might as well
  do the full binary search


# Zero if this crumb has at least n arguments, n = 1, 2, 3.
not1applied = nonzeros >> 2
not2applied = not1applied | nonzeros >> 4
not3applied = not2applied | nonzeros >> 6
# Test if each combinator is saturated
# (applied to at least its number of args)
satIs = not1applied | crumbs ^ (1 * lower)
satKs = not2applied | crumbs ^ (2 * lower)
satSs = not3applied | crumbs ^ (3 * lower)


# Each redex is tagged with its combinator
redexes = crumbs & 0b11*lower^(nonzeros(satIs) & nonzeros(satKs) & nonzeros(satSs)) # 23 ops
# Take the first redex
redexTgt = clz(redexes) / 2
redexType = redexes >> (word_size - 2 - 2*redexTgt)
redexStart = redexTgt - redexType


# Optimizations?
032 = SK = KI = 021
0003xy01z = 00xz0yz
0003xy002zk = 00xz0yz

# Optimize Bfgx = f(gx)
B = S(KS)K = 0030232
0000030232mno = 0m0no
S(Km)no = Kmo(no) = m(no)
000302mno = 0m0no

# Optimize Cfxy = fyx
C = ((S((S(K((S(KS))K)))S))(KK))
    00300302003023  2   3  022
0000030030200302323022mno = 00mon
00030030200302323022m = 0030203m2

   0003(00302 00302323) (022)(m)
  = 0 (0003(020030232)(3)(m))(0022m)
  = 0  00 02 (0030232)m   03m 0022m
  = 0  0      003(023)(2)(03m)0022m
  = 00002(3)(03m)0203m 0022m
  = 0030203m 0022m
  = 0030203m2

; C                        m
: 0003 0030200302323  022  m
= 0003(0030200302323)(022)(m)
> 00 (003 020030232  3)(m)   0(022)(m)
= 0 (0003(020030232)(3)(m))  (0022m)
> 0 (00(020030232)(m)0(3)(m))(0022m)
= 00 (002(0030232)(m))(03m)  (0022m)
> 00 0030232     (03m)       (0022m)
= 0 (0003(023)(2)(03m))      (0022m)
> 0 (00(023)(03m)0(2)(03m))  (0022m)
= 00 002(3)(03m) 0203m       (0022m)
> 00 3           0203m       (0022m)
= 0030203m(0022m)
> 0030203m2
; S(K(Sm))K = Cm

S(K(Sm))K = Cm
000030203m2no = 00mon

Sm(Kn)o = mo(Kno) = mon
0003m02no = 00mon

S(Km)(Kn) = K(mn)
00302m02n = 020mn

-}




{-
https://en.wikipedia.org/wiki/Iota_and_Jot
https://en.wikipedia.org/wiki/SKI_combinator_calculus#SKI_expressions
https://crypto.stanford.edu/~blynn/lambda/cl.html
https://crypto.stanford.edu/~blynn/lambda/crazyl.html
https://web.archive.org/web/20160823182917/http://semarch.linguistics.fas.nyu.edu/barker/Iota
https://www.angelfire.com/tx4/cus/combinator/birds.html
https://cs.stackexchange.com/questions/57361/combinator-equivalent-to-eta-conversion#57371
https://olydis.medium.com/one-point-bases-for-%CE%BB-calculus-4163b1b326ad

https://news.ycombinator.com/item?id=42375422
http://www.chriswarbo.net/blog/2024-05-10-sk_logic_in_egglog_4.html
https://okmij.org/ftp/tagless-final/ski.pdf
https://cs.stackexchange.com/questions/57476/what-functions-can-combinator-calculus-expressions-compute

ι = VSK = S(S(SKK)(KS))(KK)

-- Ojo is Jot with its bits reversed, and interpreted as a natural number
-- instead of as a bitstring.

-- Translation from Ojo to `SKI` combinator calculus
[] = I = SKx = SKK = SKS
[w1] = ([w]S)K = [w]SK = VSK[w] = ι[w] -- iota
[w0] = S(K[w]) = \yz. K[w]z(yz) = \yz. [w](yz) = B[w] -- composition

-- Two-bit translations
[w11] = Bιι[w] = ι(ι[w]) = ι[w]SK = [w]SKSK
[w01] = BιB[w] = ι(B[w]) = B[w]SK = [w](SK) = T(SK)[w]
[w10] = B(Bι)[w] = B(ι[w]) = B([w]SK) -- ugly!
-- Composition composes nicely, using composition
[w00] = (B∘B)[w] = BBB[w] = B(B[w]) = \mn. B[w](mn) = \mno. B[w](mn)o = \mno. [w](mno)
[w000] = (B∘B∘B)[w] = B(BBB)B[w] = B(BBB[w])) = B(B(B[w])) = \mnop. [w](mnop)

-- Recover the base `SKI` combinators via iterating iota
[w011] = B[w]SKSK = [w](SK)SK
[w00011] = BBB[w](SK)SK = B[w](SKS)K = B[w]IK = [w]K
[w00000111] = ι([w11]K) = ι(BBB[w]K) = BBB[w]KSK = [w](KSK) = [w]S
[w00101] = [w00000000011111] = [w]I
-- ^ except `(SK)(SK)` is much shorter for `I`

-- Could also derive `I` via `S` and `K`
[w00000001110001100011] = BBB[w]SKK = [w](SKK) = [w]I
[w00100011] = [w](SKK) = [w]I
-- `B` is also important
[w00000000011100111] = [w](IKSKIKSK) = [w](SIKSK) = [w](IS(KS)K) = [w]B
[w00000001110000111] = [w](S(KS)K) = [w]B

-- We can actually reduce some bitstrings of Ojo directly
[w000000000011111] = B[w]I = \m. [w](Im) = \m. [w]m = [w]
[w000000001110001100011] = B[w]I = [w]
[w000100011] = B[w]I = [w]
[w000101] = [w]
[w0000011100011] = [w]SK = [w1]
[w00010001011] = [w]K

-- To do more, we need to figure out how terms are encoded on the stack
-- Since '1' pushes two terms and '0' decreases stack size by 1, then
-- <P(n)> must satisfy the equation `n = 2 * #1s - #0s` and furthermore the
-- stack must not drop to zero (e.g. `001` cannot be a trailing string).

-- Push 1 onto the stack
-- `^0(1|00(?1)(?1)|0(?1)0(?1))$`
<P1> = '0' <P2>
-- Push 2 onto the stack (ambiguous grammar...?)
-- `^(1|00(?1)(?1)|0(?1)0(?1))$`
<P2> = '1' | '0' '0' <P2> <P2> | '0' <P2> '0' <P2>

fst :: <P2> -> <P1>
fst '1' = '00000111' -- S
fst ('0' <fa:P2> '0' <gb:P2>) = '0' <fa:P2>
fst ('0' '0' <fa:P2> <bz:P2>) = '0' '0' <fa:P2> (fst <bz:P2>)

snd :: <P2> -> <P1>
snd '1' = '00011' -- K
snd ('0' <fa:P2> '0' <gb:P2>) = '0' <gb:P2>
snd ('0' '0' <fa:P2> <bz:P2>) = snd <bz:P2>


-- So now we can reduce `K` = fst
[w0000011] = BBB[w]K = \mn. [w](Kmn) = \mn. [w]m
[w0000011<m:P1><n:P1>] = [w<m:P1>]
-- Split iota into `S K` and drop the `K`
-- [w00000111] = BBB[w]KSK = [w](KSK) = [w]S = [w00000111] -- oh.
-- Split iota into `S K` after `<m:P1>` and drop the `S`
[w0000011<m:P1>1] = [w<m:P1>]K = [w<m:P1>00011]
-- Split the annoying <P2> = 00<P2><P2> case
[w000001100<fa:P2><bz:P2>] = [w00<fa:P2>0000011<bz:P2>]

-- And we can reduce `KI` = `SK` = snd, which drops one from the stack
[w](KI) = B[w]KI = [w0]KI = [w000011]I = [w00001100101] = [w](SK) = [w01]
[w001<m:P1>] = [w]I = [w00101] -- not necessarily a reduction
[w0001<mn:P2>] = [w]n = [w<snd mn:P1>] -- not necessarily a reduction
-- Split iota into `S K` and drop the `S`
-- [w00011] = BBB[w](SK)SK = [w](SKSK) = [w]K = [w00011] -- oh.
-- [w0011] = B[w](SK)SK = [w](SKS)K = [w]IK = [w0010100011] -- <P2> -> <P1><P1>
-- Split the annoying <P2> = 00<P2><P2> case
[w00100<fa:P2><bz:P2>] = [w001<bz:P2>]

-- And `S`, I guess
[w00000000011111] = [w](SSKSK) = [w](SKS) = [w]I = [w00101]
[w000000000011111] = [w]
[w00000000111<mn:P2><o:P1>]
  = [w](Smno) = [w]((mo)(no))
  = [w00<fst mn:P1><o:P1>0<snd mn:P1><o:P1>]
[w00000000111<m:P1><no:P2>]
  = [w00<m:P1><snd no:P1>0<no:P2>]
[w00000000111<mn:P2><op:P2>]
  = [w](Smno)p = [w]((mo)(no))p
  = [w00<fst mn:P1><fst op:P1>0<snd mn:P1><op:P2>]

-- Derive function application within Ojo
[X] = Ix… = x <-> {x} = X
[Y] = Iy… = y <-> {y} = Y
[X 0*m Y] = B^m(Ix…)y… = xy <-> {xy} = X 0*m Y
-- We can determine what `m` is by `(2 * #1s - #0s) - 1`: the number of `0`s
-- we need to insert so that `0*m Y` results in a single item on the stack.
-- That is, ordinarily we would want to work with <Y:P1>, but because we drop
-- leading `0`s when converting from a bitstring to a natural number, it is
-- instead <Y:P(m+1)>, so then <0*m Y:P1> like we want.

[1] = ISK = SK = KI = \mn. n = False
[11] = ISKSK = K = \mn. m = True
[111] = ISKSKSK = S = \mno. mo(no)
[1111] = ISKSKSKSK = SSK = WC = \mn.mnm
[11111] = ISKSKSKSKSK = SKS = I = []
[111111] = ISKSKSKSKSKSK = SK = [1]

[] = I
[0] = BI = I
[00] = B(BI) = I
[000] = B(B(BI)) = I

[10] = B(SK) = S(K(SK)) = \mno. o
[100] = B(B(SK)) = S(K(S(K(SK)))) = \mnop. p
...
[110] = B(SKSK) = BK = S(KK) = \mnop. mnp
[1100] = B(B(K)) = S(K(S(KK))) = \mno. mno
[11000] = B(B(B(K))) = S(K(S(K(S(KK))))) = \mnopq. mnop
[1110] = BS = S(KS) = \ponm. pom(nm)
[11100] = B(BS) = S(K(S(KS))) = \qponm. qpom(nm)
[11110] = B(SSK) = S(K(SSK)) = \mno. mno(mn)
[111100] = B(B(SSK)) = S(K(S(K(SSK)))) = \mnop. mnop(mno)

[10001] = [1000]SK = B(B(B(SK)))SK = \mno. o
[100001] = B(B(B(B(SK))))SK = \mnop. p

[110001] = B(B(B(SKSK)))SK = B(B(SKSK))(SK) = S(K(S(KK)))(SK) = \mno. n = KK
[1100001] = B(B(B(B(SKSK))))SK = S(K(S(K(S(KK)))))(SK) = \mnop. no
[11000001] = B(B(B(B(B(SKSK)))))SK = \mnopq. nop
[w11000001] = B(B(B(B(B([w]SKSK))))SK = \mnopq. [w]SKSK(nop)q

[11000011] = BKK = S(KK)K
[11000011111111] = I

w([110001])
   11 -> <00011:P1>
     00 -> (B)(B)_
       01 -> 0_01
[w00000000011100001110000000011100001110001101]
[w$$(B              )$(B              )(ιιI) F]
[w00000000011100001110000000011100001110001101]
[w$$S         (KS)K   B                K     F]

[w00000011100101] = wO = w(SI)
[w00000001110010100101] = wM = w(SII)

Array(3) [ "00000000000111001110000001110011", "w(S(K(S(SKK)))K)", "wT" ]
-}

data ComCalc c = Com c | Ap (ComCalc c) (ComCalc c)
infixl 6 Ap as :@
instance eqComCalc :: Eq c => Eq (ComCalc c) where
  eq x y | unsafeRefEq x y = true
  eq (Com x) (Com y) = x == y
  eq (f :@ x) (g :@ y) = f == g && x == y
  eq _ _ = false
instance ordComCalc :: Ord c => Ord (ComCalc c) where
  compare x y | unsafeRefEq x y = EQ
  compare (Com x) (Com y) = compare x y
  compare (f :@ x) (g :@ y) = case compare f g of
    EQ -> compare x y
    areNotEq -> areNotEq
  compare (Com _) (Ap _ _) = LT
  compare (Ap _ _) (Com _) = GT

reduce :: ComCalc String -> Maybe (ComCalc String)
reduce (Com "S" :@ x :@ y :@ z) = Just $ x :@ z :@ (y :@ z)
reduce (Com "K" :@ x :@ _)      = Just x
-- Bonus combinators.
reduce (Com "B" :@ x :@ y :@ z) = Just $ x :@ (y :@ z)
reduce (Com "C" :@ x :@ y :@ z) = Just $ x :@ z :@ y
reduce (Com "R" :@ x :@ y :@ z) = Just $ y :@ z :@ x
reduce (Com "I" :@ x)           = Just x
reduce (Com "T" :@ x :@ y)      = Just $ y :@ x
-- Improper combinators
reduce (Com "ι" :@ x)           = Just $ x :@ Com "S" :@ Com "K"
reduce _ = Nothing

-- step :: ComCalc String -> ComCalc String
-- step (f :@ z) = maybe (step f :@ z) identity $ reduce (f :@ z)
-- step t = t

isNormalized :: forall c. (ComCalc c -> Maybe (ComCalc c)) -> ComCalc c -> Boolean
isNormalized reducer =
  let { norm } = evalComCalc { reducer, depth: Just 1 } in
  unsafeRefEq <*> norm

isWHNF :: forall c. (ComCalc c -> Maybe (ComCalc c)) -> ComCalc c -> Boolean
isWHNF reducer =
  let { whnf } = evalComCalc { reducer, depth: Just 1 } in
  unsafeRefEq <*> whnf

evalSKI ::
  { norm :: ComCalc String -> ComCalc String
  , normSteps :: ComCalc String -> NonEmptyList (ComCalc String)
  , normTo :: Maybe Int -> ComCalc String -> ComCalc String
  , normStepsTo :: Maybe Int -> ComCalc String -> NonEmptyList (ComCalc String)
  , whnf :: ComCalc String -> ComCalc String
  , whnfSteps :: ComCalc String -> NonEmptyList (ComCalc String)
  , whnfTo :: Maybe Int -> ComCalc String -> ComCalc String
  , whnfStepsTo :: Maybe Int -> ComCalc String -> NonEmptyList (ComCalc String)
  }
evalSKI = let full = evalComCalc { reducer: reduce, depth: Nothing } in
  { norm: full.norm
  , normSteps: full.normSteps
  , whnf: full.whnf
  , whnfSteps: full.whnfSteps
  , normTo: \depth -> (evalComCalc { reducer: reduce, depth }).norm
  , normStepsTo: \depth -> (evalComCalc { reducer: reduce, depth }).normSteps
  , whnfTo: \depth -> (evalComCalc { reducer: reduce, depth }).whnf
  , whnfStepsTo: \depth -> (evalComCalc { reducer: reduce, depth }).whnfSteps
  }

type Depthed = Free.Free ((->) (Maybe Int))

evalComCalc :: forall c.
  { reducer :: ComCalc c -> Maybe (ComCalc c)
  , depth :: Maybe Int
  } ->
  { whnf :: ComCalc c -> ComCalc c
  , norm :: ComCalc c -> ComCalc c
  , whnfSteps :: ComCalc c -> NonEmptyList (ComCalc c)
  , normSteps :: ComCalc c -> NonEmptyList (ComCalc c)
  }
evalComCalc { reducer, depth: depth0 } =
  { whnf: run <<< whnf
  , norm: run <<< norm
  , whnfSteps: run <<< whnfSteps
  , normSteps: run <<< normSteps
  }
  where
  run :: forall v. Depthed v -> v
  run = Free.runFree (_ $ depth0)

  checkDepth :: forall v. (v -> Depthed v) -> v -> Depthed v
  checkDepth f v = join $ Free.liftF case _ of
    Just d | d <= 0 -> pure v
    _ -> Free.hoistFree (\g d -> g (map (_ - 1) d)) (f v)

  checkDepthSteps :: forall v. (v -> Depthed (NonEmptyList v)) -> v -> Depthed (NonEmptyList v)
  checkDepthSteps f v = join $ Free.liftF case _ of
    Just d | d <= 0 -> pure (NEL.singleton v)
    _ -> Free.hoistFree (\g d -> g (map (_ - 1) d)) (f v)

  whnf :: ComCalc c -> Depthed (ComCalc c)
  whnf t@(f :@ z) = do
    t' <- checkDepth whnf f <#> case _ of
      f' | unsafeRefEq f f' -> t
      f' -> f' :@ z
    case reducer t' of
      Nothing -> pure t'
      Just t'' -> checkDepth whnf t''
  whnf t = pure t

  norm :: ComCalc c -> Depthed (ComCalc c)
  norm t =
    whnf t >>= case _ of
      t'@(x :@ y) -> ado
        x' <- checkDepth norm x
        y' <- checkDepth norm y
        in if unsafeRefEq x x' && unsafeRefEq y y'
          then t' else x' :@ y'
      t' -> pure t'

  ext :: forall f v. Functor f => (v -> f (NonEmptyList v)) -> NonEmptyList v -> f (NonEmptyList v)
  ext f = NEL.uncons >>> \{ head, tail } -> NEL.appendFoldable <$> (f head) <@> tail

  whnfSteps :: ComCalc c -> Depthed (NonEmptyList (ComCalc c))
  whnfSteps t@(f :@ z) = do
    t's <- checkDepthSteps whnfSteps f <#> map case _ of
      f' | unsafeRefEq f f' -> t
      f' -> f' :@ z
    t's # ext \t' -> case reducer t' of
      Nothing -> pure (NEL.singleton t')
      Just t'' -> checkDepthSteps whnfSteps t''
  whnfSteps t = pure (NEL.singleton t)

  normSteps :: ComCalc c -> Depthed (NonEmptyList (ComCalc c))
  normSteps t =
    whnfSteps t >>= ext case _ of
      t'@(x :@ y) -> ado
        let remake x' y' = if unsafeRefEq x x' && unsafeRefEq y y' then t' else x' :@ y'
        { head: x', tail: x's } <- NEL.uncons <$> checkDepthSteps normSteps x
        y's <- checkDepthSteps normSteps y
        in (remake x' <$> y's) `NEL.appendFoldable` (remake <$> x's <@> y)
      t' -> pure (NEL.singleton t')

{- Scratch pad (using types to figure out combinators) -}

-- b = (<<<) :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)

-- x :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
-- x = b b b

-- y :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
-- y f g a b = f (g a b)

-- m :: forall a b c d. (d -> b) -> (c -> d) -> (a -> c) -> a -> b
-- m = b b b b b

-- n :: forall a b c d. (d -> b) -> (c -> d) -> (a -> c) -> a -> b
-- n f g h a = f (g (h a))


-- p :: forall a b c d e. (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
-- p = (<<<) <<< (<<<) <<< (<<<)

-- q :: forall a b c d e. (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
-- q f g a b c = f (g a b c)

-- r :: forall a b c d e. (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
-- r = b (b b b) b

-- s :: forall a b c d r z. (r -> z) -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> z
-- s = b (b (b b b) b) b

{-
Scrapped:

I am going to use an encoding better optimized for bignats:
- every bitstring is inverted and prefixed with 1

As Chris Barker says,

> For one thing, the 1 operator does not treat 0 as a unit, but is capable of
> dividing the meaning of 0 into its K half and its S half. As a result, there
> are typically more 1's than 0's in a useful Jot program

So inverting this and having more 0's will result in a smaller number.
And it is a nice mneumonic: 1 = I and 1 = ι, and 0 = B = (∘) (composition).

-- Translation from Croak to `SKI` combinator calculus
[1] = I = SKx = SKK = SKS
[w1] = ([w]S)K = [w]SK = VSK[w] = ι[w] -- iota
[w0] = S(K[w]) = \yz. K[w]z(yz) = \yz. [w](yz) = B[w] -- composition

-- Derive function application within Croak
[1X] = Ix = x <-> {x} = 1X
[1Y] = Iy = y <-> {y} = 1Y
[1XY] = Ixy = xy <-> {xy} = 1XY
-}
