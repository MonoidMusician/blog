```tmttmt
%title("Judgments from the Dhall standard")

%defaults({ comments={ syntax="md" } })
(# remove the need for the `md:` prefix in comments #)

{@collapsed (# Display this section collapsed by default #)}
%section("Basics") (# Basics needed for tmTTmt stuffs #)

{# True or False #}
%type(Bool)("TT" (@name "true") | "FF" (@name "false")) (@enum) (@deriving Eq Ord Enum)
{# The result of comparing within a total order #}
%type(ComparedTotal)("LT" | "EQ" | "GT") (@enum) (@deriving Eq Ord Enum)
{# A comparison to perform within a total order #}
%type(ComparisonTotal)("LT" | "LE" | "EQ" | "GE" | "GT") (@enum) (@deriving Eq Ord Enum)

%typeclass(Eq t (: Type :)){{{
  {# Check if two values are equal #}
  eq:: t -> t -> Bool
  {# Default implementation if `Ord t` is implemented #}
  {@default (Ord t)}
  eq: x y => are_eq:
    compares "EQ" x y => are_eq
    (# in turn it falls back to `compare x y => "EQ"` #)
}}}

{# An ADT representation for natural numbers. #}
%type(Nat)([] (@name "zero") | [Nat] (@name "succ"))
{# With pattern synonyms for convenience. #}
%pattern(Nat)("0")([])
%pattern(Nat)(["1+" n])([n])
%pattern(Nat)([n "+1"])([n])
%pattern(Nat)("1")(["1+" "0"])

{# An ADT representation for integers. #}
%type(Int)([@name:"nonneg" Nat] | [@name:"neg" Nat])
%pattern(Int)("0")(["nonneg" "0"])
%pattern(Int)("+1")(["nonneg" "1"])
%pattern(Int)("-1")(["neg" "0"])

pos1 := ["nonneg" [[]]] (: Int :)
neg1 := ["neg" []] (: Int :)

%typeclass(Ord t (: Type :)){{{
  compare:: t -> t -> ComparedTotal (# "LT" | "EQ" | "GT" #)
  compares::
    ComparisonTotal (# "LT" | "LE" | "EQ" | "GE" | "GT" #) ->
    t -> t -> Bool

  {# Default implementation for `compares` based on `compare`. #}
  @default:
  compares: "LE" x y => r:
    compare x y
      ? "GT": "FF" => r
      ? _:    "TT" => r !
  @default:
  compares: "GE" x y => r:
    compare x y
      ? "LT": "FF" => r
      ? _:    "TT" => r !
  @default:
  compares: "EQ" x y => r:
    @not-default: eq x y => r
    (# make sure there is no circular dependency #)
  @default:
  compares: cmp x y => r:
    {# note that this compares across types #}
    {# (assisted by being tagged `@enum`) #}
    compare x y
      ? cmp:  "TT" => r
      ? _:    "FF" => r !
}}}

{# Applies the integer delta if the result is still nonnegative. #}
apply_delta:: Int -> Nat -> ?Nat
apply_delta: ["neg" []] [y] => [y];
(# Success, subtracted the right amount #)
apply_delta: ["neg" [x]] [y] => [z]:
  apply_delta ["neg" x] y =>? [z]
  (# Subtract from x and y, continue #)
apply_delta: ["nonneg" []] y => [y];
(# Success, added the right amount #)
apply_delta: ["nonneg" [x]] y => [[z]]:
  {# Keep adding #}
  apply_delta ["nonneg" x] y =>? [z]
  (# Okay this is actually never going to fail #)
apply_delta: _ _ => [];
(# Failed, too much subtracted #)


%section("Expressions")

%type(Name)(String)
%type(Index)(Nat)

%type(Variable)(
  ["Variable" Name Index]
)
%pattern(Variable)(["Var" name idx])(["Variable" name idx])
%pattern(Variable)(["V"   name idx])(["Variable" name idx])
%type(ExprF t)(
  | Bool | Variable (@inherit-patterns)
  | ["Lambda" Name t (@index "type") t (@index "body")]
  | ["Forall" Name t (@index "type") t (@index "body")]
  | ["Let" Name ?t (@index "type") t (@index "value") t (@index "body")]
  | ["If" t (@index "condition") t (@index "if-T") t (@index "if-F")]
  | ["Annotation" t (@index "value") t (@index "type")]
  | ["Application" t (@index "function") t (@index "argument")]
  | ["Operator" Operator t (@index "L") t (@index "R")]
  | "Bool" | "Type" | "Kind" | "Sort"
) (@deriving TraversableWithIndex
    (# Derives everything from `Functor` to `TraversableWithIndex` #))
%newtype(Expr)(ExprF Expr)



%section("Shift")
%link("https://github.com/dhall-lang/dhall-lang/blob/master/standard/shift.md")

{#
  Shift a variable in an expression.

  `shift delta ["Var" name min]` shifts all free variables with the same name
  and index at least `min` by the amount `delta`
#}
shift:: Int -> Variable (# Name, Index #) -> Expr -> Expr

{# The main case is the variable itself #}
shift: delta ["Var" name2shift min]
  ["Variable" name2shift index#0]
  => ["Variable" name2shift index#1]:

  {# Check if `index#0 <= min` #}
  compares "LE" min index#0 =>? "TT"
  (# (This is like a guard: if it fails, the next cases are tried.) #)

  {# And if so, apply the delta #}
  apply_delta delta index#0 => [index#1]
  (# (If this fails, then `shift` fails with a pattern match error.) #)
shift: _ _ ["Variable" name index] => ["Variable" name index]; (# Otherwise preserve it #)

{# Two judgements for `Lambda`, for if it matches or not #}
shift: delta ["Var" name2shift min]
  ["Lambda" name2shift type#0 body#0]
  => ["Lambda" name2shift type#1 body#1]:

  {# Nothing more is bound yet: apply it normally #}
  shift delta ["Var" name2shift min] type#0 => type#1
  {# Increment `min` now that a new `name2shift` will be bound in `body##` #}
  shift delta ["Var" name2shift ["1+" min]] body#0 => body#1
shift: delta ["Var" name2shift min]
  ["Lambda" name2bind type#0 body#0]
  => ["Lambda" name2bind type#1 body#1]:

  shift delta ["Var" name2shift min] type#0 => type#1
  {# A different variable is bound, so it is fine #}
  shift delta ["Var" name2shift min] body#0 => body#1

{# Or we can write it in a single case #}
shift: delta ["Var" name2shift min#0]
  ["Forall" name2bind type#0 body#0]
  => ["Forall" name2bind type#1 body#1]:

  shift delta ["Var" name2shift min#0] type#0 => type#1
  {# Increment `min#1 = min#0 + 1` if the bound name is the target name #}
  name2bind
    ? name2shift: ["1+" min#0] => min#1
    ? _:                min#0  => min#1 !
  shift delta ["Var" name2shift min#1] body#0 => body#1

shift: delta ["Var" name2shift min#0]
  ["Let" name2bind opt_type#0 value#0 body#0]
  => ["Let" name2bind opt_type#1 value#1 body#1]:
    map (\ type => shift delta name2bind min#0 type) opt_type#0 => opt_type#1
    shift delta ["Var" name2bind min#0] value#0 => value#1
    incr_if_eq min#0 name2shift name2bind => min#1
    shift delta ["Var" name2bind min#1] body#0 => body#1

{# Helper used above #}
incr_if_eq:: %forall(t)( %constr(Eq t)(Nat -> t -> t -> Nat) )
incr_if_eq: n t t => [n]; (# The successor of `n`, i.e. `n+1` #)
incr_if_eq: n _ _ => n;   (# No change #)

{#
  Finally, if we are not dealing with variables or binders,
  then we recurse through the structure:
#}
shift: delta ["Var" name2shift min] other#0 => other#1:
  {# Unwrap `Expr` into `ExprF Expr` #}
  unlayer other#0 => layer#0 (: ExprF Expr :)
  {# Map over the structure and recurse #}
  map (\ child => shift delta ["Var" name2bind min] child) layer#0 => layer#1
  {# Wrap it back up #}
  relayer layer#1 => other#1 (: Expr :)

%section("Substitute")
%link("https://github.com/dhall-lang/dhall-lang/blob/master/standard/substitution.md")

{# We can handle all binders with a specialized `map` operation: #}
map_with_binder:: (?Name -> i -> o) -> ExprF i -> ExprF o
map_with_binder: f ["Lambda" name type#0 body#0] => ["Lambda" name type#1 body#1]:
  f [] type#0 => type#1
  f [name] body#0 => body#1
map_with_binder: f ["Forall" name type#0 body#0] => ["Forall" name type#1 body#1]:
  f [] type#0 => type#1
  f [name] body#0 => body#1
map_with_binder: f ["Let" name opt_type#0 value#0 body#0] => ["Forall" name opt_type#1 value#1 body#1]:
  map ($f []) opt_type#0 => opt_type#1
  f [] value#0 => value#1
  f [name] body#0 => body#1
map_with_binder: f layer#0 => layer#1:
  map ($f []) layer#0 => layer#1

{# And use that to implement direct substitution: #}
subst:: Variable (# Name, Index #) -> Expr -> Expr -> Expr
subst: ["Var" name2sub idx] value ["Variable" name2sub idx] => value;
subst: ["Var" name2sub idx] value#0 expr#0 => expr#1:
  unlayer expr#0 => layer#1
  map_with_binder (|
    {# Direct recursion, if nothing is bound #}
    | [] child#0 => child#1:
      subst ["Var" name2sub idx] value#0 child#0 => child#1
    {# When we bind a variable #}
    | [name2bind] child#0 => child#1:
      {# We need to shift the value to bind, so that it
        references the same variables in outer scopes #}
      shift pos1(# +1 #) ["Var" name2bind "0"] value#0 => value#1
      {# If it is the same name, then the variable
        index to substitute also needs to increase #}
      incr_if_eq idx name2sub name2bind => idx#1
      subst ["Var" name2sub idx#1] value#1 child#0 => child#1
  |) layer#0 => layer#1
  relayer layer#1 => expr#1

{# Helper for substitution with the correct shifts #}
shift_subst_shift: ["Var" name2sub idx] value#0 expr## => expr##:
  {# Make sure `["Variable" name2sub idx]` does not occur in `value##`
    by shifting everything up that is not below it #}
  shift pos1(# +1 #) ["Var" name2sub idx] value#0 => value#1
  {# Substitute this shifted expression in `expr##` #}
  subst name2sub idx value#1 expr## => expr##
  {# Reverse the shifting now that `["Variable" name2sub idx]`
    does not occur in `expr##` #}
  shift neg1(# -1 #) ["Var" name2sub idx] expr## => expr##

{#purescript:
shiftSubstShift :: forall m a. Var -> Expr m a -> Expr m a -> Expr m a
shiftSubstShift v a b = shift (-1) v (subst v (shift 1 v a) b)

shiftSubstShift0 :: forall m a. String ->  Expr m a -> Expr m a -> Expr m a
shiftSubstShift0 v = shiftSubstShift $ AST.V v 0
#}
```
