---
title: "tmTTmt Syntax"
---

## Overview

- Top-level structure
  - `name:: type`{.tmttmt} type signature, like `name :: Type`{.haskell} in Haskell
  - `name: inputs… => output;`{.tmttmt} function case, like `name inputs… = output`{.haskell}
    - `name: inputs… => output: stmts…`{.tmttmt} for a function body
  - `#pragma(arg)(u)(ments)`{.tmttmt}

- General structure / design philosophy:
  - `[…]`{.tmttmt} for vectors
  - `{…}`{.tmttmt} for hashes/records
    - `{ "K"=V … }` for hashes, which allow implicit weakening
      - [e.g.]{t=} `{ "type"="error" "error"=$$ } | { "type"="result" "value"=$ }`{.tmttmt}
    - `{. "K"=V … .}` for records, which allow matching on the exact field names
      - [e.g.]{t=} `{. "error"=$$ .} | {. "result"=$ .}`{.tmttmt}
  - `{{…}}`{.tmttmt} for other block structure
    - `{{# … #}}`{.tmttmt} nested comments
    - `{{@annotation}}`{.tmttmt}
    - `{{: … :}}`{.tmttmt} type/kind annotation
    - `{{! … }}`{.tmttmt} function block
    - `{{{ … }}}`{.tmttmt} module block
  - `(…)`{.tmttmt} for parenthetical or grouped structure
    - `(# … #)`{.tmttmt} nested comments
    - `(@annotation)`{.tmttmt}
    - `(: … :)`{.tmttmt} type/kind annotation

    - `(| … )`{.tmttmt}, `(! … )`{.tmttmt}, `(\ … )`{.tmttmt} lambdas (see below)
    - `(? … )`{.tmttmt} type(d) hole

    - future: `(= …)` for infix operator expressions, possibly [distfix](https://dl.acm.org/doi/pdf/10.1145/5657.5659) too

- Comments
  - Leading comments: `{{# look at this: #}} "this"`{.tmttmt}
  - Trailing comments: `"thing" (# <- this thing does this #)`{.tmttmt}
  - Line comments: `## comment`{.tmttmt}, `#!tmttmt`{.tmttmt}

- Functions
  - Lambda macro: `(!2 append #1 #0 !)`{.tmttmt}, `(! thunked)`{.tmttmt}, `(!! double-thunked)`{.tmttmt}
  - Lambda case:
    ```tmttmt
    (
    | in1 => out1:
      helper => intermediate
    | in2 => out2;
    )
    ```
  - Lambda result:
    ```tmttmt
    (\ "0" i1 => f a b c i1 | "1" i2 => g x y z i2)
    ```
    short for
    ```tmttmt
    (
    | "0" i1 => r:
      f a b c i1 => r
    | "1" i2 => r:
      f a b c i2
    )
    ```
  - Block of statements:
    ```tmttmt
    {{!
      f a b c #0 => r1
      g x y z #1 => r2
    !}}
    ```
    short for
    ```tmttmt
    (
    | i1 i2 => []:
      f a b c i1 => r1
      g x y z i2 => r2
    )
    ```

- Variables
  - Unifying variable: `x`{.tmttmt}
  - Shadowable variable: `x#`{.tmttmt}
  - State variable: `x#1`{.tmttmt}, `x##`{.tmttmt} (auto incrementing)

## Grammar

```bnf
## Fundamentals

name = /(?![0-9])[-a-zA-Z0-9_]+/
qual = name | qual'.'name
string = '"' strchar* '"'  ## string literal
strchar = /[^\"]+|\\./

int = /\d+/

## Main syntactic categories

structures(aspect) =
  | '[' aspect* ']'                 ## vector literal
  | '{'  (string '=' aspect)*  '}'  ## hash literal
  | '{.' (string '=' aspect)* '.}'  ## enumerated record literal


## Term syntax
## (It is a subset of balanced syntax,
## for modularity of parsing)
tm (< any_balanced_stringly) =
  | name('#'('#'|/\d+/))?      ## variable
  | qual'.'name               ## qualified name
  | '#'int                     ## template variable
  | string                     ## string literal

  | structures(tm)             ## vector/hash/record literals

  | tyannotated(tm)            ## term with a type or comment or custom annotation

  | '(' '$' qual tm* '$'? ')'  ## macro expansion thingy

  | '(?' tm* '?'? ')'          ## typed hole
  | '(?:' ty* ':'? '?'? ')'    ## hole with type

  | lambda

lambda =
  ## absurd :: Void -> forall a. a
  | '?!'
  ## full lambda syntax
  | '('
      ('|' casedecl)+
      '!'?
    ')'
  ## positional hole syntax, e.g.
  ## (!2 append #1 #0 !)
  | '(' ('!'{1,4} | '!'int) tm+ '!'? ')'
  ## Haskell-like syntax
  | '(' '\\' tm+ arrow tm+ ('|' tm+ arrow tm+)* ('!' | '/')? ')'
  ## statement block
  | '{{' ('!'{1,4} | '!'int) stmt* ('==>' tm)? '!'? '}}'

## top-level declaration of a case of a function
casedecl =
  ## a case declaration
  | qual':'             ## function we are defining
      tm+                ## arguments
      arrow
      tm                 ## return value
    ( ';' | ':' stmt+ )  ## body of statements
  ## type annotation of the (whole / remaining) function
  | '{{' ':' ty ':'? '}}' casedecl
  ## comment for the function case
  | '{{#' any_balanced_comment '#}}' casedecl
  ## annotation for the function case
  | '{{' '@' annotation '}}' casedecl
  | '@' annotation_name casedecl

## top-level type signature
typesig =
  | qual '::'  ty

arrow =
  ## fall-through call (tries the next case if it fails)
  | '?' '=>'
  ## irrevocable call
  | '=>'
  ## annotate the call specifically
  | annotated(arrow)

## Statement syntax (the bodies of functions)
stmt =
  ## normal call (applicative/monad style)
  | '?'? tm+ arrow tm
  ## case call (selective applicative style)
  | '?'? tm+ '?'? select_case* '!'
  | '{{' '@' annotation '}}' stmt  ## duplicates the term annotation for the stmt
  | '@' annotation_name stmt       ## duplicates the term annotation for the stmt
  ## top-level declarations (function-local module scope)
  | '{{{' top_level '}}}'

select_case =
  | '?' tm ';'
  | '?' tm ':' stmt*

## Type syntax
ty (< any_balanced_stringly) =
  | structures(ty)     ## vector/hash/record literals
  | '+' ty             ## non-empty list
  | '*' ty             ## possibly-empty list, sugar for `'+' ty '|' '[' ']'`
  | '?' ty             ## optional, sugar for `'[' ty ']' '|' '[' ']'`
  | ty '|' ty          ## union (may also have leading bars, or trailing bars, if within parentheses)
  | '$$'               ## non-empty strings
  | '$'                ## strings, sugar for `'$$' '|' '"' '"'`
  | string             ## string singleton
  | ty '->' ty         ## function type
  | name               ## type variable
  | quantified(ty)     ## quantifiers (#forall, #exists)
  | '(' ty+ ')'        ## type application (maybe?)
  | '(?' ty* '?'? ')'  ## type hole
  | '(' '|' ')'        ## empty type (Void/never)
  | tyannotated(ty)    ## annotations

## Pragmas are not technically allowed to stand for types (yet),
## but we can at least special case it, since the syntax is
## reserved and could work like this anyways
quantified(aspect) =
  | '#'('forall'|'exists')
    '(' tyannotated(name)* ')'  ## variables, annotated with kinds
    '(' aspect ')'
  | ('∀'|'∃')
    tyannotated(name)*
    ':'
    aspect

pragma =
  | '#'name
    ('(' (tm* | ty* | stmt*) ')')*
  | tyannotated(pragma)

top_level =
  | (pragma | casedecl | typesig)*

## Helpers

annotation_name = name (':' name)*
annotation = annotation_name annotation_term*
## terms used in custom annotations, so everything is parsed properly
annotation_term (< any_balanced_stringly) =
  ## bare string
  | string
  ## data structures / non leaf nodes
  | structures(annotation_term)
  ## labeled syntactic categories
  | '('  'name' ':' name  ')'
  | '('  'stmt' ':' stmt  ')'
  | '('  'tm'   ':' tm    ')'
  | '('  'ty'   ':' ty    ')'
  | '(' '*name' ':' name* ')'
  | '(' '*stmt' ':' stmt* ')'
  | '(' '*tm'   ':' tm*   ')'
  | '(' '*ty'   ':' ty*   ')'
  | '(' '+name' ':' name+ ')'
  | '(' '+stmt' ':' stmt+ ')'
  | '(' '+tm'   ':' tm+   ')'
  | '(' '+ty'   ':' ty+   ')'


## Annotation or type annotation
tyannotated(aspect) =
  | aspect '(' ':' ty ('::' ty)? ':'? ')'    ## suffix type annotation
  | '{{' ':' ty ':'? '}}' aspect             ## prefix type annotation
  | annotated(aspect)
## Annotation or comment
annotated(aspect) (< any_balanced_stringly) =
  | aspect '(#' any_balanced_comment* '#)'    ## suffix comment
  | '{{#' any_balanced_comment* '#}}' aspect  ## prefix comment
  | aspect '(' '@' annotation ')'             ## suffix parameterized annotation
  | '{{' '@' annotation '}}' aspect           ## prefix parameterized annotation
  | '@' annotation_name aspect                ## prefix simple annotation


any_balanced_comment =
  | '(#' any_balanced_comment '#)'
  | '{{#' any_balanced_comment '#}}'
  | /./  ## any single character
  | any_balanced_comment*

any_balanced_stringly (< any_balanced_comment) =
  | '(#' any_balanced_comment '#)'
  | '{{#' any_balanced_comment '#}}'
  | '(' any_balanced_stringly ')'
  | '[' any_balanced_stringly ']'
  | '{' any_balanced_stringly '}'
  | string
  | /./  ## any single character
  | any_balanced_stringly*
```

## Examples

```tmttmt
## unifying variable
normalize: ["if" cond "then" result "else" result] => result;
## desugars to:
normalize: ["if" cond "then" result#1 "else" result#2] => result#0:
  eq result#1 result#2 => result#0

build-options:: { "X"=boolean "Y"=("built-in" | ["provided" $$]) "args"=*$ } -> *$
build-options: { "X"=X "Y"=Y "args"=args } => options##:
  {{# start with empty options #}}
  [] => options##
  X ? "true":
    append2 options## [ "--use-X" ] => options##
  ? _; !

  Y
  ? "built-in":
    append2 options## [ "--built-in-Y" ] => options##
  ? ["provided" path]:
    append2 options## [ "--path-to-Y" path ] => options##
  !

  args
  ? [];
  ? _:
    append3 options## [ "--" ] args => options##
  !

build-options-with-monad: { "X"=X "Y"=Y "args"=args } => options:
  run-writer-monad (| [] => []:
    X ? "true":
      appending1 [ "--use-X" ] => []
    ? _; !

    Y
    ? "built-in":
      appending1 [ "--built-in-Y" ] => []
    ? ["provided" path]:
      appending1 [ "--path-to-Y" path ] => []
    !

    args
    ? [];
    ? _:
      appending2 [ "--" ] args => []
    !
  |) => [options []]
@comment {{{
  run-writer-monad {{!
    ...
    ==> []
  !}} => [options []]
}}}
@comment {{{
  run-writer-monad
    {{!
      f g h => []
      ==> []
    !}}
  => [options []]
}}}
```

## Dynamic

### Pragmas

`#forall(<names>)(<ty>)`{.tmttmt}, `#exists(<names>)(<ty>)`{.tmttmt} quantifiers

`#type(<name> <arg_name...>)(<ty>)`{.tmttmt} like `data`{.haskell}

`#newtype(<name> <arg_name...>)(<ty>)`{.tmttmt} like `newtype`{.haskell}

`#synonym(<name> <arg_name...>)(<ty>)`{.tmttmt} like `type`{.haskell}

`#localize(<name>)(<suffix...>)`{.tmttmt}: for each `<suffix>`{.tmttmt} and for each `<arg_name>`{.tmttmt} when `<name>`{.tmttmt} was declared, if `<arg_name ~ suffix>`{.tmttmt} is a type variable in scope, use it to specialize `<name ~ suffix>`{.tmttmt}, otherwise use `<arg_name>`{.tmttmt}, otherwise leave it as a visible argument.

`#eval(<tm>+)`{.tmttmt}, eval and print

`#print(<tm>+)`{.tmttmt}, print without evaluating

`#infer(<tm>+)`{.tmttmt}, print the inferred type

### Attributes

`@comment`{.tmttmt} syntax check, but do not incorporate, the attributed aspect

`@log`{.tmttmt}
