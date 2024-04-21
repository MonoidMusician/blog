module Parser.Languages.Dhall where

import Parser.Parserlude

import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Number as Math
import Data.Number as Number
import Data.Ratio (reduce)
import Data.String as String

end_of_line = token "\n" <|> token "\r\n"
tab = token "\t"
ascii = ascii_not_del <|> rawr "\\x7F"
ascii_not_del = rawr "[\\x20-\\x7E]"
valid_non_ascii = named "valid_non_ascii" $ oneOfMap rawr
  [ "[\\x80-\\uD7FF\\uE000-\\uFFFD]"
  , "[\\uD800-\\uD83E\\uD840-\\uD87E\\uD880-\\uD8BE\\uD8C0-\\uD8FE\\uD900-\\uD93E\\uD940-\\uD97E\\uD980-\\uD9BE\\uD9C0-\\uD9FE\\uDA00-\\uDA3E\\uDA40-\\uDA7E\\uDA80-\\uDABE\\uDAC0-\\uDAFE\\uDB00-\\uDB3E\\uDB40-\\uDB7E\\uDB80-\\uDBBE\\uDBC0-\\uDBFE][\\uDC00-\\uDFFF]"
  , "[\\uD83F\\uD87F\\uD8BF\\uD8FF\\uD93F\\uD97F\\uD9BF\\uD9FF\\uDA3F\\uDA7F\\uDABF\\uDAFF\\uDB3F\\uDB7F\\uDBBF\\uDBFF][\\uDC00-\\uDFFD]"
  ]
not_end_of_line = ascii <|> valid_non_ascii <|> tab
valid_char = not_end_of_line <|> end_of_line

block_comment = sourceOf $ namedRec "block_comment" \block_comment ->
  delim "{-" "-}" $ void valid_char <|> void block_comment

line_comment_prefix = sourceOf $ token "--" *> many "not_end_of_lines" not_end_of_line
line_comment = line_comment_prefix *> end_of_line
whitespace_chunk = "whitespace_chunk"#:
  rawr " " <|> tab <|> end_of_line <|> line_comment <|> block_comment
whsp = opt whsp1
whsp1 = void $ many1 "whsp1" whitespace_chunk

alpha = rawr "[A-Za-z]"
digit = rawr "[0-9]"
alphanum = alpha <|> digit
hexdig = digit <|> rawr "[AaBbCcDdEeFf]"

-- TODO
simple_label = "simple_label"#: fold
  [ simple_label_first_char
  , fold <$> many "simple_label_next_chars" simple_label_next_char
  ]
  where
  simple_label_first_char = alpha <|> rawr "_"
  simple_label_next_char = alphanum <|> rawr "-|/|_"

label = "label"#: join delim "`" ascii_not_del <|> simple_label

-- TODO
nonreserved_label = label

any_label = label

any_label_or_some = "any_label_or_some"#: any_label <|> token "Some"

with_component = any_label_or_some <|> token "?"

double_quote_chunk expr = "double_quote_chunk"#: oneOf
  [ Left <$> interpolation expr
  , Right <$> do token "\\" *> double_quote_escaped
  , Right <$> do ascii <|> valid_non_ascii
  ]
  where
  double_quote_escaped = oneOf
    [ token "\"" $> "\""
    , token "$" $> "$"
    , token "\\" $> "\\"
    , token "/" $> "/"
    , token "n" $> "\n"
    , token "r" $> "\r"
    , token "t" $> "\t"
    , token "b" $> "\x8"
    , token "f" $> "\xC"
    -- TODO: reject lone surrogates and reserved codepoints
    , token "u" *> rawr "[0-9a-fA-F]{4}" <#?> do
        Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
    , token "u" *> delim "{" "}" (rawr "0*[0-9a-fA-F]+") <#?> do
        Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
    ]

double_quote_literal expr = "double_quote_literal"#: do
  join delim "\"" $ many "double_quote_chunks" $ double_quote_chunk expr

single_quote_chunk expr = "single_quote_chunk"#: oneOf
  [ Left <$> interpolation expr
  , Right <$> do token "'''" $> "''"
  , Right <$> do token "''${" $> "${"
  , Right <$> do valid_char
  ]

-- TODO
single_quote_literal expr = "single_quote_literal"#: do
  join delim "''" $ end_of_line *> many "single_quote_chunks" (single_quote_chunk expr)

-- TODO
interpolation = delim "${" "}"

text_literal expr = double_quote_literal expr <|> single_quote_literal expr

bytes_literal = "bytes_literal"#: do
  delim "0x\"" "\"" $ foldMany "bytes" $ hexdig <> hexdig

keywords =
  [ "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "using"
  , "missing"
  , "as"
  , "Infinity"
  , "NaN"
  , "merge"
  , "Some"
  , "Universe"
  , "toMap"
  , "assert"
  , "forall"
  , "with"
  ]

builtins =
  [ "Natural/fold"
  , "Natural/build"
  , "Natural/isZero"
  , "Natural/even"
  , "Natural/odd"
  , "Natural/toInteger"
  , "Natural/show"
  , "Integer/toDouble"
  , "Integer/show"
  , "Integer/negate"
  , "Integer/clamp"
  , "Natural/subtract"
  , "Double/show"
  , "List/build"
  , "List/fold"
  , "List/length"
  , "List/head"
  , "List/last"
  , "List/indexed"
  , "List/reverse"
  , "Text/show"
  , "Text/replace"
  , "Date/show"
  , "Time/show"
  , "TimeZone/show"
  , "Bool"
  , "True"
  , "False"
  , "Optional"
  , "None"
  , "Natural"
  , "Integer"
  , "Double"
  , "Text"
  , "Date"
  , "Time"
  , "TimeZone"
  , "List"
  , "Type"
  , "Kind"
  , "Sort"
  ]

builtin = "builtin"#: oneOfMap token builtins

numeric_double_literal =
  rawr "[-+]?[0-9]+(\\.[0-9]+([eE][-+]?[0-9]+)?|[eE][-+]?[0-9]+)" <#?>
    Number.fromString -- TODO
double_literal = "double_literal"#: oneOf
  [ (-Math.infinity) <$ (token "-" *> token "Infinity")
  , Math.infinity <$ token "Infinity"
  , Math.nan <$ token "NaN"
  , numeric_double_literal
  ]
-- TODO
natural_literal = "natural_literal"#: oneOf
  [ token "0b" *> do
      rawr "[0-1]+" <#?> Int.fromStringAs binary
  , token "0x" *> do
      foldMany1 "hexdigs" hexdig <#?> Int.fromStringAs hexadecimal
  , rawr "0" $> 0
  , Int.fromString <$?> rawr "[1-9]" <> foldMany "digits" digit
  ]

integer_literal = "integer_literal"#: do
  oneOf
    [ token "+" $> identity
    , token "-" $> negate
    ] <*> natural_literal

-- TODO
temporal_literal = empty

-- TODO
identifier = variable <|> builtin

variable = "variable"#:
  nonreserved_label <> opt ((whsp *> token "@" <* whsp) <> map show natural_literal)

path_character =
  rawr "[\\x21\\x24-\\x27\\x2A-\\x2B\\x2D-\\x2E\\x30-\\x3B\\x3D\\x40-\\x5A\\x5E-\\x7A\\x7C\\x7E]"
quoted_path_character = valid_non_ascii <|> rawr "[\\x20-\\x21\\x23-\\x2E\\x30-\\x7F]"
unquoted_path_component = foldMany1 "unquoted_path_component" path_character
quoted_path_component = foldMany1 "quoted_path_component" quoted_path_character
path_component = token "/" *> (unquoted_path_component <|> join delim "\"" quoted_path_component)
path = many1 "path" path_component

local = opt (token ".." <|> token "." <|> token "~") <> map fold path

-- TODO
http_raw = empty
  -- sourceOf $ sequence_
  --   [ token "http://" <|> token "https://" -- TODO
  --   , opt (foldMany "userinfo" <> token "@")
  --   , oneOf
  --     [ 
  --     ]
  --   ]

-- TODO
http = http_raw /|\ optional (whsp1 *> token "using" *> whsp1 *> empty)

env = empty

import_ = empty

arrow = token "->" <|> token "\x2192"

grammar = mutual \grammar@{ expression, import_expression } ->
  { expression: oneOf
      [ ado
          token "\\" <|> token "\x3BB"
          whsp
          token "("
          whsp
          nonreserved_label
          whsp
          token ":"
          whsp1
          expression
          whsp
          token ")"
          whsp
          arrow
          whsp
          expression
        in unit
      , ado
          token "if"
          whsp1
          expression
          whsp
          token "then"
          whsp1
          expression
          whsp
          token "else"
          whsp1
          expression
        in unit
      , ado
          many1 "let_binding" ado
            token "let"
            whsp1
            nonreserved_label
            whsp
            optional ado
              token ":"
              whsp1
              expression
              whsp
              in unit
            token "="
            whsp
            expression
            whsp1
            in unit
          token "in"
          whsp1
          expression
        in unit
      , ado
          token "forall" <|> token "\x2200"
          whsp
          token "("
          nonreserved_label
          whsp
          token ":"
          whsp1
          expression
          whsp
          token ")"
          whsp
          arrow
          whsp
          expression
        in unit
      , ado
          grammar.operator_expression
          whsp
          arrow
          whsp
          expression
        in unit
      , ado
          import_expression
          many1 "with_clauses" ado
            whsp1
            token "with"
            whsp1
            many1SepBy "with_components" (whsp *> token ":" <* whsp) with_component
            whsp
            token "="
            whsp
            grammar.operator_expression
            in unit
        in unit
      , ado
          token "merge"
          whsp1
          import_expression
          whsp1
          import_expression
          whsp
          token ":"
          whsp1
          expression
        in unit
      , ado
          token "["
          whsp
          optional (token "," <* whsp)
          token "]"
          whsp
          token ":"
          whsp1
          expression
        in unit
      , ado
          token "toMap"
          whsp1
          import_expression
          whsp
          token ":"
          whsp1
          expression
        in unit
      , ado
          token "assert"
          whsp
          token ":"
          whsp1
          expression
        in unit
      , ado
          grammar.operator_expression
          optional ado
            whsp
            token ":"
            whsp1
            expression
            in unit
        in unit
      ]
  , operator_expression:
      let
        op prec ts w = ado
          grammar.operator_expression
          whsp
          oneOfMap (tokenPrecL <@> reduce prec 1) ts
          if w then whsp1 else whsp
          grammar.operator_expression
          in unit
      in oneOf
        [ op 0 ["===", "\x2261"] false
        , op 1 ["?"] true
        , op 2 ["||"] false
        , op 3 ["+"] true
        , op 4 ["++"] false
        , op 5 ["#"] false
        , op 6 ["&&"] false
        , op 7 ["/\\", "\x2227"] false
        , op 8 ["//", "\x2AFD"] false
        , op 9 ["//\\\\", "\x2A53"] false
        , op 10 ["*"] false
        , op 11 ["=="] false
        , op 12 ["!="] false
        , grammar.application_expression
        ]
  , application_expression: ado
      oneOf
        [ token "merge" *> whsp1 *> import_expression *> whsp1 *> import_expression
        , token "toMap" *> whsp1 *> import_expression
        , token "Some" *> whsp1 *> import_expression
        , token "showConstructor" *> whsp1 *> import_expression
        , import_expression
        ]
      many "import_expressions" (whsp1 *> import_expression)
      in unit
  , import_expression: void import_ <|> grammar.completion_expression
  , completion_expression: ado
      grammar.selector_expression
      optional ado
        whsp
        token "::"
        whsp
        grammar.selector_expression
        in unit
      in unit
  , selector_expression: ado
      grammar.primitive_expression
      many "selectors" ado
        whsp
        token "."
        whsp
        oneOf
          [ void any_label
          , void $ delim "{" "}" ado
              whsp *> optional (token "," <* whsp)
              optional ado
                any_label_or_some
                whsp
                many "labels" ado
                  token ","
                  whsp
                  any_label_or_some
                  in unit
                optional (token "," <* whsp)
                in unit
            in unit
          , void $ delim "(" ")" $ whsp *> expression <* whsp
          ]
        in unit
      in unit
  , primitive_expression: oneOf
      [ void temporal_literal
      , void double_literal
      , void natural_literal
      , void integer_literal
      , void (text_literal expression)
      , void bytes_literal
      , delim "{" "}" ado
          whsp *> optional (token "," <* whsp)
          oneOf
            let
              rec name entry = ado
                many1SepBy name (whsp *> token "," <* whsp) entry
                optional (whsp *> token ",")
                in unit
            in
              [ void $ token "=" <* optional (whsp *> token ",")
              , pure unit
              , rec "record_type_entries" ado
                  any_label_or_some
                  whsp
                  token ":"
                  whsp1
                  expression
                  in unit
              , rec "record_value_entries" ado
                  many1SepBy "label_path" (whsp *> token "." <* whsp) any_label_or_some
                  whsp
                  token "="
                  whsp
                  expression
                  in unit
              ]
          in unit
      , void $ delim "<" ">" $ entries0 "union_type_entries" "|" ado
          any_label_or_some
          optional ado
            whsp
            token ":"
            whsp1
            expression
            in unit
          in unit
      , void $ delim "[" "]" $ entries0 "non_empty_list_entries" "," expression
      , void identifier
      , delim "(" ")" $ whsp *> expression <* whsp
      ]
  }

entries0 name sep entry = ado
  whsp
  optional (token sep <* whsp)
  r <- opt ado
    r <- NEA.toArray <$> many1SepBy name (whsp *> token sep <* whsp) entry
    opt (whsp *> token sep)
    in r
  whsp
  in r

complete_dhall_file = sourceOf ado
  many "shebangs" $ token "#!" *> many "shebang" not_end_of_line <* end_of_line
  whsp
  grammar.expression
  whsp
  optional $ line_comment_prefix
  in unit
