module Widget.Datatypes where

import Prelude

import Control.Alternative (guard)
import Data.Array (fold)
import Data.Array as Array
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (oneOf)
import Data.HeytingAlgebra (ff)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (power)
import Data.Monoid as M
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class.Console (log)
import Riverdragon.Dragon.Bones (Dragon, (.$~~), (<:>), (=:=), (=?=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (hatching)
import Riverdragon.River as River
import Riverdragon.River.Beyond as Beyond
import Web.DOM.AttrName (AttrName(..))
import Widget (Widget)

type DatatypeWidget =
  { widget :: Widget
  , recognize :: String -> Boolean
  -- , parser :: ?help
  }

linkify :: Boolean -> Maybe String -> Dragon -> Dragon
linkify _ Nothing = identity
linkify external (Just href) =
  D.aW href [ D.attr "target" =?= ("_blank" <$ guard external) ]

nobreak :: String -> Dragon
nobreak = D.span [ D.attr "style" =:= "white-space: nowrap" ] <<< D.text

unabbreviate :: String -> Maybe Abbreviation
unabbreviate abbr =
  oneOf
    [ Map.lookup abbr abbreviations
    , Map.lookup (String.toLower abbr) abbreviations
    , Map.lookup (String.toUpper abbr) abbreviations
    ]

datatypes :: Map String DatatypeWidget
datatypes = Map.fromFoldable
  [ "abbr" /\
    { recognize: isJust <<< unabbreviate
    , widget: \{ text, content } -> do
        abbr <- text
        case unabbreviate abbr of
          Just meaning -> do
            embed <- content
            pure case meaning of
              Acronym expanded ->
                D.span [ D.className =:= "tooltipped abbreviated" ] $ D.Fragment
                  [ D.span [ D.className =:= "abbreviation", D.aria_"hidden" =:= "true" ] embed
                  , D.span [ D.className =:= "tooltip abbreviation" ] $ nobreak expanded
                  ]
              Described expanded ->
                D.span [ D.className =:= "tooltipped" ] $ D.Fragment
                  [ D.span [ D.className =:= "" ] embed
                  , D.span [ D.className =:= "tooltip", D.aria_"hidden" =:= "true" ] $ nobreak expanded
                  ]
              Jargon short fmt desc ->
                let
                  shortFmtDesc = case if desc == "" then FormatNone else fmt of
                    FormatNone -> [short]
                    FormatColon -> [short <> ":", desc]
                    FormatComma -> [short <> ",", desc]
                    FormatEquiv -> [short, "≈ " <> desc]
                    FormatEqual -> [short, "= " <> desc]
                    FormatDefEq -> [short, "≜ " <> desc]
                    FormatParen -> [short, "(" <> desc <> ")"]
                in D.span [ D.className =:= "tooltipped abbreviated" ] $ D.Fragment
                  [ D.span [ D.className =:= "abbreviation" ] embed
                  , D.span [ D.className =:= "tooltip abbreviation", D.aria_"hidden" =:= "true" ] $ D.Fragment
                    [ Array.intercalate (D.br[]) $ shortFmtDesc <#>
                        \m -> nobreak m
                    ]
                  ]
              Foreign lang value link meanings ->
                D.span [ D.className =:= "tooltipped abbreviated" ] $ D.Fragment
                  [ D.span [ D.className =:= "foreign", D.attr "lang" =:= lang ] embed
                  , D.span [ D.className =:= "tooltip abbreviation", D.aria_"hidden" =:= "true" ] $ D.Fragment
                    [ linkify true link $
                        D.span [ D.className =:= "foreign", D.attr "lang" =:= lang ] $ nobreak value
                    , Array.intercalate (D.text ",") $ meanings <#>
                        \m -> D.text " “" <> nobreak m <> D.text "”"
                    ]
                  ]
          Nothing -> do
            log $ "Unknown abbrevation " <> show abbr
            pure mempty
    }
  , "color" /\
    { recognize: const false
    , widget: \{ text, content } -> do
        color <- text
        embed <- content
        pure
          let
            attrs =
              [ D.className =:= "color-sample sample-after sample"
              , D.style =:= ("background-color:" <> color)
              ]
          in embed <> D.span attrs mempty
    }
  , "unicode" /\
    { recognize: isJust <<< String.stripPrefix (String.Pattern "U+")
    , widget: \{ text, content, rawAttr } -> do
        textValue <- text
        case parseUnicode textValue of
          Nothing -> do
            log $ "Invalid codepoint " <> show textValue
            pure mempty
          Just cp -> do
            hide <- maybe false (String.contains (String.Pattern "no-preview")) <$> rawAttr (AttrName "data-c")
            _self <- content
            pure
              let
                attrs =
                  [ D.className =:= "unicode-sample sample-after sample"
                  ]
              in fold
                [ D.span.$~~
                  [ D.span [ D.className =:= "meta-code" ] $ D.text "U+"
                  , D.span [ D.className =:= "code numeric" ] $ D.text $ printUnicodeHex cp
                  ]
                , M.guard (not hide) $ D.span attrs $ D.text $ String.singleton cp
                ]
    }
  , "etym" /\
    { recognize: ff
    , widget: \{ content, rawAttr } -> do
        mainContent <- content
        replacement <- rawAttr (AttrName "data-etym")
        pure $ hatching \shell -> do
          { stream: upstream, send } <- shell.track $ River.createStore false
          stream <- shell.store $ Beyond.dedup upstream
          shell.destructor =<< Beyond.keyEvents \{ mod } -> do
            let bti = if _ then 1 else 0
            send (mod.shift && bti mod.ctrl + bti mod.alt + bti mod.meta >= 2)
          let
            display = if _ then "" else "none"
            displayContent = display <$> not stream
            displayEtym = display <$> stream
          pure $ D.Fragment
            [ D.span [ D.stylish <:> D.smarties { display: displayContent } ] mainContent
            , D.span [ D.stylish <:> D.smarties { display: displayEtym } ]
              (maybe mainContent D.text replacement)
            ]
    }
  ]

parseUnicode :: String -> Maybe CodePoint
parseUnicode = String.trim >>> \s -> do
  s' <- oneOf
    [ String.stripPrefix (String.Pattern "U+") s
    , String.stripPrefix (String.Pattern "U") s
    , pure s
    ]
  i <- Int.fromStringAs hexadecimal (String.trim s')
  toEnum i

printUnicodeHex :: CodePoint -> String
printUnicodeHex cp = do
  let s = String.toUpper (Int.toStringAs hexadecimal (fromEnum cp))
  case String.length s of
    5 -> "0" <> s
    6 -> s
    l -> power "0" (4 - l) <> s

data Abbreviation
  = Acronym String
  | Jargon String InfoFormat String
  | Foreign String String (Maybe String) (Array String)
  | Described String

data InfoFormat
  = FormatNone
  | FormatColon
  | FormatComma
  | FormatEquiv
  | FormatEqual
  | FormatDefEq
  | FormatParen

abbreviations :: Map String Abbreviation
abbreviations = Map.fromFoldable
  [ "WIP" /\ Acronym "Work In Progress"
  , "TODO" /\ Acronym "To Do"
  , "e.g." /\ Foreign "la" "exempli\x0304 gra\x0304tia\x0304"
      (Just "https://en.wiktionary.org/wiki/e.g.#English")
      ["for example"]
  , "i.e." /\ Foreign "la" "id est"
      (Just "https://en.wiktionary.org/wiki/i.e.#English")
      ["that is"]
  , "cf." /\ Foreign "la" "co\x0304nfer"
      (Just "https://en.wiktionary.org/wiki/cf.#English")
      ["see also", "compare with"]
  , "viz." /\ Foreign "la" "vide\x0304licet"
      (Just "https://en.wiktionary.org/wiki/viz.#English")
      ["namely", "specifically"]
  , "sc." /\ Foreign "la" "sci\x0304licet"
      (Just "https://en.wiktionary.org/wiki/sc.#English")
      ["namely", "as follows"]
  , "tl;dr" /\ Acronym "too long; didnʼt read"
  , "etc." /\ Foreign "la" "et ce\x0304tera"
      (Just "https://en.wiktionary.org/wiki/etc.#English")
      ["and so on", "and the remaining items"]
  , "&c." /\ Foreign "la" "et ce\x0304tera"
      (Just "https://en.wiktionary.org/wiki/etc.#English")
      ["and so on", "and the remaining items"]
  , "et al." /\ Foreign "la" "et aliae, et alii\x0304"
      (Just "https://en.wiktionary.org/wiki/et_al.#Etymology_1")
      ["and others"]
  , "QED" /\ Foreign "la" "quod erat de\x0304mo\x0304nstrandum"
      (Just "https://en.wiktionary.org/wiki/Q.E.D.#English")
      ["which was to be proved"]
  , "wrt" /\ Acronym "with regard to"
  , "NbE" /\ Acronym "Normalization by Evaluation"
  , "GC" /\ Acronym "Garbage Collector"
  , "GCable" /\ Acronym "Garbage Collectable"
  , "N-ary" /\ Acronym "Arbitrary arity, in the sense of unary or binary operators"
  , "CSS" /\ Acronym "Cascading Style Sheets"
  , "JS" /\ Acronym "JavaScript"
  , "PS" /\ Acronym "PureScript"
  , "HS" /\ Acronym "Haskell"
  , "P2P" /\ Acronym "Peer-to-Peer"
  , "LAN" /\ Jargon "Local Area Network" FormatEquiv "private internet"
  , "WAN" /\ Jargon "Wide Area Network" FormatEquiv "public internet"
  , "IP" /\ Jargon "Internet Protocol" FormatEquiv "routing between computers"
  , "STUN" /\ Jargon "Session Traversal Utilities for NAT" FormatColon "returns public IP and public port"
  , "TURN" /\ Jargon "Traversal Using Relays around NAT" FormatColon "relay server for when P2P fails"
  , "SDP" /\ Jargon "Session Description Protocol" FormatColon "data for WebRTC establishment"
  , "ICE" /\ Jargon "Interactive Connectivity Establishment" FormatNone ""
  , "WebRTC" /\ Jargon "Web Real Time-Communication" FormatColon "between browsers or browser and server"
  , "CORS" /\ Acronym "Cross-Origin Resource Sharing"
  , "HTML" /\ Acronym "Hypertext Markup Language"
  , "XML" /\ Acronym "Extensible Markup Language"
  , "XHTML" /\ Acronym "XML-encoded HTML"
  , "CSS" /\ Acronym "Cascading Style Sheets"
  , "SCSS" /\ Jargon "CSS preprocessor" FormatNone ""
  , "Sass" /\ Jargon "CSS preprocessor" FormatComma "whitespace-sensitive"
  , "SVG" /\ Acronym "Scalable Vector Graphics"
  , "QR" /\ Jargon "Quick-Response code" FormatParen "a 2D Matrix Barcode"
  , "URI" /\ Acronym "Uniform Resource Indicator"
  , "URL" /\ Acronym "Uniform Resource Locator"
  , "NAT" /\ Jargon "Network Address Translation" FormatColon "routes private IPs to public internet"
  , "FRP" /\ Acronym "Functional Reactive Programming"
  , "FP" /\ Acronym "Functional Programming"
  , "OOP" /\ Acronym "Object-Oriented Programming"
  , "LR(1)" /\ Jargon "LtR, Rightmost Deriv., lookahead 1 token" FormatParen "Automaton parser to handle left-recursion"
  , "DOM" /\ Jargon "Document Object Model" FormatComma "represents HTML at runtime"
  , "VDOM" /\ Jargon "Virtual DOM" FormatComma "abstract DOM to render to real DOM"
  , "ADT" /\ Jargon "Algebraic Data Type" FormatComma "data as a sum of products; Haskell-style"
  , "MIDI" /\ Acronym "Musical Instrument Digital Interface"
  , "ASCII" /\ Jargon "Basic character set" FormatNone ""
  , "Unicode" /\ Jargon "Universal character set" FormatParen "fancy characters"
  , "HoTT" /\ Acronym "Homotopy Type Theory"
  , "Haskell" /\ Described "Pure & lazy functional programming, native codegen"
  , "PureScript" /\ Described "Strict & pure functional programming, JavaScript codegen"
  , "Dhall" /\ Described "Pure typed functional config language"
  , "UI" /\ Acronym "User Interface"
  , "UX" /\ Acronym "User Experience"
  , "JSON" /\ Acronym "JavaScript Object Notation"
  , "DAG" /\ Acronym "Directed Acyclic Graph"
  , "WHNF" /\ Acronym "Weak Head Normal Form"
  , "NF" /\ Acronym "Normal Form"
  , "AST" /\ Acronym "Abstract Syntax Tree"
  , "CST" /\ Acronym "Concrete Syntax Tree"
  , "WASM" /\ Acronym "Web Assembly"
  , "SKI" /\ Jargon "SKI Combinator Calculus" FormatColon "a minimal foundation for computation"
  , "API" /\ Jargon "Application Programming Interface" FormatEquiv "interface of components for programming"
  ]

matching :: String -> Array String
matching t =
  Map.toUnfoldable datatypes
    # Array.mapMaybe \(Tuple k { recognize }) ->
      if recognize t then Just k else Nothing

widget :: Widget
widget interface@{ text, rawAttr } = do
  givenDatatype <- rawAttr (AttrName "data-t") <#> fromMaybe ""
  t <- text
  datatype <- pure case givenDatatype of
    "" | [obvious] <- matching t -> obvious
    _ -> givenDatatype
  case Map.lookup datatype datatypes of
    Nothing -> do
      log $ "Unknown datatype " <> show datatype <> " (" <> show t <> ")"
      pure mempty
    Just datatypeWidget -> do
      datatypeWidget.widget interface
