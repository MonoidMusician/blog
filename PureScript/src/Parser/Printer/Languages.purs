module Parser.Printer.Languages where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Either.Inject as Auto
import Data.Either.Nested (type (\/), (\/))
import Data.Filterable (filterMap)
import Data.Lens as Q
import Data.Maybe (fromMaybe)
import Data.Number as Number
import Data.Profunctor (dimap)
import Data.Profunctor.Choice ((|||))
import Data.String as S
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\))
import Dodo as O
import Foreign.Object as Object
import Idiolect ((<#?), (<$?>))
import Parser.Comb.Comber as I
import Parser.Languages as IL
import Parser.Printer.Juxt ((!!!), (!/), (!>), (!\), (/!), (<!), (\!/))
import Parser.Printer.Juxt as J
import Parser.Printer.Types as IO
import Whitespace as WS

jsonNumber :: IO.PP Number
jsonNumber = IO.named "json_number" $ IO.printerParser'
  do O.text <<< apply fromMaybe (S.stripSuffix (S.Pattern ".0")) <<< show
  do Number.fromString <$?> I.rawr "[-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?"

jsonString :: IO.PP String
jsonString = IO.named "json_string" $ IO.printerParser'
  do O.text <<< Json.stringify <<< Json.fromString
  do IL.string

jsonBoolean :: IO.PP Boolean
jsonBoolean = IO.named "json_boolean" $
  J._Boolean
  !!! IO.token "false"
  \!/ IO.token "true"

jsonNull :: IO.PP Unit
jsonNull = IO.token "null"

json :: IO.PP Json.Json
json = IO.namedRec "json" \jsonValue ->
  let
    ws = IO.wsOf WS.Allowed
    wsws p = ws !> p <! ws
    comma = IO.wsOf WS.Allowed !> IO.token "," <! IO.wsOf (WS.BreakPreferred true)
    jsonArrayContents = IO.manySepBy "json_array_contents" comma
      $   IO.wsOf (WS.BreakPreferred true)
       !> jsonValue
    jsonArray = IO.flexGroup
      $   IO.token "["
       !> IO.indent jsonArrayContents
      <!  IO.wsOf (WS.BreakPreferred true)
      <!  IO.token "]"
    jsonObjectContents = IO.manySepBy "json_object_contents" comma
      $   IO.wsOf (WS.BreakPreferred true)
       !> jsonString
      /!  IO.wsOf WS.Allowed
       !> IO.token ":"
      <!  IO.wsOf (WS.SpacePreferred true)
       !\ jsonValue
    jsonObject = IO.flexGroup
      $   IO.token "{"
       !> IO.indent do
            IO.checkError jsonObjectContents ["Duplicate keys"] $
              map fst >>> \keys ->
                Array.length (Array.nub keys) == Array.length keys
      <!  IO.wsOf (WS.BreakPreferred true)
      <!  IO.token "}"
    _Json :: Q.Iso' Json.Json (Unit \/ Boolean \/ Number \/ String \/ Array Json.Json \/ Array (String /\ Json.Json))
    _Json = dimap fromJson toJson
    fromJson = Json.caseJson Auto.inj Auto.inj Auto.inj Auto.inj Auto.inj
      (Object.toUnfoldable >>> (identity :: Array ~> Array) >>> Auto.inj)
    toJson = const Json.jsonNull
      ||| Json.fromBoolean
      ||| Json.fromNumber
      ||| Json.fromString
      ||| Json.fromArray
      ||| (Json.fromObject <<< Object.fromFoldable)
  in wsws $
    _Json
    !!! jsonNull
    \!/ jsonBoolean
    \!/ jsonNumber
    \!/ jsonString
    \!/ jsonArray
    \!/ jsonObject

-- O.break
