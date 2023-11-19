module Script where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.Process (argv, exit, stdin)
import Node.Stream (onDataString, onEnd, onError)
import Parser.Languages.Show (reShow)

scripts :: Map String (Array String -> Either String (String -> String))
scripts = Map.fromFoldable
  [ Tuple "reShow" (const (Right reShow))
  , Tuple "echo" (const (Right identity))
  ]

main :: Effect Unit
main = do
  argv >>= Array.drop 1 >>> Array.uncons >>> case _ of
    Nothing -> do
      logShow $ Array.fromFoldable $ Map.keys scripts
      exit 1
    Just { head: script, tail: args } -> do
      case Map.lookup script scripts of
        Nothing -> do
          log $ "Unknown script " <> show script <> ", please call one of these scripts:"
          logShow $ Array.fromFoldable $ Map.keys scripts
          exit 1
        Just exec -> do
          case exec args of
            Left err -> log err
            Right fn -> launchAff_ do
              input <- makeAff \cb -> do
                ref <- Ref.new ""
                onDataString stdin UTF8 \s -> do
                  Ref.modify_ (_ <> s) ref
                onEnd stdin do
                  Ref.read ref >>= Right >>> cb
                onError stdin \e -> do
                  log (show e)
                  cb (Left e)
                pure mempty
              liftEffect $ log $ fn input
