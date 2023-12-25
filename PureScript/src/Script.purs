module Script where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Ref as Ref
import Idiolect ((==<), (>==))
import Node.Encoding (Encoding(..))
import Node.Process (argv, exit, stderr, stdin, stdout)
import Node.Stream (onDataString, onEnd, onError, writeString)
import Parser.Languages.Show (mkReShow)
import Parser.Main.Comb (assemble, bundle, compile, printErrors)
import PureScript.Highlight (highlight, highlightPandoc)

scripts :: Map String (Array String -> Either String (String -> Aff (Either String String)))
scripts = Map.fromFoldable
  [ Tuple "reShow" \_ -> Right (pure <<< Right <<< mkReShow Nothing)
  , Tuple "echo" (const (Right (pure <<< Right)))
  , Tuple "highlight" (const (Right (pure <<< Right <<< highlight)))
  , Tuple "highlightPandoc" (const (Right (pure <<< Right <<< highlightPandoc)))
  , Tuple "assembleParser" (const (Right (pure <<< lmap printErrors <<< assemble)))
  , Tuple "compileParser" $ const $ Right $ assemble >>> case _ of
      Left errs -> pure $ Left $ printErrors errs
      Right assembled -> compile assembled <#> case _ of
        Left errs -> Left $ intercalate "\n" errs
        Right result -> Right result
  , Tuple "bundleParser" $ const $ Right $ assemble >>> case _ of
      Left errs -> pure $ Left $ printErrors errs
      Right assembled -> compile assembled <#> case _ of
        Left errs -> Left $ intercalate "\n" errs
        Right result -> Right $ bundle result
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
              result <- fn input
              liftEffect case result of
                Left err -> do
                  void $ writeString stderr UTF8 err mempty
                  exit 1
                Right output -> do
                  void $ writeString stdout UTF8 output mempty
                  pure unit
