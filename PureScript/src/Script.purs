module Script where

import Prelude

import Conversions.Index (conversions)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Functor (mapFlipped)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice ((+++), (|||))
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Ref as Ref
import Idiolect ((>==))
import Node.Encoding (Encoding(..))
import Node.Process (argv, exit, stderr, stdin, stdout)
import Node.Stream (onDataString, onEnd, onError, writeString)
import Parser.Comb.Comber ((<|>))
import Parser.Comb.Comber as Comber
import Parser.Languages.ShowFast (mkReShow)
import PureScript.CST.Lexer as CST.Lexer
import PureScript.Highlight (classify, highlight, highlightPandoc, highlightPandocInline)
import Runtime.Live (lexemes, lexemes')

noArgs :: (String -> Aff (Either String String)) -> (Array String -> Either String (String -> Aff (Either String String)))
noArgs = const <<< Right

succeeds :: forall i e r. (i -> Aff r) -> i -> Aff (Either e r)
succeeds = compose (map Right)

noAff :: forall i r. (i -> r) -> i -> Aff r
noAff = compose pure

simplest :: (String -> String) -> (Array String -> Either String (String -> Aff (Either String String)))
simplest = noArgs <<< succeeds <<< noAff

simpler :: (String -> Either String String) -> (Array String -> Either String (String -> Aff (Either String String)))
simpler = noArgs <<< noAff

failing :: forall i e r. (i -> e) -> i -> Aff (Either e r)
failing = noAff <<< map Left

scripts :: Map String (Array String -> Either String (String -> Aff (Either String String)))
scripts = Map.fromFoldable
  [ Tuple "reShow" $ simplest $ mkReShow Nothing <@> Dodo.twoSpaces
  , Tuple "echo" $ simplest identity
  , Tuple "highlight" $ simplest highlight
  , Tuple "highlightPandoc" $ simplest highlightPandoc
  , Tuple "highlightPandocInline" $ simplest highlightPandocInline
  , Tuple "lexemes" $ simplest $
      let f tok str = str <> " " <> show (classify tok)
      in intercalate "\n" <<< lexemes' f <<< CST.Lexer.lex
  -- , Tuple "assembleParser" $ simpler $ lmap printErrors <<< assemble
  -- , Tuple "compileParser" $ noArgs $
  --     assemble >>> do
  --       failing printErrors |||
  --         compile >== (intercalate "\n" +++ identity)
  -- , Tuple "bundleParser" $ noArgs $
  --     assemble >>> do
  --       failing printErrors |||
  --         compile >== (intercalate "\n" +++ bundle)
  ] <|> mapFlipped conversions \parserPrinter -> noArgs $ noAff $
      Comber.parse parserPrinter >== Dodo.print Dodo.plainText Dodo.twoSpaces

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
