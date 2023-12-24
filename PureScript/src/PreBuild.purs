module PreBuild where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import PureScript.CST as CST
import PureScript.ToSource (toSource)
import Tidy.Codegen as TC
import Tidy.Codegen as Tidy
import Tidy.Codegen.Monad as TCG

main :: Effect Unit
main = launchAff_ $ unsafePartial do
  parserlude <- FS.readTextFile UTF8 "./PureScript/src/Parser/Parserlude.purs"
  cst <- case CST.parseModule parserlude of
    CST.ParseSucceeded r -> pure r
    _ -> throwError $ error "Could not parse Parserlude"
  let
    sourced = do
      cstSource <- toSource cst
      moduleType <- TCG.importFrom "PureScript.CST.Types" (TCG.importType "Module")
      voidType <- TCG.importFrom "Prelude" (TCG.importType "Void")
      tell
        [ TC.declSignature "template" $
            TC.typeApp (TC.typeCtor moduleType) [TC.typeCtor voidType]
        , TC.declValue "template" [] cstSource
        ]
  FS.writeTextFile UTF8 "./PureScript/src/Parser/Template.purs" $
    Tidy.printModule $
      TCG.codegenModule "Parser.Template" sourced
