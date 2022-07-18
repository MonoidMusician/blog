module SSR where

import Prelude (($), pure)
import Data.String as String
import Deku.Toplevel (Template(..), runSSR)
import Effect (Effect)
import Parser.Main as Parser

ssr :: Effect String
ssr = pure $ String.trim
    """
    <!DOCTYPE html>
    <html>
    <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width">
    <title>PureScript Blog</title>
    </head>
    <body>
      <div data-widget="Widget.Query" data-widget-datakey="default" data-widget-data-keys="grammar"></div>
      <div data-widget="Parser.Main" data-widget-datakey="default"></div>
      <div data-widget="Parser.Input" data-widget-datakey="default"></div>
      <div data-widget="Parser.Random" data-widget-datakey="default"></div>
      <div data-widget="Parser.Explorer" data-widget-datakey="default"></div>
      <div data-widget="Parser.StateTable" data-widget-datakey="default"></div>
      <div data-widget="Parser.ParseTable" data-widget-datakey="default"></div>
    </body>
    """
