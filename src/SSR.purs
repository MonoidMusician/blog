module SSR where


import Deku.Toplevel (Template(..), runSSR)
import Effect (Effect)
import Parser.Main as Parser

ssr :: Effect String
ssr = runSSR
    ( Template
        { head:
            """<!DOCTYPE html>
<html>
	<head>
		<meta charset="utf-8" />
    <meta name="viewport" content="width=device-width">
		<title>PureScript Blog</title>
	</head>"""
        , tail: "</html>"
        }
    )
    Parser.main
