module Riverdragon.Dragon.Bones where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Riverdragon.Dragon (Dragon(..))

_el = Element Nothing <@> empty

text = Text <<< pure
code = _el "code"
span = _el "span"
div = _el "div"
pre = _el "pre"
ol = _el "ol"
li = _el "li"
b = _el "b"
i = _el "i"

svgNS = "http://www.w3.org/2000/svg"
xhtmlNS = "http://www.w3.org/1999/xhtml"
xlinkNS = "http://www.w3.org/1999/xlink"
xmlNS = "http://www.w3.org/XML/1998/namespace"
xmlnsNS = "http://www.w3.org/2000/xmlns/"
