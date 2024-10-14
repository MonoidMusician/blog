module Runtime.Types where

import Parser.Comb.Comber (Comber)

data NamedParser a = NamedParser String (Comber a)
