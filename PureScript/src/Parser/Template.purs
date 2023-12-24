-- Use this command to ignore this file:
--     git update-index --skip-worktree
module Parser.Template where

import PureScript.CST.Types

import Prelude (Void)
import Unsafe.Coerce (unsafeCoerce)

template :: Module Void
template = unsafeCoerce {}
