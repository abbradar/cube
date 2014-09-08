module Multiline where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

multiline :: QuasiQuoter
multiline = QuasiQuoter { quoteExp = litE . stringL }
