module ToStringQuasiQuoter (
  toString
) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

toString :: QuasiQuoter
toString = QuasiQuoter
  { quoteExp  = TH.stringE
  , quotePat  = fail "`toString` quasi-quoter is not supposed to be used in patterns"
  , quoteType = fail "`toString` quasi-quoter is not supposed to be used in types"
  , quoteDec  = fail "`toString` quasi-quoter is not supposed to be used in declarations"
  }
