{- 2012 Joel Svensson -} 

module Trace.Hit where 

import Trace.RGB
import Trace.Vector3


data Hit = Hit {hitGetT :: Float,   -- t parameter
                hitGetNormal :: Vector3, -- normal
                hitGetRGB :: RGB}    -- Color