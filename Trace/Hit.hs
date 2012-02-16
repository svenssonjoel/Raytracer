{- 2012 Joel Svensson -} 

module Trace.Hit where 

import Trace.RGB
import Trace.Vector3


data Hit = Hit Float   -- t parameter
               Vector3 -- normal
               RGB     -- Color