{- 2012 Joel Svensson -} 

module Trace.Shape where 


import Trace.Vector3 
import Trace.Ray 
import Trace.Hit 


class Shape a where 
  shapeHit :: Ray -> Float -> Float -> a -> Maybe Hit
  