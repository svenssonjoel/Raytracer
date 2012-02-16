{- 2012 Joel Svensson -} 

module Trace.Ray where 

import Trace.Vector3 


-- make difference between vector and points ? 
data Ray = Ray {rayGetPos :: Vector3,  
                rayGetDir :: Vector3}
           
mkRay = Ray 


rayTime :: Ray -> Float -> Vector3            
rayTime r t = rayGetPos r + (rayGetDir r `scaleVector` t)
