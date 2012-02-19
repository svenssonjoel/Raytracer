{- 2012 Joel Svensson -} 

module Trace.Objects.Sphere where 

import Trace.Vector3 
import Trace.RGB
import Trace.Shape
import Trace.Ray
import Trace.Hit

----------------------------------------------------------------------------

data Sphere = Sphere {sphereGetCenter :: Vector3, 
                      sphereGetRadius :: Float,  
                      sphereGetColor  :: RGB} 
              

instance Shape Sphere where 
  shapeHit ray tmin tmax (Sphere center r rgb) = 
    if (discr' > 0) 
    then if ( t < tmin || t > tmax) 
         then Nothing 
         else Just (Hit t (normalize (rayGetPos ray +  
                              (t <*> rayGetDir ray) -
                              center)) rgb)
      
      else Nothing  
      
    where
      temp = rayGetPos ray - center
      
      a = (realToFrac (rayGetDir ray `dotProd` rayGetDir ray) :: Double)
      b = 2 * (realToFrac (rayGetDir ray `dotProd` temp) :: Double)
      c = (realToFrac (temp `dotProd` temp) :: Double)  - ((realToFrac r :: Double) * 
                                                           realToFrac r) 
      
      discr' = b*b - 4*a*c
      discr  = sqrt discr'
      t'' = ( - b - discr) / (2*a)
      t'  = if ( t'' < (realToFrac tmin) ) 
            then ( - b + discr) / (2*a)
            else t'' 
      t   = realToFrac t' :: Float