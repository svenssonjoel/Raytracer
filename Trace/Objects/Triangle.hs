{- 2012 Joel Svensson -} 

module Trace.Objects.Triangle where

import Trace.Vector3
import Trace.RGB
import Trace.Shape 
import Trace.Ray
import Trace.Hit

data Triangle = Triangle Vector3 Vector3 Vector3 
                         RGB
                         
epsilon = 0.00001                         

instance Shape Triangle where                          
      
   
  shapeHit ray tmin tmax (Triangle p0 p1 p2 color) = 
    if beta <= 0.0 || beta >= 1.0 || 
       gamma <= 0.0 || beta + gamma >= 1.0 ||  
       tval > tmax || tval < tmin 
    then Nothing  
    else Just$ Hit tval (normalize ((p1-p0) `crossProd` (p2-p0))) color
    where
      -- same as in Shirley & Morley but using dotProd & crossProd 
      -- functions in-place of "inlining" their definitions. 
      e0 = p0 - p1 
      e1 = p0 - p2 
    
      e4 = p0 - rayGetPos ray
  
      e3 = e1 `crossProd` rayGetDir ray
      
      denom  = e0 `dotProd` e3
      
      denom' = 1/denom 
      
      beta = (e4 `dotProd` e3) * denom' 
      
      e5 = e0 `crossProd` e4
      
      gamma = (e5 `dotProd` rayGetDir ray) * denom'
  
      tval = -(e5 `dotProd` e1) * denom'
      
-- Now the above and the below look very similar.       
      
-- From Real-time rendering 
{-  shapeHit ray tmin tmax (Triangle p0 p1 p2 color) = 
    if (a > (-epsilon) && a < epsilon) 
    then Nothing 
    else 
      if (u < 0.0) 
      then Nothing 
      else 
        if (v < 0.0 || u + v > 1.0) 
        then Nothing 
        else
          Just$ Hit t e1 color -- (normalize ((p1-p0) `crossProd` (p2-p0))) color
    where 
      e1 = p1 - p0 
      e2 = p2 - p0 
      q  = rayGetDir ray `crossProd` e2
      a  = e1 `dotProd` q
      
      -- 
      f  = 1/a 
      
      --
      s  = rayGetPos ray - p0 
      u  = f * (s `dotProd` q)
      
      r  = s `crossProd` e1
      v  = f * (rayGetDir ray `dotProd` r)
      
      t  = f * (e2 `dotProd` q)
-}      