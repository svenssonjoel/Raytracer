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
      -- Directly from "Realistic RayTracing, Shirley and Morley 
      (Vector3 a b c) = p0 - p1 
      (Vector3 d e f) = p0 - p2 
      (Vector3 g h i) = rayGetDir ray 
      (Vector3 j k l) = p0 - (rayGetPos ray) 
      eihf = e*i - h*f 
      gfdi = g*f - d*i 
      dheg = d*h - e*g
      
      denom = a*eihf + b*gfdi + c*dheg
      
      denom' = 1/denom 
      
      beta  = (j*eihf + k*gfdi + l*dheg) * denom'
      
      akjb = a*k - j*b
      jcal = j*c - a*l 
      blkc = b*l - k*c
      
      gamma = (i*akjb + h*jcal + g*blkc) * denom'
      
      tval = -(f*akjb + e*jcal + d*blkc) * denom'
    
      
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
      s  = rayGetPos ray - p0 -- book says v0?? 
      u  = f * (s `dotProd` q)
      
      r  = s `crossProd` e1
      v  = f * (rayGetDir ray `dotProd` r)
      
      t  = f * (e2 `dotProd` q)
-}      