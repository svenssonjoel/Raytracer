{- 2012 Joel Svensson -} 

module Trace.Objects.Triangle where

import Trace.Vector3
import Trace.RGB
import Trace.Shape 
import Trace.Ray
import Trace.Hit

data Triangle = Triangle Vector3 Vector3 Vector3 
                         RGB
                         
                         
instance Shape Triangle where                          
  shapeHit ray tmin tmax (Triangle p0 p1 p2 color) = 
    if beta <= 0.0 || beta >= 1.0 || 
       gamma <= 0.0 || beta + gamma >= 1.0 ||  
       tval > tmax || tval < tmin 
    then Nothing  
    else Just$ Hit tval (normalize ((p1-p0) `crossProd` (p2-p0))) color
    where
      -- Directly from "Realistic RayTracin, Shirley and Morley 
      (Vector3 a b c) = p0 - p1 
      (Vector3 d e f) = p0 - p2 
      (Vector3 g h i) = rayGetDir ray 
      (Vector3 j k l) = p0 - (rayGetPos ray) 
      eihf = e*i - h*f 
      gfdi = g*f - d*i 
      dheg = d*h - e*g
      
      denom = a*eihf + b*gfdi + c*dheg
      beta  = (j*eihf + k*gfdi + l*dheg) / denom
      
      akjb = a*k - j*b
      jcal = j*k - a*l 
      blkc = b*l - k*c
      
      gamma = (i*akjb + h*jcal + g*blkc) / denom 
      
      tval = -(f*akjb + e*jcal + d*blkc) / denom
      