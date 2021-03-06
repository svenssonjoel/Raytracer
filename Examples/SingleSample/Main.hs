{- 2012 Joel Svensson -} 

import Trace.RGB
import Trace.Vector3
import Trace.Ray
import Trace.Shape
import Trace.Hit 
import Trace.Objects.Triangle
import Trace.Objects.Sphere

import Data.Maybe 
import Data.Word
import Data.List 

import System.IO
import Foreign.Marshal.Array

main = 
  withBinaryFile "test.raw" WriteMode $ \ handle -> 
     withArray (concatMap convert image) $ \ arr -> 
       hPutBuf handle arr (500*500*3) 
  
    
  where 
    dir = Vector3 0 0 (-1) -- direction of rays
    
    
    pixCoords = [(x,y) | y <- [0..499], x <- [0..499]] 
    
    shapesTrigs = [Triangle (Vector3 300 600 (-800)) 
                            (Vector3 0 100 (-1000)) 
                            (Vector3 450 20 (-1000))
                            (RGB 0.8 0.2 0.2)] 
    shapesSpheres = [Sphere   (Vector3 250 250 (-1000))
                              150
                              (RGB 0.2 0.2 0.8)] 
    
    image = castRays dir pixCoords 
    tmax = 100000
    castRays d pc = map (castRay d) pc 
   
    castRay  d (x,y) = 
      case (catMaybes (
               map (shapeHit ray 0 tmax) shapesSpheres               
               ++ 
               map (shapeHit ray 0 tmax) shapesTrigs 

               )) of 
        [] -> Hit 0 (Vector3 0 0 0 ) (RGB 0.0 0.0 0.0)
        xs -> head (sortBy cmpHits xs) 
       where
          ray = mkRay (Vector3 x y 0) d 

    convert :: Hit -> [Word8]
    convert (Hit _ _ (RGB r g b)) = [floor (r*256), floor (g*256), floor (b*256)]
    
    cmpHits (Hit t0 _ _) (Hit t1 _ _) = compare t0 t1 