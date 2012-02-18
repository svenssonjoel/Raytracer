{- 2012 Joel Svensson -} 

module Trace.Vector3 where 

import Foreign.Storable 
import Foreign.C.Types

data Vector3 = Vector3 Float Float Float 
     deriving (Eq,Show) 

instance Storable Vector3 where 
  sizeOf _ = 3 * sizeOf (undefined :: Float) 
  alignment _ = 4  -- ? 
  peek p = do 
    x <- realToFrac `fmap` (peekByteOff p 0 :: IO CFloat)
    y <- realToFrac `fmap` (peekByteOff p s :: IO CFloat) 
    z <- realToFrac `fmap` (peekByteOff p (2*s) :: IO CFloat) 
    return $ Vector3 x y z 
    where s = sizeOf (undefined :: Float)
  poke p (Vector3 x y z) = do 
    pokeByteOff p 0     (realToFrac x :: CFloat) 
    pokeByteOff p s     (realToFrac y :: CFloat)
    pokeByteOff p (2*s) (realToFrac z :: CFloat) 
    where s = sizeOf (undefined :: Float)



instance Num Vector3 where 
  (+) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x+x') (y+y') (z+z') 
  (-) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x-x') (y-y') (z-z') 
  (*) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x*x') (y*y') (z*z') 
  abs = undefined
  signum = undefined 
  fromInteger = undefined 
  
  
  
----------------------------------------------------------------------------
-- Vector ops 



magnitude :: Vector3 -> Float 
magnitude (Vector3 x y z) = sqrt (x*x + y*y + z*z) 

normalize :: Vector3 -> Vector3
normalize v@(Vector3 x y z) = Vector3 (x / m) (y / m) (z / m) 
  where 
    m = magnitude v
    
dotProd :: Vector3 -> Vector3 -> Float    
dotProd (Vector3 x y z) (Vector3 u v w) = 
  x * u + y * v + z * w 

crossProd  :: Vector3 -> Vector3 -> Vector3 
crossProd (Vector3 x y z) (Vector3 u v w) = 
  Vector3 (y * w - z * v) 
          (z * u - x * w) 
          (x * v - y * u) 
  

scaleVector :: Vector3 -> Float -> Vector3 
scaleVector (Vector3 x y z) s = Vector3 (x*s) (y*s) (z*s)  


(<*>) t (Vector3 x y z) = Vector3 (t*x) (t*y) (t*z)