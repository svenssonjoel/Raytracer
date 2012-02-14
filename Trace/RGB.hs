{- 2012 Joel Svensson -} 

module Trace.RGB where 

import Foreign.Storable
import Foreign.C.Types

----------------------------------------------------------------------------
-- Colors 

data RGB = RGB Float Float Float 
  deriving (Eq,Show)


clampRGB rgb = clampDown$ clampUp rgb  
  where 
    clampUp (RGB r g b) = RGB (if r > 1.0 then 1.0 else r)
                              (if g > 1.0 then 1.0 else g)
                              (if b > 1.0 then 1.0 else b) 
    clampDown (RGB r g b) = RGB (if r < 0.0 then 0.0 else r) 
                                (if g < 0.0 then 0.0 else g)
                                (if b < 0.0 then 0.0 else b)

---------------------------------------------------------------------------- 
-- Instances 

instance Storable RGB where 
  sizeOf _ = 3 * sizeOf (undefined :: Float) 
  alignment _ = 4  -- ? 
  peek p = do 
    r <- realToFrac `fmap` (peekByteOff p 0 :: IO CFloat)
    g <- realToFrac `fmap` (peekByteOff p s :: IO CFloat) 
    b <- realToFrac `fmap` (peekByteOff p (2*s) :: IO CFloat) 
    return $ RGB r g b 
    where s = sizeOf (undefined :: Float)
  poke p (RGB r g b) = do 
    pokeByteOff p 0     (realToFrac r :: CFloat) 
    pokeByteOff p s     (realToFrac g :: CFloat)
    pokeByteOff p (2*s) (realToFrac b :: CFloat) 
    where s = sizeOf (undefined :: Float)



instance Num RGB where 
  (+) (RGB r g b) (RGB r' g' b') = RGB (r+r') (g+g') (b+b') 
  (-) (RGB r g b) (RGB r' g' b') = RGB (r-r') (g-g') (b-b') 
  (*) (RGB r g b) (RGB r' g' b') = RGB (r*r') (g*g') (b*b') 
  abs = undefined
  signum = undefined 
  fromInteger = undefined 
  
-- and so on 