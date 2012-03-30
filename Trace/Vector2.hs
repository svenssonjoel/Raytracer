
module Trace.Vector2 where 

import Foreign.Storable 
import Foreign.C.Types 


data Vector2 = Vector2 Float Float 
               deriving (Eq, Show) 
                        
instance Storable Vector2 where 
  sizeOf _ = 2 * sizeOf (undefined :: Float) 
  alignment _ = 4  -- ? 
  peek p = do 
    x <- realToFrac `fmap` (peekByteOff p 0 :: IO CFloat)
    y <- realToFrac `fmap` (peekByteOff p s :: IO CFloat) 
    return $ Vector2 x y  
    where s = sizeOf (undefined :: Float)
  poke p (Vector2 x y) = do 
    pokeByteOff p 0     (realToFrac x :: CFloat) 
    pokeByteOff p s     (realToFrac y :: CFloat)
    where s = sizeOf (undefined :: Float)
