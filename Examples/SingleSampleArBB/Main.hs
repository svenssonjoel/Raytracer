{- 2012 Joel Svensson -} 

import Trace.RGB
import Trace.Vector3
import Trace.Ray
import Trace.Shape
import Trace.Hit 
import Trace.Objects.Triangle

import Data.Maybe 
import Data.Word
import Data.Int
import Data.List 

import System.IO
import Foreign.Marshal.Array
import Foreign.Ptr

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

---------------------------------------------------------------------------- 
-- Convert a float image to a byte (0,256] image using ArBB  
arbbConvert :: Ptr RGB -> Ptr Int8 -> EmitArbb () 
arbbConvert src targ = do 
  
  s_f <- getScalarType_ ArbbF32 
  s_i <- getScalarType_ ArbbI8
  d_f <- getDenseType_ s_f 1   -- 1D array type
  d_i <- getDenseType_ s_i 1   -- 1D array type
  
  body <- funDef_ "body" [s_i] [s_f] $ \ [outp] [inp] -> do 
    imm  <- local_float32_ "imm"
    imm2 <- local_float32_ "imm2"
    twofiftysix <- float32_ 256
    op_ ArbbOpMul [imm] [inp,twofiftysix] 
    op_ ArbbOpFloor [imm2] [imm] 
    op_ ArbbOpCast  [outp]  [imm2]
  
  mapper <- funDef_ "mapper" [d_i] [d_f] $ \ [outp] [inp] -> do 
    map_ body [outp] [inp] 
  
  inb <- createDenseBinding_ (castPtr src) 1 [500*500*3*4{-size-}] [4{-pitch-}] 
  outb <- createDenseBinding_ (castPtr targ) 1 [500*500*3] [1] 
  
  gin <- createGlobal_ d_f "input" inb
  gout <- createGlobal_ d_i "output" outb 
  
  vin <- variableFromGlobal_ gin
  vout <- variableFromGlobal_ gout 
  
  execute_ mapper [vout] [vin] 
  

----------------------------------------------------------------------------
-- Main
main = 
  withBinaryFile "test.raw" WriteMode $ \ handle -> 
     withArray rgbs $ \ arr -> 
        allocaArray (500*500*3) $ \ bytes -> do 
          arbbSession (arbbConvert arr bytes) 
          hPutBuf handle bytes (500*500*3) 
  
    
  where 
    dir = Vector3 0 0 (-1) -- direction of rays
    
    
    pixCoords = [(x,y) | y <- [0..499], x <- [0..499]] 
    
    shapes = [Triangle (Vector3 300 600 (-500)) 
                       (Vector3 0 100 (-1000)) 
                       (Vector3 450 20 (-1000))
                       (RGB 0.8 0.2 0.2)] 
    
    image = castRays dir pixCoords 
    tmax = 100000
    castRays d pc = map (castRay d) pc 

    castRay  d (x,y) = 
      case (catMaybes (map (shapeHit (mkRay (Vector3 x y 0) d) 0 tmax) shapes)) of 
        [] -> Hit 0 (Vector3 0 0 0 ) (RGB 0.0 0.0 0.0)
        xs -> head (sortBy cmpHits xs) 

    rgbs = map hitGetRGB image
    --convert :: Hit -> [Word8]
    --convert (Hit _ _ (RGB r g b)) = [floor (r*256), floor (g*256), floor (b*256)]
    
    cmpHits (Hit t0 _ _) (Hit t1 _ _) = compare t1 t0 