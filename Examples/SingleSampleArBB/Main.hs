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
import Data.Int
import Data.List 

import System.IO
import Foreign.Marshal.Array
import Foreign.Ptr

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

---------------------------------------------------------------------------- 
-- Convert a float image to a byte [0,255) image using ArBB  
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
  
  -- elements, not bytes. (size) 
  inb <- createDenseBinding_ (castPtr src) 1 [500*500*3{-size-}] [4{-pitch-}] 
  outb <- createDenseBinding_ (castPtr targ) 1 [500*500*3] [1] 
  
  gin <- createGlobal_ d_f "input" inb
  gout <- createGlobal_ d_i "output" outb 
  
  vin <- variableFromGlobal_ gin
  vout <- variableFromGlobal_ gout 
  
  execute_ mapper [vout] [vin] 
  
----------------------------------------------------------------------------
-- generate arbb vector math functions 
genArbbDotProd = 
  do 
    s_f <- getScalarType_ ArbbF32
    s_d <- getDenseType_ s_f 1  
    funDef_ "dotProd" [s_f] [s_d,s_d] $ \ [out] [v1,v2] -> do 
      prods <- createLocal_ s_d "prods" 
      op_ ArbbOpMul [prods] [v1,v2] 
      opDynamic_ ArbbOpAddReduce [out] [prods]
    
genArbbCrossProd =   
  do 
    s_f <- getScalarType_ ArbbF32
    s_d <- getDenseType_ s_f 1 
    funDef_ "crossProd" [s_d] [s_d,s_d] $ \ [r] [v1,v2] -> do 
      lprods <- createLocal_ s_d "leftProds" 
      rprods <- createLocal_ s_d "rightProds" 
      
      one    <- isize_ 1 
      two    <- isize_ 2
      opDynamic_ ArbbOpRotate [v1] [v1,one] 
      opDynamic_ ArbbOpRotateReverse [v2] [v2,one] 
      op_ ArbbOpMul [lprods] [v1,v2] 
      opDynamic_ ArbbOpRotate [v1] [v1,one] 
      opDynamic_ ArbbOpRotateReverse [v2] [v2,one] 
      op_ ArbbOpMul [rprods] [v1,v2] 
      op_ ArbbOpSub [r] [lprods,rprods]
      
bindToVar1D ptr s p t nom = 
  do 
   t' <- getScalarType_ t 
   dt <- getDenseType_ t' 1
   bin <- createDenseBinding_ (castPtr ptr) 1 [s{-size-}] [p{-pitch-}] 
   glob <- createGlobal_ dt nom bin
   variableFromGlobal_ glob
   

----------------------------------------------------------------------------
-- Main
main = 
  -- Test
  withArray [1.0,1.0,1.0 :: Float] $ \ v1 -> 
    withArray [2.0,3.0,4.0 :: Float] $ \ v2 -> 
      allocaArray (3*4) $ \ r -> 
        do 
          arbbSession $ 
           do  
             f1 <- genArbbCrossProd

             var1 <- bindToVar1D v1 (3 {-elements-}) 4 ArbbF32 "v1" 
             var2 <- bindToVar1D v2 3 4 ArbbF32 "v2" 
             res1 <- bindToVar1D r  3 4 ArbbF32 "r" 
          
             execute_ f1 [res1] [var1,var2] 
          result <- (peekArray 3 r :: IO [Float])
          putStrLn $ show result    
          putStrLn $ show (Vector3 1 1 1 `crossProd` Vector3 2 3 4)
          
          
  
  --withBinaryFile "test.raw" WriteMode $ \ handle -> 
  --   withArray rgbs $ \ arr -> 
  --      allocaArray (500*500*3) $ \ bytes -> do 
  --        arbbSession (arbbConvert arr bytes) 
  --        hPutBuf handle bytes (500*500*3) 
  
    
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

    rgbs = map hitGetRGB image
    
    cmpHits (Hit t0 _ _) (Hit t1 _ _) = compare t0 t1 