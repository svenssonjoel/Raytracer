{-# LANGUAGE ScopedTypeVariables #-} 
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
  inb <- createDenseBinding_ (castPtr src) 1 [10*10*3{-size-}] [4{-pitch-}] 
  outb <- createDenseBinding_ (castPtr targ) 1 [10*10*3] [1] 
  
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
    d_f <- getDenseType_ s_f 1  
    funDef_ "dotProd" [s_f] [d_f,d_f] $ \ [out] [v1,v2] -> do 
      prods <- createLocal_ d_f "prods" 
      op_ ArbbOpMul [prods] [v1,v2] 
      opDynamic_ ArbbOpAddReduce [out] [prods]
    
genArbbCrossProd =   
  do 
    s_f <- getScalarType_ ArbbF32
    d_f <- getDenseType_ s_f 1 
    funDef_ "crossProd" [d_f] [d_f,d_f] $ \ [r] [v1,v2] -> do 
      lprods <- createLocal_ d_f "leftProds" 
      rprods <- createLocal_ d_f "rightProds" 
      
      one    <- isize_ 1 
      two    <- isize_ 2
      opDynamic_ ArbbOpRotate [v1] [v1,one] 
      opDynamic_ ArbbOpRotateReverse [v2] [v2,one] 
      op_ ArbbOpMul [lprods] [v1,v2] 
      opDynamic_ ArbbOpRotate [v1] [v1,one] 
      opDynamic_ ArbbOpRotateReverse [v2] [v2,one] 
      op_ ArbbOpMul [rprods] [v1,v2] 
      op_ ArbbOpSub [r] [lprods,rprods]
        
genRayTrigIntersect =       
  do 
    s_i <- getScalarType_ ArbbI32
    s_b <- getScalarType_ ArbbBoolean
    s_f <- getScalarType_ ArbbF32
    d_f <- getDenseType_  s_f 1   -- 3d vectors and points
    
    crossP <- genArbbCrossProd
    dotP   <- genArbbDotProd
    
    funDef_ "trigIntersect" [{-s_i,-}d_f] [d_f,d_f,d_f,d_f,d_f,d_f,d_f] $ \ [{-hit,-}color] [o,d,t0,t1,t2,tcol,black] -> do 
      e0 <- createLocal_ d_f "e0"
      e1 <- createLocal_ d_f "e1"
      e3 <- createLocal_ d_f "e3"
      e4 <- createLocal_ d_f "e4" 
      e5 <- createLocal_ d_f "e5"
      
      denom <- createLocal_ s_f "denom" 
      beta  <- createLocal_ s_f "beta"
      gamma <- createLocal_ s_f "gamma" 
      tval  <- createLocal_ s_f "tval" 
      cond1 <- createLocal_ s_b "c1"
      cond2 <- createLocal_ s_b "c2"
      cond3 <- createLocal_ s_b "c3"
      
      zero <- float32_ 0
      one  <- float32_ 1
      
      true <- int32_ 1
      false <- int32_ 0
      
      op_ ArbbOpSub [e0] [t0,t1] 
      op_ ArbbOpSub [e1] [t0,t2] 
      op_ ArbbOpSub [e4] [t0,o]  --t0 - rayOrigin
      
      call_ crossP [e3] [e1,d] 
      
      call_ dotP   [denom] [e0,e3] 
      
      op_ ArbbOpDiv [denom] [one,denom] 
      
      call_ dotP   [beta] [e4,e3] 
      op_ ArbbOpMul [beta] [beta,denom] 
      
      call_ crossP [e5] [e0,e4] 
      
      call_ dotP [gamma] [e5,d]      
      op_ ArbbOpMul [gamma] [gamma,denom]
      
      call_ dotP [tval] [e5,e1] 
      op_ ArbbOpSub [tval] [zero,tval]
      op_ ArbbOpMul [tval] [tval,denom]
      
      op_ ArbbOpLeq [cond1] [beta,zero] 
      op_ ArbbOpGeq [cond2] [beta,one] 
      op_ ArbbOpMax [cond3] [cond1,cond2] -- Max used as or. possible ? 
      
      if_ cond3 
        (do 
          -- op_ ArbbOpCopy [hit] [false]   
          op_ ArbbOpCopy [color] [black]
        ) 
        (do 
          op_ ArbbOpLeq [cond1] [gamma,zero]   
          bpg <- createLocal_ s_f "beta + gamma" 
          op_ ArbbOpAdd [bpg] [beta,gamma] 
          op_ ArbbOpGeq [cond2] [bpg,one] 
          op_ ArbbOpMax [cond3] [cond1,cond2] 
          if_ cond3 
            (do 
              --op_ ArbbOpCopy [hit] [false]   
              op_ ArbbOpCopy [color] [black]
            )
            (do 
              --op_ ArbbOpCopy [hit] [true]   
              op_ ArbbOpCopy [color] [tcol]
            )
        )
          
  
        
bindToVar1D ptr s p t nom = 
  do 
   t' <- getScalarType_ t 
   dt <- getDenseType_ t' 1
   bin <- createDenseBinding_ (castPtr ptr) 1 [s{-size-}] [p{-pitch-}] 
   glob <- createGlobal_ dt nom bin
   variableFromGlobal_ glob
   

----------------------------------------------------------------------------
-- Main
main = do
  -- Test
   
  {- 
  withArray [1.0,1.0,1.0 :: Float] $ \ v1 -> 
    withArray [2.0,3.0,4.0 :: Float] $ \ v2 -> 
      allocaArray (3*4) $ \ r -> 
        do 
          arbbSession $ 
           do  
             f1 <- genArbbCrossProd
             f2 <- genRayTrigIntersect

             var1 <- bindToVar1D v1 (3 {-elements-}) 4 ArbbF32 "v1" 
             var2 <- bindToVar1D v2 3 4 ArbbF32 "v2" 
             res1 <- bindToVar1D r  3 4 ArbbF32 "r" 
          
             execute_ f1 [res1] [var1,var2] 
          result <- (peekArray 3 r :: IO [Float])
          putStrLn $ show result    
          putStrLn $ show (Vector3 1 1 1 `crossProd` Vector3 2 3 4)
  -}         
  {-
  withArray [0.0,0.0,-1.0 :: Float] $ \ d ->  
   withArray [250.0,250.0,0.0 :: Float] $ \ o -> 
    withArray [300.0,600.0,-800.0 :: Float] $ \ p0 -> 
     withArray [0.0,100.0,-1000.0 :: Float] $ \ p1 -> 
      withArray [450.0,20.0,-1000.0 :: Float] $ \ p2 -> 
       withArray [0.8,0.2,0.2 :: Float] $ \ c -> 
        withArray [0.0,0.0,0.0 :: Float] $ \ black -> 
         allocaArray (3*4) $ \ (r :: Ptr Float) -> 
           do 
            arbbSession $ 
             do  
               f1 <- genRayTrigIntersect

               d' <- bindToVar1D d (3 {-elements-}) 4 ArbbF32 "d" 
               o' <- bindToVar1D o 3 4 ArbbF32 "o" 
              
               p0' <- bindToVar1D p0 3 4 ArbbF32 "p0" 
               p1' <- bindToVar1D p1 3 4 ArbbF32 "p1" 
               p2' <- bindToVar1D p2 3 4 ArbbF32 "p2" 
               c' <- bindToVar1D  c  3 4 ArbbF32 "c" 
               black' <- bindToVar1D  black  3 4 ArbbF32 "c" 
               
              
               r' <- bindToVar1D r   3 4 ArbbF32 "r" 
              
               --sty  <- getScalarType_ ArbbI32
               --g    <- createGlobal_nobind_ sty "res" --outb
               --b    <- variableFromGlobal_ g 
          
               execute_ f1 [r'] [o',d',p0',p1',p2',c',black'] 
               result <- liftIO$ (peekArray 3 r :: IO [Float])
               --bresult :: Int32 <- readScalar_ b 
               liftIO$ putStrLn $ show result -- ++ " " ++ show bresult
  -}
  
  
  -- Trying to figure out if rayTrigIntersection works
  -- (Extremely slow running test) 
  withArray [0.0,0.0,-1.0 :: Float] $ \ d ->  
    withArray [300.0,600.0,-800.0 :: Float] $ \ p0 -> 
     withArray [0.0,100.0,-1000.0 :: Float] $ \ p1 -> 
      withArray [450.0,20.0,-1000.0 :: Float] $ \ p2 -> 
       withArray [0.8,0.2,0.2 :: Float] $ \ c -> 
        withArray [0.0,0.0,0.0 :: Float] $ \ black -> 
           do 
             
             rgbs <- sequence
               [do
                 withArray [x,(y+98),0::Float]  $ \ o -> 
                  allocaArray (3*4) $ \ (r :: Ptr Float) ->  
                   arbbSession $ 
                    do  
                
                      f1 <- genRayTrigIntersect

                      d' <- bindToVar1D d (3 {-elements-}) 4 ArbbF32 "d" 
                      o' <- bindToVar1D o 3 4 ArbbF32 "o" 
              
                      p0' <- bindToVar1D p0 3 4 ArbbF32 "p0" 
                      p1' <- bindToVar1D p1 3 4 ArbbF32 "p1" 
                      p2' <- bindToVar1D p2 3 4 ArbbF32 "p2" 
                      c'  <- bindToVar1D  c  3 4 ArbbF32 "c" 
                      black' <- bindToVar1D  black  3 4 ArbbF32 "b" 
               
                      r' <- bindToVar1D r 3 4 ArbbF32 "r" 
              
          
                      execute_ f1 [r'] [o',d',p0',p1',p2',c',black'] 
              
                      result <- liftIO$ (peekArray 3 r :: IO [Float])
                      liftIO$ putStrLn "*******************************************"
                      return result 
               | y <- [0..9], x <- [0..9]
               ]   
             putStrLn $ show $ length (concat rgbs)
             withBinaryFile "test.raw" WriteMode $ \ handle -> 
               withArray (concat rgbs) $ \ arr -> 
                 allocaArray (10*10*3) $ \ bytes -> do 
                  arbbSession (arbbConvert (castPtr arr) bytes) 
                  hPutBuf handle bytes (10*10*3) 
  
         
          
  
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