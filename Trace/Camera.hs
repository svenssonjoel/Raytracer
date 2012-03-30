
module Trace.Camera where 


import Trace.Vector3
import Trace.Ray
import Trace.ONB 


data Camera = 
  Camera { cameraCenter :: Vector3, 
           cameraCorner :: Vector3, 
           cameraAcross :: Vector3, 
           cameraONB    :: ONB,
           cameraLensRadius :: Float, 
           cameraU      :: Vector2, 
           cameraV      :: Vector2
           cameraD      :: Float }
  
  
           