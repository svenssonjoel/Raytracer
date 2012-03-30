
module Trace.ONB where 


import Trace.Vector3

data ONB = ONB {onbU :: Vector3, 
                onbV :: Vector3, 
                onbW :: Vector3}
           

epsilon =  0.01 