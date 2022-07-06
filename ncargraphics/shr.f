      FUNCTION SHR(PS)                                                
      IF(PS) 5,20,40                                            
    5 SS = 1.                                                  
      P3 = -36.*PS                                            
      P5 = -54.*PS                                        
   10 SHR= SS*(3.*SS+P3+SS**(-3))/(4.*SS + P5)           
      IF(ABS(SHR-SS).LT.1.0E-4) RETURN                
      SS = SHR                                     
      GO TO 10                              
   20 SHR= 1.0                        
      RETURN                                           
   40 SHR= 1. + 7.0*PS/(1. + PS)                   
      RETURN                                 
      END                               
