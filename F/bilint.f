      SUBROUTINE BILINT(X,Y,SSST,SSTI,IDIM,JDIM)                                
      DIMENSION SSST(IDIM,JDIM)                                            
C                                                                       
      SAVE
C                                                                       
c      write(6,601) 
 601  format(1h ,'  BEGIN BILINT')
C                                                                       
      ID = IDIM                                                          
      JD = JDIM                                                           
      I = X                                                             
      J = Y                                                             
      YJ = J                                                            
      XI = I                                                            
C                                                                       
      SSTI= 99.                                                         
      IF(I.LT.1.OR.I.GE.ID) GO TO 10               
      IF(J.LT.1.OR.J.GE.JD) GO TO 10                 
C                                                                       
      S =  SSST(I,J)                                                    
      SIP = SSST(I+1,J)                                                 
      SJP = SSST(I,J+1)                                                 
      SIPJP = SSST(I+1,J+1)                                             
      A = S                                                             
      B = SIP - S                                                       
      C = SJP - S                                                       
      D = S + SIPJP - SIP - SJP                                         
      SSTI=A + B*(X-XI) + C*(Y-YJ) +D*(X-XI)*(Y-YJ)                     
C                                                                       
   10 CONTINUE                    
c
c       write(6,699) 
  699  format(1h ,'END OF BILINT')                                      
C                                                                       
      RETURN                                                            
      END                                                               
