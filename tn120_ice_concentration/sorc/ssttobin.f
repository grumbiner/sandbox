      PROGRAM oiread

      REAL SST(360,180)                                              
      PRINT *,'Entered program'

  200 READ (1,55,END=100) IYRST,IMST,IDST,IYREND,IMEND,IDEND,NDAYS,            
     1 INDEX                                                           
   55 FORMAT (8I5)                                                     
      READ (1,56) ((SST(I,J),I=1,360),J=1,180)                         
   56 FORMAT (20F4.2)                                                  
      AMAX =  -1.E30                                                   
      AMIN = -AMAX                                                     
      DO I=1,360                                                    
      DO J=1,180                                                    
      IF (AMAX.LT.SST(I,J)) AMAX = SST(I,J)                            
      IF (AMIN.GT.SST(I,J)) AMIN = SST(I,J)                            
      END DO
      END DO
      PRINT 7,IYRST,IMST,IDST,IYREND,IMEND,IDEND,NDAYS,INDEX           
    7 FORMAT ('0IYRST = ',I3,3X,'IMST = ',I3,3X,'IDST = ',I3,3X,       
     1 'IYREND = ',I3,3X,'IMEND = ',I3,3X,'IDEND = ',I3,3X,            
     2 'NDAYS = ',I3,3X,'INDEX = ',I3)                                 
      PRINT 33,AMAX,AMIN                                               
   33 FORMAT ('0SST MAX  =',F10.3,3X,'SST MIN  =',F10.3)               
    
      WRITE (10) SST
  100 STOP 1                                                           
      END                                                              
