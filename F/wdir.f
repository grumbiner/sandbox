      FUNCTION wdir(U,V,D)                                            
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                          
C SUBPROGRAM:    WDIR        MEAN ICE-DRIFT VECTOR DIRECTION            
C   PRGMMR: TOWNSHEND        ORG: W/NMC411   DATE: 89-09-27             
C                                                                       
C ABSTRACT: CALCULATE DIRECTION OF MEAN ICE-DRIFT VECTOR.               
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   89-09-27  TOWNSHEND   ORIGINAL AUTHOR                               
C   92-03-06  GRUMBINE    REVISION TO CALL AS FUNCTION
C                                                                      
C USAGE:    CALL WDIR(U,V,D)                                            
C   INPUT ARGUMENT LIST:                                                
C     U    - E-W COMPONENT OF MEAN ICE-DRIFT VECTOR                     
C     V    - N-S COMPONENT OF MEAN ICE-DRIFT VECTOR                     
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     D    - DIRECTION (DEGREES) OF MEAN ICE-DRIFT VECTOR               
C                                                                       
C   LANGUAGE: FORTRAN 77                                                
C   MACHINE:  CRAY Y-MP
C
C LAST MODIFIED: 27 September 1989
C                                                                       
C$$$                                                                    
C                                                                       
      IMPLICIT none
      REAL*4  D, U, V, WDIR
      REAL*4 R, RADIAN
C                                                                       
      DATA  RADIAN/57.29578/                                            
C                                                                       
      IF (U.EQ.0.) GO TO 60                                        
      IF (U.LT.0.) GO TO 40                                        
      IF (V.LT.0.) GO TO 30                                        
      R=V/U                                                   
      D=270.-ATAN(R)*RADIAN                                        
      GO TO 70                                                          
C                                                                       
C     NW QUAD                                                           
C                                                                       
   30 CONTINUE                                                          
      R=V/U                                                   
      D=270.-ATAN(R)*RADIAN                                        
      GO TO 70                                                          
C                                                                       
C     NE QUAD                                                           
C                                                                       
   40 CONTINUE                                                          
      IF (V.GT.0.) GO TO 50                                        
      R=V/U                                                   
      D=90.-ATAN(R)*RADIAN                                         
      GO TO 70                                                          
C                                                                       
C     SE QUAD                                                           
C                                                                       
   50 CONTINUE                                                          
      R=V/U                                                   
      D=90.-ATAN(R)*RADIAN                                         
      GO TO 70                                                          
   60 CONTINUE                                                          
      IF (V.GT.0.) D=180.                                     
      IF (V.LT.0.) D=0.                                       
   70 CONTINUE                                                          

      WDIR = D
                                                    
      RETURN                                                            
      END                                                              
