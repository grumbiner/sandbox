      SUBROUTINE FILTER (IMX,JMX,ZI,IPASS)                            
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    FILTER      FILTERS THE ARRAY ZI                       
C   PRGMMR: DIMEGO           ORG: W/NP22     DATE: 86-07-18             
C                                                                       
C ABSTRACT: FILTERS AN ARRAY USING A 25PT BLECK FILTER IN THE           
C   INTERIOR OF THE DOMAIN                                              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   86-07-18  G DIMEGO                                                  
C   88-09-23  B SCHMIDT ADDED THE DOCBLOCK                              
C   90-11-27  G DIMEGO LEFT Z AS INTERNAL WORK ARRAY ON CRAY
C   93-06-21  R TREADON STREAMLINED CODE
C                                                                       
C USAGE:    CALL FILTER (IMX, JMX, ZI, IPASS)                        
C   INPUT ARGUMENT LIST:                                                
C     ZI       - ARRAY CONTAINING THE ARRAY TO BE FILTERED              
C     IMX      - I DIMENSION OF ARRAYS ZI AND Z                  
C     JMX      - J DIMENSION OF ARRAYS ZI AND Z                  
C     IPASS    - NUMBER OF PASSES THROUGH THE FILTER                    
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     ZI       - ARRAY CONTAINING THE FILTERED FIELD                    
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: STANDARD FORTRAN                                          
C   MACHINE:                                                            
C                                                                       
C$$$                                                                    
      DIMENSION ZI(IMX,JMX),Z(IMX,JMX)                                
      LOGICAL BND,FST,SMTH                                              
C
      LI=IMX                                                            
      LJ=JMX                                                            
      I1 = 1                                                            
      J1 = 1                                                            
      I2 = I1 + 1                                                       
      J2 = J1 + 1                                                       
      I3 = I2 + 1                                                       
      LI1 = LI - 1                                                      
      LJ1 = LJ - 1                                                      
      LI2 = LI - 2                                                      
      LJ3 = LJ - 3                                                      
      LI4 = LI - 4                                                      
      LJ4 = LJ - 4                                                      
      LI5 = LI - 5                                                      
      IPC = IPASS                                                       
C
      DO 105 IP = 1,IPC                                                 
C **  FILTER REST OF INTERIOR POINTS   25-PT BLECK FILTER               
!$omp  parallel do
!$omp& private(i,ii,im1,im2,ip1,ip2,j,jm1,jm2,jp1,jp2)
         DO 102 JJ = J1,LJ4                                             
            JM2=JJ                                                      
            JM1=JJ+1                                                    
            J  =JJ+2                                                    
            JP1=JJ+3                                                    
            JP2=JJ+4                                                    
            DO 101 II = I1,LI4                                          
               IM2=II                                                   
               IM1=II+1                                                 
               I  =II+2                                                 
               IP1=II+3                                                 
               IP2=II+4                                                 
               Z(I,J) = 0.279372 * ZI(I,J)                              
     +   +0.171943*((ZI(IM1,J  )+ZI(I  ,JM1))+(ZI(IP1,J  )+ZI(I  ,JP1)))
     +   -0.006918*((ZI(IM2,J  )+ZI(I  ,JM2))+(ZI(IP2,J  )+ZI(I  ,JP2)))
     +   +0.077458*((ZI(IM1,JM1)+ZI(IP1,JP1))+(ZI(IP1,JM1)+ZI(IM1,JP1)))
     +   -0.024693*((ZI(IM1,JM2)+ZI(IP1,JM2))+(ZI(IM2,JM1)+ZI(IP2,JM1)) 
     +             +(ZI(IM2,JP1)+ZI(IP2,JP1))+(ZI(IM1,JP2)+ZI(IP1,JP2)))
     +   -0.01294 *((ZI(IM2,JM2)+ZI(IP2,JM2))+(ZI(IM2,JP2)+ZI(IP2,JP2)))
  101       CONTINUE                                                    
  102    CONTINUE                                                       
C
!$omp  parallel do
!$omp& private(i,j)
         DO 104 JJ= J1,LJ4                                              
            J = JJ + 2                                                  
            DO 103 II= I1,LI4                                           
               I = II + 2                                               
               ZI(I,J) = Z(I,J)                                         
 103        CONTINUE                                                    
 104     CONTINUE                                                       
C
 105  CONTINUE                                                          
      RETURN                                                            
      END                                                               
