      SUBROUTINE SMTR9(F1,F2)                                           
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    SMTR9       PERFORMS 9-PT SMOOTHING                    
C   PRGMMR: R. M. REAP       ORG: W/OSD21           DATE: 95-11-01      
C                                                                       
C ABSTRACT:  COMPUTES 9-PT SMOOTHED FIELD FROM INPUT FIELD              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-08-01  R. M. REAP                                                
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-01  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM SMTR9                                                     
C        NOV 1995       R. M. REAP         MDL          CRAY            
C        PURPOSE                                                        
C        COMPUTES A 9 POINT SMOOTHED FIELD FROM A GIVEN FIELD           
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      DIMENSION F1(26,33),F2(26,33)                                     
C        ***************************************************************
      DO 110 K=2,25                                                     
      DO 110 L=2,32                                                     
      F2(K,L)=(F1(K,L)+F1(K-1,L)+F1(K+1,L)+F1(K,L-1)+F1(K,L+1)          
     1+F1(K+1,L+1)+F1(K+1,L-1)+F1(K-1,L-1)+F1(K-1,L+1))/9.              
  110 CONTINUE                                                          
C        FORM OUTER BOUNDRY VALUES FOR 9 POINT SMOOTHED FIELD           
      DO 115 K=2,25                                                     
      F2(K,1)=(F1(K,1)*3.+F1(K-1,1)+F1(K+1,1))/5.                       
  115 CONTINUE                                                          
      DO 120 K=2,25                                                     
      F2(K,33)=(F1(K,33)*3.+F1(K-1,33)+F1(K+1,33))/5.                   
  120 CONTINUE                                                          
      DO 125 L=2,32                                                     
      F2(1,L)=(F1(1,L)*3.+F1(1,L-1)+F1(1,L+1))/5.                       
  125 CONTINUE                                                          
      DO 130 L=2,32                                                     
      F2(26,L)=(F1(26,L)*3.+F1(26,L-1)+F1(26,L+1))/5.                   
  130 CONTINUE                                                          
      F2(1,1)=(F2(2,1)+F2(1,2))/2.                                      
      F2(26,1)=(F2(26,2)+F2(25,1))/2.                                   
      F2(26,33)=(F2(26,32)+F2(25,33))/2.                                
      F2(1,33)=(F2(1,32)+F2(2,33))/2.                                   
      RETURN                                                            
      END                                                               
