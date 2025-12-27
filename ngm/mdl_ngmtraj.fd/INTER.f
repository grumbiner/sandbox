      SUBROUTINE INTER(JND,PSA,UCM,VCM)                                 
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    INTER       INTERPOLATES U,V TO RAOB STATION           
C   PRGMMR: R. M. REAP       ORG: W/OSD21           DATE: 95-11-02      
C                                                                       
C ABSTRACT:  INTERPOLATES U,V WINDS TO UPPER-AIR STATIONS               
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   68-02-01  R. M. REAP                                                
C   74-04-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM INTER                                                     
C        NOV 1995       R. M. REAP         MDL         CRAY             
C        PURPOSE                                                        
C        INTERPOLATES INITIAL PE U AND V WIND COMPONENTS TO UPPER-AIR   
C        STATIONS.  WINDS ARE USED TO COMPUTE THE UPSTREAM-DOWNSTREAM   
C        ENHANCEMENT TERM IN THE WEIGHT FACTOR.                         
C        VARIABLES                                                      
C             JND--CLOSEST STATION INDEX                                
C             PSA--PARCEL HEIGHT (MB)                                   
C        COMMON BLOCKS                                                  
C            KDMY,BLOCKD                                                
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR,KSFC,KSHP,KUPA           
      COMMON/BLOCKD/KU(26,33,5),KV(26,33,5)                             
      DIMENSION US(4,5),VS(4,5),P(5),UAR(5),VAR(5)                      
      DATA P/1000.,850.,700.,500.,300./                                 
C     ****************************************************************  
      LP=XCORD(JND)                                                     
      KP=YCORD(JND)                                                     
      DX=XCORD(JND)-LP                                                  
      DY=YCORD(JND)-KP                                                  
      LXP1=LP+1                                                         
      KYP1=KP+1                                                         
      ALT=PSA                                                           
      IF(ALT-1000.)12,12,10                                             
  10  ALT=1000.                                                         
  12  IF(ALT-300.)14,16,16                                              
  14  ALT=300.                                                          
  16  DO 20 I=2,5                                                       
      IF(ALT-P(I))20,18,18                                              
  18  N=I                                                               
      GO TO 22                                                          
  20  CONTINUE                                                          
  22  M=N-1                                                             
      DO 24 K=M,N                                                       
      L=0                                                               
      DO 24 I=KP,KYP1                                                   
      DO 24 J=LP,LXP1                                                   
      L=L+1                                                             
      US(L,K)=KU(I,J,K)*0.0001                                          
      VS(L,K)=KV(I,J,K)*0.0001                                          
  24  CONTINUE                                                          
      DO 26 K=M,N                                                       
      UAS=US(1,K)                                                       
      VAS=VS(1,K)                                                       
      UAR(K)=UAS+(US(2,K)-UAS)*DX+(US(3,K)-UAS)*DY+(UAS+US(4,K)-US(3,K)-
     1US(2,K))*DX*DY                                                    
      VAR(K)=VAS+(VS(2,K)-VAS)*DX+(VS(3,K)-VAS)*DY+(VAS+VS(4,K)-VS(3,K)-
     1VS(2,K))*DX*DY                                                    
  26  CONTINUE                                                          
      H=P(M)-P(N)                                                       
      XO=P(M)                                                           
      G=(XO-ALT)/H                                                      
      UCM=(1.-G)*UAR(M)+G*UAR(N)                                        
      VCM=(1.-G)*VAR(M)+G*VAR(N)                                        
      IF(UCM)32,28,32                                                   
  28  PRINT 30, UCM,XCORD(JND),YCORD(JND),PSA,H,XO,G,UAR(M),UAR(N),     
     1VAR(M),VAR(N)                                                     
  30  FORMAT(15X,'UCM=',F4.2,3F8.1,3F8.2,4F9.6)                         
      UCM=0.001                                                         
  32  IF(VCM)40,34,40                                                   
  34  PRINT 36, VCM,XCORD(JND),YCORD(JND),PSA,H,XO,G,UAR(M),UAR(N),     
     1VAR(M),VAR(N)                                                     
  36  FORMAT(15X,'VCM=',F4.2,3F8.1,3F8.2,4F9.6)                         
      VCM=0.001                                                         
  40  CONTINUE                                                          
      RETURN                                                            
      END                                                               
