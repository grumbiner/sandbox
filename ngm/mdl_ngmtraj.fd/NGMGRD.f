      SUBROUTINE NGMGRD(BUF2,D,AA)                                      
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    NGMGRD      TRANSFER WINDS TO TRAJ GRID                
C   PRGMMR: R. M. REAP       ORG: W/OSD21        DATE: 95-11-01         
C                                                                       
C ABSTRACT:  ORIENTS WINDS FROM NGM GRID TO MDL 26X33 GRID              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   87-11-01  R. M. REAP                                                
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-01  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C     SUBPROGRAM NGMGRD                                                 
C        NOV 1995       R. M. REAP         MDL          CRAY            
C        PURPOSE                                                        
C        POSITIONS DATA FROM NCEP 2385 POINT (45X53) NGM GRID (MESH     
C        LENGTH = 190.5 KM) TO MDL 858 POINT (26X33) GRID (MESH LENGTH =
C        381.0 KM). AT THIS POINT WINDS ARE ORIENTED WITH RESPECT       
C        TO NGM GRID.                                                   
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      DIMENSION BUF2(2385),D(45,53),AA(26,33),B(4)                      
C     ******************************************************************
      SFX=-0.42262                                                      
      CFX=0.90631                                                       
      CH=3.15018                                                        
      CK=18.48524                                                       
      KK=0                                                              
      DO 300 I=1,45                                                     
      DO 300 J=1,53                                                     
      KK=KK+1                                                           
      D(I,J)=BUF2(KK)                                                   
  300 CONTINUE                                                          
      DO 350 M=1,26                                                     
      DO 350 N=1,33                                                     
      AA(M,N)=9999.                                                     
      I=M+M-1                                                           
      J=N+N-1                                                           
      XP=((J-CH)*CFX+(I-CK)*SFX)+1.0                                    
      YP=((I-CK)*CFX-(J-CH)*SFX)+1.0                                    
      LP=XP                                                             
      KP=YP                                                             
      DX=XP-LP                                                          
      DY=YP-KP                                                          
      IF(LP-1)350,308,302                                               
  302 IF(LP-52)304,308,350                                              
  304 IF(KP-1)350,318,306                                               
  306 IF(KP-44)320,318,350                                              
  308 IF(KP-1)350,318,310                                               
  310 IF(KP-44)318,318,350                                              
C        START BI-LINEAR INTERPOLATION IN OUTSIDE GRID INTERVAL         
  318 LXP1=LP+1                                                         
      KYP1=KP+1                                                         
      CNR=D(KP,LP)                                                      
      DATUM=CNR+(D(KP,LXP1)-CNR)*DX+(D(KYP1,LP)-CNR)*DY+                
     1(CNR+D(KYP1,LXP1)-D(KYP1,LP)-D(KP,LXP1))*DX*DY                    
      AA(M,N)=DATUM                                                     
      GO TO 350                                                         
C        START BI-QUADRATIC INTERPOLATION                               
  320 KYP2=KP+2                                                         
      KYP1=KP+1                                                         
      KYM1=KP-1                                                         
      FAC=((DY*DY)-DY)/4.                                               
      FAD=((DX*DX)-DX)/4.                                               
      DO 330 K=1,4                                                      
      L=LP-2+K                                                          
      B(K)=D(KP,L)+(D(KYP1,L)-D(KP,L))*DY+(D(KYM1,L)+D(KYP2,L)          
     1-D(KP,L)-D(KYP1,L))*FAC                                           
  330 CONTINUE                                                          
      DATUM=B(2)+(B(3)-B(2))*DX+(B(1)+B(4)-B(2)-B(3))*FAD               
      AA(M,N)=DATUM                                                     
  350 CONTINUE                                                          
C        EXTRAPOLATE INTERIOR VALUES TO FILL NO-DATA POINTS ADJACENT    
C        TO NGM BOUNDARIES                                              
      DO 400 N=1,3                                                      
      DO 390 I=2,25                                                     
      DO 390 J=2,32                                                     
      ITAB=4                                                            
      TOT=0.0                                                           
      IF(AA(I,J)-9999.)382,354,382                                      
  354 KYP1=I+1                                                          
      IF(AA(KYP1,J)-9999.)356,358,356                                   
  356 TOT=TOT+AA(KYP1,J)                                                
      GO TO 360                                                         
  358 ITAB=ITAB-1                                                       
  360 KYM1=I-1                                                          
      IF(AA(KYM1,J)-9999.)362,364,362                                   
  362 TOT=TOT+AA(KYM1,J)                                                
      GO TO 366                                                         
  364 ITAB=ITAB-1                                                       
  366 KXP1=J+1                                                          
      IF(AA(I,KXP1)-9999.)368,370,368                                   
  368 TOT=TOT+AA(I,KXP1)                                                
      GO TO 372                                                         
  370 ITAB=ITAB-1                                                       
  372 KXM1=J-1                                                          
      IF(AA(I,KXM1)-9999.)374,376,374                                   
  374 TOT=TOT+AA(I,KXM1)                                                
      GO TO 378                                                         
  376 ITAB=ITAB-1                                                       
  378 IF(ITAB)382,382,380                                               
  380 TAB=ITAB                                                          
      AVG=TOT/TAB                                                       
      D(I,J)=AVG                                                        
      GO TO 390                                                         
  382 D(I,J)=AA(I,J)                                                    
  390 CONTINUE                                                          
      DO 394 I=2,25                                                     
      DO 394 J=2,32                                                     
      AA(I,J)=D(I,J)                                                    
  394 CONTINUE                                                          
  400 CONTINUE                                                          
      DO 410 I=1,26                                                     
      DO 410 J=1,33                                                     
      IF(AA(I,J)-9999.)410,404,410                                      
  404 AA(I,J)=0.0                                                       
  410 CONTINUE                                                          
      RETURN                                                            
      END                                                               
