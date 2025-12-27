      SUBROUTINE NGMWND                                                 
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    NGMWND      ROTATES NGM U,V TO TRAJ GRID               
C   PRGMMR: R. M. REAP       ORG: W/OSD21          DATE: 95-11-01       
C                                                                       
C ABSTRACT:  U,V WIND COMPONENTS ARE ORIENTED W.R.T. MDL 26X33 GRID     
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   87-11-01  R. M. REAP                                                
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   94-09-14  INCREASE JTAB, JTABX DIMENSIONS TO 340                    
C   95-03-15  INCREASE JTAB, JTABX DIMENSIONS TO 400                    
C   95-11-01  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM NGMWND                                                    
C        NOV 1995      R. M. REAP        MDL          CRAY              
C        PURPOSE                                                        
C        U AND V WIND COMPONENTS ORIENTED WITH RESPECT TO NGM           
C        GRID ARE ROTATED TO TRUE NORTH, THEN ROTATED TO MDL 26X33 GRID 
C        DATA SET USE                                                   
C              FT84 (OUTPUT)                                            
C        SUBPROGRAMS CALLED:                                            
C           RDPE                                                        
C         LIBRARY:                                                      
C           COMMON,W3LIB,TDLLIB                                          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      DIMENSION UCM(26,33),UC(858),VCM(26,33),VC(858),JPP(7),JII(2),    
     1AA(26,33),AP(858),JTAB(400,5),JTABX(25,400)                       
      COMMON/BLOCK3/JTAB,JTABX,KEY                                      
      EQUIVALENCE (AP(1),AA(1,1)),(UC(1),UCM(1,1)),(VC(1),VCM(1,1))     
      DATA JPP/160,133,112,80,48,40,149/,JII/48,49/                     
C     ****************************************************************  
      NFILE=84                                                          
      DO 200 KK=1,9                                                     
      JTAU=(KK-1)*6                                                     
      DO 200 NN=1,7                                                     
      KEY=999                                                           
      CALL RDPE(AP,JTAU,JPP(NN),JII(1),NFILE)                           
      IF(KEY.EQ.999) GO TO 200                                          
      KEY1=KEY                                                          
      DO 100 I=1,26                                                     
      DO 100 J=1,33                                                     
      UCM(I,J)=AA(I,J)                                                  
  100 CONTINUE                                                          
      CALL RDPE(AP,JTAU,JPP(NN),JII(2),NFILE)                           
      KEY2=KEY                                                          
      DO 102 I=1,26                                                     
      DO 102 J=1,33                                                     
      VCM(I,J)=AA(I,J)                                                  
  102 CONTINUE                                                          
C        ROTATE LFM U AND V WIND COMPONENTS TO TRUE NORTH               
      SFX=-0.42262                                                      
      CFX=0.90631                                                       
      CH=3.15018                                                        
      CK=18.48524                                                       
      DO 106 M=1,26                                                     
      DO 106 N=1,33                                                     
      I=M+M-1                                                           
      J=N+N-1                                                           
      XCR=((J-CH)*CFX+(I-CK)*SFX)+1.0                                   
      YCR=((I-CK)*CFX-(J-CH)*SFX)+1.0                                   
      RX=27.-XCR                                                        
      IF(RX)104,106,104                                                 
  104 RY=49.-YCR                                                        
      RSQ=SQRT(RX*RX+RY*RY)                                             
      UFAC=RX/RSQ                                                       
      VFAC=RY/RSQ                                                       
      H=UCM(M,N)                                                        
      UCM(M,N)=H*VFAC-VCM(M,N)*UFAC                                     
      VCM(M,N)=VCM(M,N)*VFAC+H*UFAC                                     
  106 CONTINUE                                                          
C        ROTATE U AND V WIND COMPONENTS TO MDL 26X33 GRID               
      C1=0.01745329                                                     
      C2=57.2957795                                                     
      DO 150 I=1,26                                                     
      DO 150 J=1,33                                                     
      IF(UCM(I,J))114,108,114                                           
  108 IF(VCM(I,J))110,110,112                                           
  110 DIR=0.0                                                           
      GO TO 120                                                         
  112 DIR=180.                                                          
      GO TO 120                                                         
  114 DIR=270.-C2*ATAN2(VCM(I,J),UCM(I,J))                              
  120 SPD=SQRT(UCM(I,J)*UCM(I,J)+VCM(I,J)*VCM(I,J))                     
      IF(DIR-360.)128,124,124                                           
  124 DIR=DIR-360.                                                      
C        AT THIS POINT DIR=WIND DIRECTION WITH RESPECT TO TRUE NORTH    
  128 DX=24.-J                                                          
      IF(DX)130,150,130                                                 
  130 DY=26.-I                                                          
      R=SQRT(DX*DX+DY*DY)                                               
      ARG=DX/R                                                          
      THETA=C2*ASIN(ARG)                                                
      DIR=DIR+THETA                                                     
C        AT THIS POINT DIR=WIND DIRECTION WITH RESPECT TO MODEL GRID    
      IF(DIR-360.)134,132,132                                           
  132 DIR=DIR-360.                                                      
  134 IF(DIR)138,140,140                                                
  138 DIR=DIR+360.                                                      
C        ARGUMENT IN RADIANS                                            
  140 ARG=C1*DIR                                                        
      UCM(I,J)=-1.0*SPD*SIN(ARG)                                        
      VCM(I,J)=-1.0*SPD*COS(ARG)                                        
  150 CONTINUE                                                          
      WRITE(84,REC=KEY1) (UC(K),K=1,858)                                
      WRITE(84,REC=KEY2) (VC(K),K=1,858)                                
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                               
