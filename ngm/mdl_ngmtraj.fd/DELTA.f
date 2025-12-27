      SUBROUTINE DELTA(XPOS,YPOS,ZPOS,CRT,VDT,STM,TOT,TMC,STAV,PSTAR,   
     1 MXNR)                                                            
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    DELTA       COMPUTES LANDFALL/OVERWATER VARIABLES      
C   PRGMMR: R. M. REAP       ORG: W/OSD21            DATE: 95-11-02     
C                                                                       
C ABSTRACT:  COMPUTES PARCEL LANDFALL, OVERWATER DIST, AVG SEA-SFC TEMP 
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   70-06-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   89-09-20  CONVERT TO 36 & 48 H FORECAST PROJECTIONS                 
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM DELTA                                                     
C        NOV 1995       R. M. REAP          MDL           CRAY          
C          PURPOSE                                                      
C          COMPUTES (X,Y,P,T) COORDINATES AT PARCEL LANDFALL (SFC,850,  
C          700-MB).  COMPUTES TOTAL DISTANCE (SINGLE OR MULTIPLE        
C          SEGMENTS) OF PARCEL OVERWATER PATH.  COMPUTES TOTAL TIME OVER
C          WATER, PARCEL TEMPERATURE AT LANDFALL (SFC,850,700-MB), AND  
C          AVERAGE SEA-SURFACE TEMPERATURE ALONG SURFACE TRAJECTORY.    
C          VARIABLES                                                    
C             XPOS,YPOS,ZPOS--CONTAINS PARCEL COORDINATES AT LANDFALL   
C             (OR ORIGIN POINT IF INITIALLY OVER WATER).                
C             CRT--TIME OF LANDFALL (OR 00-HR IF PARCEL INITIALLY OVER  
C             WATER).                                                   
C             VDT--TOTAL DISTANCE COVERED BY SFC OVERWATER TRAJECTORY   
C             TOT--TOTAL TIME OVER WATER (HOURS)                        
C             TMC--PARCEL TEMPERATURE AT LANDFALL (OR 00-HR IF PARCEL   
C             INITIALLY OVER WATER).                                    
C             STM--SEA-SURFACE TEMPERATURE                              
C             STAV--AVERAGE SEA-SURFACE TEMPERATURE                     
C          DATA SET USE                                                 
C             FT80,FT82                                                 
C          SUBPROGRAMS CALLED:                                          
C             INTR                                                      
C           LIBRARY                                                     
C             COMMON,W3LIB,TDLLIB                                        
C                                                                       
C ATTRIBUTES:                                                           
C  LANGUAGE: FORTRAN 90                                                 
C  MACHINE:  CRAY                                                       
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      DIMENSION XPOS(13,17),YPOS(13,17),ZPOS(13,17),CRT(13,17),         
     1VDT(13,17),PLXX(13,17),PLYY(13,17),PLPP(13,17),STM(26,33),        
     2TOT(13,17),TMC(13,17,3),STAV(13,17,8),TEMI(13,17),TEMF(13,17),    
     3DUM(26,33),BUFA(26,33),BUFB(858),PETI(26,33,2),PETF(26,33,2),     
     4PSTI(26,33),PSTF(26,33),PSTAR(13,17),CSA(26,33),CSB(858)          
      EQUIVALENCE(BUFA(1,1),BUFB(1)),(CSA(1,1),CSB(1))                  
C     ***************************************************************** 
C                                                                       
C        STORE ABSOLUTE VALUE OF SEA-SURFACE TEMPERATURE IN DUM FOR USE 
C        IN INTERPOLATION SUBROUTINE INTR.  READ CSB FOR COASTLINE      
C        INTERPOLATION.                                                 
      DO 10 I=1,26                                                      
      DO 10 J=1,33                                                      
      DUM(I,J)=ABS(STM(I,J))                                            
  10  CONTINUE                                                          
      NRCD=93                                                           
      IF(INCR.EQ.36) GO TO 18                                           
      IF(INCR.EQ.48) GO TO 18                                           
      READ 15,(CSB(K),K=1,858)                                          
  15  FORMAT(13F6.1)                                                    
C        WRITE CSB TO DISK 80                                           
      WRITE(80,REC=NRCD) (CSB(K),K=1,858)                               
      GO TO 19                                                          
C        READ CSB FROM DISK 80 FOR 36 & 48 H PROJECTIONS                
  18  READ(80,REC=NRCD) (CSB(K),K=1,858)                                
C        INITIALIZE ARRAYS                                              
  19  DO 22 I=1,13                                                      
      DO 22 J=1,17                                                      
      TOT(I,J)=0.0                                                      
      VDT(I,J)=0.0                                                      
      PSTAR(I,J)=0.0                                                    
      CRT(I,J)=9999.                                                    
      XPOS(I,J)=0.0                                                     
      YPOS(I,J)=0.0                                                     
      ZPOS(I,J)=0.0                                                     
      DO 20 N=1,3                                                       
      TMC(I,J,N)=9999.                                                  
  20  CONTINUE                                                          
      DO 21 N=1,8                                                       
      STAV(I,J,N)=9999.                                                 
  21  CONTINUE                                                          
  22  CONTINUE
      ITR=0                                                             
      NR1=22                                                            
      IF(INCR.EQ.36) NR1=25                                             
      IF(INCR.EQ.48) NR1=31                                             
      NNR=15                                                            
      IF(INCR.EQ.36) NNR=21                                             
      IF(INCR.EQ.48) NNR=27                                             
      NRD=76                                                            
      NUM=95                                                            
      MTAB=INCR/6                                                       
      ND=82                                                             
      IF(INCR.EQ.36) ND=87                                              
      IF(INCR.EQ.48) ND=88                                              
      DO 200 K=1,MTAB                                                   
C        READ SUCCESSIVE 6-HR 3D TEMP FORECASTS FROM DISK               
      KST=0                                                             
      NR1=NR1+1                                                         
      READ(ND,REC=NR1,ERR=23) (((PLX(I,J,N),N=1,4),J=1,17),I=1,13)      
      GO TO 24                                                          
  23  KST=1                                                             
  24  NR2=NR1+1                                                         
      READ(ND,REC=NR2,ERR=26) (((PLY(I,J,N),N=1,4),J=1,17),I=1,13)      
      IF(KST)208,208,26                                                 
  26  PRINT 202                                                         
 202  FORMAT(10X,'SEAMOD DISK READ PROBLEM -- 6 HR 3D TEMPS',//)        
 208  DO 210 I=1,13                                                     
      DO 210 J=1,17                                                     
      TEMI(I,J)=PLX(I,J,1)                                              
      TEMF(I,J)=PLY(I,J,1)                                              
 210  CONTINUE                                                          
C        READ SUCCESSIVE 6-HR NCEP TEMP FORECASTS FROM DISK 80          
      DO 250 M=1,2                                                      
      NRD=NRD+1                                                         
      IF(NRD.EQ.92) NRD=115                                             
      DO 250 N=1,2                                                      
      NRD=NRD+1                                                         
      READ(80,REC=NRD,ERR=240) (BUFB(KK),KK=1,858)                      
      GO TO 244                                                         
 240  PRINT 242                                                         
 242  FORMAT(10X,'SEAMOD DISK READ PROBLEM--6 HR NCEP TEMPS',//)        
 244  DO 250 I=1,26                                                     
      DO 250 J=1,33                                                     
      GO TO(246,248),M                                                  
 246  PETI(I,J,N)=BUFA(I,J)                                             
      GO TO 250                                                         
 248  PETF(I,J,N)=BUFA(I,J)                                             
 250  CONTINUE                                                          
C        READ 6-HR NCEP SFC PRESSURES FROM DISK 80                      
      DO 258 M=1,2                                                      
      NUM=NUM+1                                                         
      IF(NUM.EQ.101) NUM=110                                            
      READ(80,REC=NUM,ERR=251) (BUFB(KK),KK=1,858)                      
      GO TO 253                                                         
 251  PRINT 252                                                         
 252  FORMAT(10X,'SEAMOD DISK READ PROBLEM--6 HR NCEP SFC PRESSURE',//) 
 253  DO 258 I=1,26                                                     
      DO 258 J=1,33                                                     
      GO TO(254,256),M                                                  
 254  PSTI(I,J)=BUFA(I,J)                                               
      GO TO 258                                                         
 256  PSTF(I,J)=BUFA(I,J)                                               
 258  CONTINUE                                                          
C        READ SUCCESSIVE 6-HR PARCEL X,Y,P COORDINATES FROM DISK ND     
      KST=0                                                             
      NRXX=NNR-5                                                        
      READ(ND,REC=NRXX,ERR=2580) (((PLX(I,J,N),N=1,4),J=1,17),I=1,13)   
      GO TO 2582                                                        
 2580 KST=1                                                             
 2582 NRYY=NNR-4                                                        
      READ(ND,REC=NRYY,ERR=2584) (((PLY(I,J,N),N=1,4),J=1,17),I=1,13)   
      GO TO 2586                                                        
 2584 KST=1                                                             
 2586 NRPP=NNR-3                                                        
      READ(ND,REC=NRPP,ERR=2588) (((PLP(I,J,N),N=1,4),J=1,17),I=1,13)   
      GO TO 259                                                         
 2588 KST=1                                                             
 259  DO 260 I=1,13                                                     
      DO 260 J=1,17                                                     
      PLXX(I,J)=PLX(I,J,1)                                              
      PLYY(I,J)=PLY(I,J,1)                                              
      PLPP(I,J)=PLP(I,J,1)                                              
 260  CONTINUE                                                          
      NRX=NNR-2                                                         
      READ(ND,REC=NRX,ERR=2600) (((PLX(I,J,N),N=1,4),J=1,17),I=1,13)    
      GO TO 2602                                                        
 2600 KST=1                                                             
 2602 NRY=NNR-1                                                         
      READ(ND,REC=NRY,ERR=2604) (((PLY(I,J,N),N=1,4),J=1,17),I=1,13)    
      GO TO 2606                                                        
 2604 KST=1                                                             
 2606 NRP=NNR                                                           
      READ(ND,REC=NRP,ERR=2608) (((PLP(I,J,N),N=1,4),J=1,17),I=1,13)    
      IF(KST)32,32,2608                                                 
 2608 PRINT 30                                                          
  30  FORMAT(10X,'SEAMOD DISK READ PROBLEM -- PARCEL X,Y,P COORD',//)   
C        CHECK INITIAL AND FINAL PARCEL POSITIONS (6-HR SEGMENT) TO SEE 
C        IF PATH IS OVER WATER.                                         
  32  DO 100 I=1,13                                                     
      DO 100 J=1,17                                                     
      N=1                                                               
      XIN=PLX(I,J,N)                                                    
      YIN=PLY(I,J,N)                                                    
      CALL INTR(CSA,XIN,YIN,STVL)                                       
      STMI=STVL                                                         
      XFN=PLXX(I,J)                                                     
      YFN=PLYY(I,J)                                                     
      CALL INTR(CSA,XFN,YFN,STVL)                                       
      STMF=STVL                                                         
      IF(STMI)60,60,62                                                  
  60  IF(STMF)100,100,64                                                
  62  IF(STMF)66,66,72                                                  
C        COMPUTE COORDINATES AT PARCEL LANDFALL FROM ARRAY CSA          
  64  ITR=1                                                             
  66  D0=ABS(STMI)                                                      
      D1=ABS(STMF)                                                      
      FAC=D0/(D0+D1)                                                    
      XC=XIN+FAC*(XFN-XIN)                                              
      YC=YIN+FAC*(YFN-YIN)                                              
      IF(XPOS(I,J))662,660,662                                          
  660 XPOS(I,J)=XC                                                      
      YPOS(I,J)=YC                                                      
C        COMPUTE TIME OF LANDFALL                                       
      HRIN=(15.-NNR)*2.0                                                
      HRFN=(18.-NNR)*2.0                                                
      CRT(I,J)=HRIN+FAC*6.0                                             
C        COMPUTE PARCEL PRESSURE AT LANDFALL                            
      PRIN=PLP(I,J,N)                                                   
      PRFN=PLPP(I,J)                                                    
      ZPOS(I,J)=PRIN+FAC*(PRFN-PRIN)                                    
C        COMPUTE SURFACE, 850, 700-MB LANDFALL TEMPERATURES             
      TMC(I,J,N)=TEMI(I,J)+FAC*(TEMF(I,J)-TEMI(I,J))                    
      DO 661 M=2,3                                                      
      KXC=XC                                                            
      KYC=YC                                                            
      DX=XC-KXC                                                         
      DY=YC-KYC                                                         
      KXP1=KXC+1                                                        
      KYP1=KYC+1                                                        
      MM=M-1                                                            
      CNR1=PETI(KYC,KXC,MM)                                             
      TMI=CNR1+(PETI(KYC,KXP1,MM)-CNR1)*DX+(PETI(KYP1,KXC,MM)-CNR1)*DY+(
     1CNR1+PETI(KYP1,KXP1,MM)-PETI(KYP1,KXC,MM)-PETI(KYC,KXP1,MM))*DX*DY
      CNR2=PETF(KYC,KXC,MM)                                             
      TMF=CNR2+(PETF(KYC,KXP1,MM)-CNR2)*DX+(PETF(KYP1,KXC,MM)-CNR2)*DY+(
     1CNR2+PETF(KYP1,KXP1,MM)-PETF(KYP1,KXC,MM)-PETF(KYC,KXP1,MM))*DX*DY
      TMC(I,J,M)=TMI+FAC*(TMF-TMI)                                      
      IF(M-2)661,6600,661                                               
 6600 CNR3=PSTI(KYC,KXC)                                                
      PEPI=CNR3+(PSTI(KYC,KXP1)-CNR3)*DX+(PSTI(KYP1,KXC)-CNR3)*DY+      
     1(CNR3+PSTI(KYP1,KXP1)-PSTI(KYP1,KXC)-PSTI(KYC,KXP1))*DX*DY        
      CNR4=PSTF(KYC,KXC)                                                
      PEPF=CNR4+(PSTF(KYC,KXP1)-CNR4)*DX+(PSTF(KYP1,KXC)-CNR4)*DY+      
     1(CNR4+PSTF(KYP1,KXP1)-PSTF(KYP1,KXC)-PSTF(KYC,KXP1))*DX*DY        
      PSTAR(I,J)=PEPI+FAC*(PEPF-PEPI)                                   
  661 CONTINUE                                                          
  662 IF(ITR-1)68,70,68                                                 
C        COMPUTE TOTAL TIME OVER WATER, AVERAGE SEA-SFC TEMPERATURE, AND
C        DISTANCE (GRID INTERVAL) COVERED BY OVERWATER TRAJECTORY       
  68  TOT(I,J)=TOT(I,J)+FAC*6.0                                         
      CALL INTR(DUM,XIN,YIN,STVL)                                       
      STAV(I,J,K)=STVL                                                  
      XCM=(XC-XIN)*(XC-XIN)                                             
      YCM=(YC-YIN)*(YC-YIN)                                             
      GO TO 74                                                          
  70  TOT(I,J)=TOT(I,J)+(1.0-FAC)*6.0                                   
      CALL INTR(DUM,XFN,YFN,STVL)                                       
      STAV(I,J,K)=STVL                                                  
      XCM=(XC-XFN)*(XC-XFN)                                             
      YCM=(YC-YFN)*(YC-YFN)                                             
      GO TO 74                                                          
  72  IF(XPOS(I,J))722,720,722                                          
  720 CRT(I,J)=0.0                                                      
      XPOS(I,J)=XIN                                                     
      YPOS(I,J)=YIN                                                     
      ZPOS(I,J)=PLP(I,J,N)                                              
      TMC(I,J,N)=TEMI(I,J)                                              
      DO 721 M=2,3                                                      
      KXC=XIN                                                           
      KYC=YIN                                                           
      DX=XIN-KXC                                                        
      DY=YIN-KYC                                                        
      KXP1=KXC+1                                                        
      KYP1=KYC+1                                                        
      MM=M-1                                                            
      CNR1=PETI(KYC,KXC,MM)                                             
      TMC(I,J,M)=CNR1+(PETI(KYC,KXP1,MM)-CNR1)*DX+(PETI(KYP1,KXC,MM)-   
     1CNR1)*DY+(CNR1+PETI(KYP1,KXP1,MM)-PETI(KYP1,KXC,MM)-PETI(KYC,KXP1,
     2MM))*DX*DY                                                        
      IF(M-2)721,7200,721                                               
 7200 CNR2=PSTI(KYC,KXC)                                                
      PSTAR(I,J)=CNR2+(PSTI(KYC,KXP1)-CNR2)*DX+(PSTI(KYP1,KXC)-CNR2)*DY+
     1(CNR2+PSTI(KYP1,KXP1)-PSTI(KYP1,KXC)-PSTI(KYC,KXP1))*DX*DY        
  721 CONTINUE                                                          
  722 TOT(I,J)=TOT(I,J)+6.0                                             
      CALL INTR(DUM,XIN,YIN,STV1)                                       
      CALL INTR(DUM,XFN,YFN,STV2)                                       
      STAV(I,J,K)=0.5*(STV1+STV2)                                       
      XCM=(XIN-XFN)*(XIN-XFN)                                           
      YCM=(YIN-YFN)*(YIN-YFN)                                           
  74  DIS=SQRT(XCM+YCM)                                                 
C        COMPUTE MAP SCALE FACTOR, CONVERT DISTANCE TO KILOMETERS       
      XAV=(XIN+XFN)*0.5                                                 
      YAV=(YIN+YFN)*0.5                                                 
      YI=(YAV-26.)*(YAV-26.)                                            
      XJ=(XAV-24.)*(XAV-24.)                                            
      F2=YI+XJ                                                          
      SNLP1=1.0+((973.71-F2)/(973.71+F2))                               
      SCF=1.866/SNLP1                                                   
      DIS=(DIS*SCF)*381.0                                               
      VDT(I,J)=VDT(I,J)+DIS                                             
  80  ITR=0                                                             
  100 CONTINUE                                                          
      NNR=NNR-3                                                         
      NRD=NRD-3                                                         
      NUM=NUM-1                                                         
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                               
