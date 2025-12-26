      SUBROUTINE OBTEMP(RPRES,RTEMP,RDEWPT,STA,BLK,KSP)                 
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    OBTEMP      OBJECTIVE ANALYSIS/TEMPS                   
C   PRGMMR:  R. M. REAP      ORG: W/OSD21          DATE: 95-11-02       
C                                                                       
C ABSTRACT:  PERFORMS OBJ ANAL OF TEMPS BY WEIGHTED PLANE FIT           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   67-06-01  R. M. REAP                                                
C   74-04-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM OBTEMP                                                    
C        NOV 1995        R. M. REAP          MDL           CRAY         
C        PURPOSE                                                        
C        PROVIDES WEIGHTED PLANE FIT BY THE METHOD OF                   
C        LEAST SQUARES TO TEMPERATURES EXTRACTED FROM THE               
C        FIVE CLOSEST UPPER-AIR STATIONS. A SIMPLE WEIGHTED             
C        AVERAGE IS USED IF MORE THAN FOUR OF THE SEVEN CLOSEST         
C        STATIONS ARE MISSING DATA.  WHEN AVAILABLE, PSEUDO UPPER-AIR   
C        TEMPERATURES, DERIVED FROM SURFACE REPORTS BY A LAPSE RATE     
C        APPROACH, ARE INCLUDED IN THE WEIGHTED PLANE FIT.              
C        DATA SET USE                                                   
C           FT80, FT83 (INPUT)                                          
C        COMMON BLOCKS                                                  
C           KDMY,BLOCKA,BLOCKB,BLOCKC,BLOCKE,BLOCKH,BLOCKQ              
C        SUBPROGRAMS CALLED:                                            
C           INDXX,EXTRA,INTER,OBSFC                                     
C         LIBRARY:                                                      
C           COMMON,W3LIB,TDLLIB                                          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON  KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,  
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCKA/TEMP(13,17,4),DEWPT(13,17,4)                        
      COMMON/BLOCKB/SFC(900,4),STAX(900),STAY(900),JSTA(900),JBLK(900), 
     1 KSTAP                                                         
      COMMON/BLOCKC/SFTD(26,33)                                         
      COMMON/BLOCKE/TERRA(51,65),SFTM(7),SFDP(7),DLPS(7),DIST(7),       
     1KSFC(7),PSFC(7)                                                   
      COMMON/BLOCKH/JFL                                            
      COMMON/BLOCKQ/JPETM(26,33,5),JPEDW(26,33,5)                       
      DIMENSION ISTA(7),RPRES(150,40),RTEMP(150,40),RDEWPT(150,40),     
     1STA(150),BLK(150),KSP(150),TAB(50),HT(50),WTFX(11),PLVL(5),       
     2STEM(11),DXS(11),DYS(11),AP(858),A(26,33),ANTM(5)                 
      EQUIVALENCE (AP(1),A(1,1))                                        
      DATA PLVL/1000.,850.,700.,500.,300./                              
C     ****************************************************************  
      KB=1                                                              
      DO 100 I=1,50                                                     
      TAB(I)=0.0                                                        
      HT(I)=0.0                                                         
  100 CONTINUE                                                          
C        READ 1000-,850-,700-,500-,AND 300-MB NCEP TEMPS FROM FILE 80   
      NRCD=77                                                           
      DO 1300 K=1,5                                                     
      IF(K-4)1200,1100,1200                                             
 1100 NRCD=106                                                          
 1200 READ(80,REC=NRCD,ERR=1210) (AP(KK),KK=1,858)                      
      GO TO 1216                                                        
 1210 PRINT 1212                                                        
 1212 FORMAT(//,10X,'OBTEMP DISK READ PROBLEM--NCEP TEMPS',//)          
 1216 DO 1220 I=1,26                                                    
      DO 1220 J=1,33                                                    
      JPETM(I,J,K)=A(I,J)                                               
 1220 CONTINUE                                                          
      NRCD=NRCD+1                                                       
 1300 CONTINUE                                                          
      DO 1310 I=1,26                                                    
      DO 1310 J=1,33                                                    
      SFTD(I,J)=JPETM(I,J,1)                                            
 1310 CONTINUE                                                          
C        READ TERRAIN FROM DISK 83                                      
      NRCD=1                                                            
      READ(83,REC=NRCD,ERR=102) ((TERRA(I,J),J=1,65),I=1,51)            
      GO TO 110                                                         
  102 PRINT 104                                                         
  104 FORMAT(10X,'OBTEMP DISK READ PROBLEM--TERRAIN',//)                
  110 DO 200 I=1,13                                                     
      DO 200 J=1,17                                                     
      DO 200 N=1,3                                                      
      CALL INDXX(PLX(I,J,N),PLY(I,J,N),ISTA,0)                          
      DO 115 K=1,7                                                      
      DLPS(K)=9999.                                                     
      DIST(K)=9999.                                                     
  115 CONTINUE                                                          
      L=0                                                               
      TEM=0.0                                                           
      SUM=0.0                                                           
      XBAR=0.0                                                          
      YBAR=0.0                                                          
      RDST=0.0                                                          
      RSTA=0.0                                                          
      TEMP(I,J,N)=9999.                                                 
      DO 130 K=1,7                                                      
      JND=ISTA(K)                                                       
C        EXTRACT RAOB TEMPERATURE AT ORIGIN POINT LEVEL                 
      CALL EXTRA(JND,PLP(I,J,N),TM,DP,RPRES,RTEMP,RDEWPT,STA,BLK,KSP,
     1TAB,HT,KB)                                                        
      IF(TM-9999.) 120,130,130                                          
  120 DLPS(K)=TM                                                        
      L=L+1                                                             
      DYS(L)=YCORD(JND)-PLY(I,J,N)                                      
      DXS(L)=XCORD(JND)-PLX(I,J,N)                                      
      DYS2=DYS(L)*DYS(L)                                                
      DXS2=DXS(L)*DXS(L)                                                
C        DIST IS DISTANCE BETWEEN STATION AND TRAJECTORY ORIGIN POINT   
      DIST(K)=SQRT(DYS2+DXS2)                                           
      RDST=RDST+DIST(K)                                                 
      RSTA=RSTA+1.0                                                     
C        INTERPOLATE U AND V AT STATION                                 
      CALL INTER(JND,PLP(I,J,N),UCM,VCM)                                
C        WSPD IS WIND SPEED                                             
      WSPD=SQRT((UCM*UCM)+(VCM*VCM))                                    
C        QFAC IS UPSTREAM-DOWNSTREAM ENHANCEMENT FACTOR                 
      QFAC=ABS(((UCM*DYS(L))-(VCM*DXS(L)))/WSPD)                        
C        WTFX IS WEIGHTING FACTOR                                       
      WTFX(L)=0.75/((DIST(K)+QFAC)*(DIST(K)+QFAC)+0.75)                 
      SUM=SUM+WTFX(L)                                                   
      STEM(L)=TM*WTFX(L)                                                
      TEM=TEM+STEM(L)                                                   
      XCN=XCORD(JND)                                                    
      YCN=YCORD(JND)                                                    
      XBAR=XBAR+(XCN*WTFX(L))                                           
      YBAR=YBAR+(YCN*WTFX(L))                                           
      IF(L-5) 130,132,132                                               
  130 CONTINUE                                                          
      IF(L)131,131,132                                                  
  131 SKEW=0.0                                                          
      GO TO 136                                                         
C        COMPUTE CENTROID COORDINATES FOR RAOB STATIONS BY SUMMING      
C        MOMENTS ABOUT EACH AXIS AND DIVIDING BY SUMMED WEIGHTS         
  132 XBAR=XBAR/SUM                                                     
      YBAR=YBAR/SUM                                                     
      XMO=XBAR-PLX(I,J,N)                                               
      YMO=YBAR-PLY(I,J,N)                                               
      XMSQ=XMO*XMO                                                      
      YMSQ=YMO*YMO                                                      
      VMAG=SQRT(XMSQ+YMSQ)                                              
C        COMPUTE AVERAGE DISTANCE FROM OP TO RAOB STATIONS              
      ADIS=RDST/RSTA                                                    
C        COMPUTE RAOB DISTRIBUTION FUNCTION (WRT OP)                    
      SKEW=VMAG/ADIS                                                    
C        APPLY STATION DISTRIBUTION FUNCTION TO RAOB DATA               
      DFN=(1.-SKEW)                                                     
      DO 134 K=1,L                                                      
      WTFX(K)=WTFX(K)*DFN                                               
      STEM(K)=STEM(K)*DFN                                               
  134 CONTINUE                                                          
      TEM=TEM*DFN                                                       
      SUM=SUM*DFN                                                       
C        COMPUTE WEIGHTED TEMPERATURE AT OP FROM F00 DATA (ON FILE 15)  
  136 DO 143 K=1,5                                                      
      DO 138 II=1,26                                                    
      DO 138 JJ=1,33                                                    
      A(II,JJ)=JPETM(II,JJ,K)                                           
  138 CONTINUE                                                          
      XP=PLX(I,J,N)                                                     
      YP=PLY(I,J,N)                                                     
      LX=XP                                                             
      LY=YP                                                             
      DX=XP-LX                                                          
      DY=YP-LY                                                          
      LXP1=LX+1                                                         
      LYP1=LY+1                                                         
      CNR=A(LY,LX)                                                      
      ANTM(K)=CNR+(A(LY,LXP1)-CNR)*DX+(A(LYP1,LX)-CNR)*DY+              
     1(CNR+A(LYP1,LXP1)-A(LYP1,LX)-A(LY,LXP1))*DX*DY                    
  143 CONTINUE                                                          
      PR=PLP(I,J,N)                                                     
      IF(PR-1000.)1440,1430,1430                                        
 1430 TANL=ANTM(1)                                                      
      GO TO 147                                                         
 1440 DO 145 K=1,5                                                      
      IF(PR-PLVL(K))145,144,144                                         
  144 LU=K                                                              
      GO TO 146                                                         
  145 CONTINUE                                                          
      GO TO 148                                                         
  146 LO=LU-1                                                           
      H=PLVL(LO)-PLVL(LU)                                               
      XO=PLVL(LO)                                                       
      G=(XO-PR)/H                                                       
      TANL=(1.-G)*ANTM(LO)+G*ANTM(LU)                                   
  147 L=L+1                                                             
      DXS(L)=0.0                                                        
      DYS(L)=0.0                                                        
      BCN=0.20                                                          
      WTFX(L)=BCN+SKEW                                                  
      IF(WTFX(L)-1.0)1474,1474,1472                                     
 1472 WTFX(L)=1.0                                                       
 1474 STEM(L)=TANL*WTFX(L)                                              
      TEM=TEM+STEM(L)                                                   
      SUM=SUM+WTFX(L)                                                   
C        IF MORE THAN FOUR STATIONS MISSING DATA, ACCEPT WEIGHTED       
C        AVERAGE COMPUTED FROM RAOB AND F00 FILE DATA USING STATION     
C        DISTRIBUTION FUNCTION                                          
  148 IF(L)200,200,1480                                                 
 1480 TEMP(I,J,N)=TEM/SUM                                               
      IF(L-4)200,1484,1484                                              
C        FORM LEAST SQUARES SUMS FOR RAOB AND F00 FILE DATA             
 1484 LL=L                                                              
      DXH=0.0                                                           
      DYH=0.0                                                           
      DXYH=0.0                                                          
      DXXH=0.0                                                          
      DYYH=0.0                                                          
      DXTM=0.0                                                          
      DYTM=0.0                                                          
      DO 149 K=1,L                                                      
      DYH=DYH+DYS(K)*WTFX(K)                                            
      DXH=DXH+DXS(K)*WTFX(K)                                            
      DXYH=DXYH+DXS(K)*DYS(K)*WTFX(K)                                   
      DXXH=DXXH+DXS(K)*DXS(K)*WTFX(K)                                   
      DYYH=DYYH+DYS(K)*DYS(K)*WTFX(K)                                   
      DXTM=DXTM+STEM(K)*DXS(K)                                          
      DYTM=DYTM+STEM(K)*DYS(K)                                          
  149 CONTINUE                                                          
      IF(JFL)172,1490,172                                               
 1490 CALL OBSFC(I,J,N,ISTA,IGO,RTEMP,RPRES)                            
      IF(IGO) 172,150,172                                               
C        COMPUTE DISTANCE FROM SFC STATION TO TRAJECTORY ORIGIN POINT   
  150 DO 160 K=1,7                                                      
      IF(SFTM(K)-9999.) 154,160,160                                     
  154 KND=KSFC(K)                                                       
      L=L+1                                                             
      DYS(L)=STAY(KND)-PLY(I,J,N)                                       
      DXS(L)=STAX(KND)-PLX(I,J,N)                                       
      DYS2=DYS(L)*DYS(L)                                                
      DXS2=DXS(L)*DXS(L)                                                
      DSTN=SQRT(DYS2+DXS2)                                              
C        COMPUTE WEIGHTS FOR TEMPERATURES DERIVED FROM SURFACE REPORTS  
      PSTA=PSFC(K)-PLP(I,J,N)                                           
      IF(PSTA)155,156,156                                               
  155 PSTA=0.0                                                          
      GO TO 158                                                         
  156 IF(PSTA-100.)158,158,157                                          
  157 PSTA=100.                                                         
  158 WTHD=1.0-(PSTA*0.01)                                              
      WTFX(L)=WTHD*(0.75/(DSTN*DSTN+0.75))                              
      DFAC=1.0                                                          
      IF(AI)159,159,1590                                                
  159 DFAC=(1.-SKEW)                                                    
 1590 WTFX(L)=DFAC*WTFX(L)                                              
      SUM=SUM+WTFX(L)                                                   
      STEM(L)=SFTM(K)*WTFX(L)                                           
      TEM=TEM+STEM(L)                                                   
      IF(L-11) 160,162,162                                              
  160 CONTINUE                                                          
C        ADD LEAST SQUARES SUMS FROM SURFACE DATA TO                    
C        SUMS PREVIOUSLY CALCULATED FROM RAOB AND                       
C        F00 FILE DATA                                                  
  162 IF(L-LL) 172,172,164                                              
  164 LLL=LL+1                                                          
      DO 170 K=LLL,L                                                    
      DYH=DYH+DYS(K)*WTFX(K)                                            
      DXH=DXH+DXS(K)*WTFX(K)                                            
      DXYH=DXYH+DXS(K)*DYS(K)*WTFX(K)                                   
      DXXH=DXXH+DXS(K)*DXS(K)*WTFX(K)                                   
      DYYH=DYYH+DYS(K)*DYS(K)*WTFX(K)                                   
      DXTM=DXTM+STEM(K)*DXS(K)                                          
      DYTM=DYTM+STEM(K)*DYS(K)                                          
  170 CONTINUE                                                          
      IF(L-11)800,172,172                                               
  800 KK=L+1                                                            
      DO 2000 K=KK,11                                                   
      WTFX(K)=9999.                                                     
 2000 CONTINUE                                                          
C        COMPLETE LEAST SQUARES FIT                                     
  172 D=DYH*DYH-SUM*DYYH                                                
      E=DXYH*DYH-DXH*DYYH                                               
      G=DXH*DYH-SUM*DXYH                                                
      B=DXXH*DYH-DXH*DXYH                                               
      BDGE=(B*D)-(G*E)                                                  
      IF(BDGE)174,200,174                                               
  174 C=DXTM*DYH-TEM*DXYH                                               
      F=DYTM*DYH-TEM*DYYH                                               
      PTEM=((B*F)-(C*E))/BDGE                                           
      TA=ABS(PTEM-TEMP(I,J,N))                                          
      IF(TA-7.) 180,180,200                                             
  180 TEMP(I,J,N)=PTEM                                                  
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                               
