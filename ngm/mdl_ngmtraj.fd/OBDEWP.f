      SUBROUTINE OBDEWP(RPRES,RTEMP,RDEWPT,STA,BLK,KSP)                 
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    OBDEWP       OBJECTIVE ANALYSIS/DEW POINTS             
C   PRGMMR: R. M. REAP        ORG: W/OSD21            DATE: 95-11-02    
C                                                                       
C ABSTRACT:  PERFORMS OBJ ANALYSIS OF DEW POINTS AT TRAJ ORIGIN POINTS  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   67-06-01  R. M. REAP                                                
C   71-10-01  INCLUDE SFC STATIONS IN OBJ ANALYSIS                      
C   74-04-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM OBDEWP                                                    
C        NOV 1995        R. M. REAP          MDL           CRAY         
C        PURPOSE                                                        
C        PROVIDES WEIGHTED PLANE FIT BY THE METHOD OF LEAST SQUARES TO  
C        DEW POINTS EXTRACTED FROM THE FIVE CLOSEST UPPER AIR STATIONS. 
C        A SIMPLE WEIGHTED AVERAGE IS USED IF MORE THAN FOUR OF THE SEVE
C        CLOSEST STATIONS ARE MISSING DATA.WHEN AVAILABLE,PSEUDO UPPER-A
C        DEW POINTS,DERIVED FROM SURFACE REPORTS BY A LAPSE RATE AND/OR 
C        DECISION TREE APPROACH,ARE INCLUDED IN THE WEIGHTED PLANE FIT. 
C        DATA SET USE:                                                  
C           FT80 (INPUT)                                                
C        COMMON BLOCKS                                                  
C           KDMY,BLOCKA,BLOCKB,BLOCKC,BLOCKE,BLOCKH,BLOCKQ              
C        SUBPROGRAMS CALLED:                                            
C           EXTRA,INTER,INDXX,DIAG                                      
C         LIBRARY:                                                      
C           COMMON,W3LIB,TDLLIB                                          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE:  FORTRAN 90                                               
C   MACHINE:   CRAY                                                     
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
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
     1STA(150),BLK(150),KSP(150),TAB(50),HT(50),WTFX(11),            
     2SDEW(11),DXS(11),DYS(11),AP(858),A(26,33),ANDP(5),PLVL(5),        
     3ANTM(5),ORDP(5),SDP(2,7),STM(2,7)                             
      EQUIVALENCE (AP(1),A(1,1))                           
      DATA PLVL/1000.,850.,700.,500.,300./                              
C     ******************************************************************
      KB=1                                                              
      DO 100 I=1,50                                                     
      TAB(I)=0.0                                                        
      HT(I)=0.0                                                         
  100 CONTINUE                                                          
C        READ 1000-,850-,700-,500-,AND 300-MB DEW PNTS FROM DISK FILE 80
      NRCD=101                                                          
      DO 112 K=1,5                                                      
      READ(80,REC=NRCD,ERR=104) (AP(KK),KK=1,858)                       
      GO TO 108                                                         
  104 PRINT 106                                                         
  106 FORMAT(//,10X,'OBDEWP DISK READ PROBLEM--NCEP DEW POINTS',//)     
  108 DO 110 I=1,26                                                     
      DO 110 J=1,33                                                     
      JPEDW(I,J,K)=A(I,J)                                               
  110 CONTINUE                                                          
      NRCD=NRCD+1                                                       
  112 CONTINUE                                                          
      DO 200 I=1,13                                                     
      DO 200 J=1,17                                                     
      DO 200 N=1,3                                                      
      CALL INDXX(PLX(I,J,N),PLY(I,J,N),ISTA,0)                          
      DO 115 K=1,7                                                      
      DLPS(K)=9999.                                                     
      DIST(K)=9999.                                                     
  115 CONTINUE                                                          
      L=0                                                               
      DEW=0.0                                                           
      SUM=0.0                                                           
      XBAR=0.0                                                          
      YBAR=0.0                                                          
      RDST=0.0                                                          
      RSTA=0.0                                                          
      DEWPT(I,J,N)=9999.                                                
      DO 130 K=1,7                                                      
      JND=ISTA(K)                                                       
C        COMPUTE RAOB DEW POINT AT ORIGIN POINT LEVEL                   
      CALL EXTRA(JND,PLP(I,J,N),TM,DP,RPRES,RTEMP,RDEWPT,STA,BLK,KSP    
     1,TAB,HT,KB)                                                       
      IF(DP-9999.) 120,130,130                                          
  120 DLPS(K)=DP                                                        
      L=L+1                                                             
      DYS(L)=YCORD(JND)-PLY(I,J,N)                                      
      DXS(L)=XCORD(JND)-PLX(I,J,N)                                      
      DYS2=DYS(L)*DYS(L)                                                
      DXS2=DXS(L)*DXS(L)                                                
C        DIST IS DISTANCE BETWEEN STATION AND TRAJECTORY ORIGIN POINT   
      DIST(K)=SQRT(DYS2+DXS2)                                           
      RDST=RDST+DIST(K)                                                 
      RSTA=RSTA+1.0                                                     
C        INTERPOLATE U AND V STATION                                    
      CALL INTER(JND,PLP(I,J,N),UCM,VCM)                                
C        WSPD IS WIND SPEED                                             
      WSPD=SQRT((UCM*UCM)+(VCM*VCM))                                    
C        QFAC IS UPSTREAM-DOWNSTREAM ENHANCEMENT FACTOR                 
      QFAC=ABS(((UCM*DYS(L))-(VCM*DXS(L)))/WSPD)                        
C        WTFX IS WEIGHTING FACTOR                                       
      WTFX(L)=0.75/((DIST(K)+QFAC)*(DIST(K)+QFAC)+0.75)                 
      SUM=SUM+WTFX(L)                                                   
      SDEW(L)=DP*WTFX(L)                                                
      DEW=DEW+SDEW(L)                                                   
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
      SDEW(K)=SDEW(K)*DFN                                               
  134 CONTINUE                                                          
      DEW=DEW*DFN                                                       
      SUM=SUM*DFN                                                       
C        LOCATE SEVEN CLOSEST SURFACE STATIONS                          
  136 CALL INDXX(PLX(I,J,N),PLY(I,J,N),KSFC,1)                          
C        COMPUTE DEW POINT DEPRESSIONS FROM F00 FILE DATA AT 1000-,     
C        850-, 700-, 500-, AND 300-MB AT OP AND AT 850-, AND 700-MB     
C        FOR SEVEN CLOSEST SURFACE STATIONS NEAR OP                     
      XP=PLX(I,J,N)                                                     
      YP=PLY(I,J,N)                                                     
      LX=XP                                                             
      LY=YP                                                             
      DX=XP-LX                                                          
      DY=YP-LY                                                          
      LXP1=LX+1                                                         
      LYP1=LY+1                                                         
      DO 143 K=1,5                                                      
      DO 1360 II=1,26                                                   
      DO 1360 JJ=1,33                                                   
      A(II,JJ)=JPEDW(II,JJ,K)                                           
 1360 CONTINUE                                                          
      CNR=A(LY,LX)                                                      
      ANDP(K)=CNR+(A(LY,LXP1)-CNR)*DX+(A(LYP1,LX)-CNR)*DY+              
     1(CNR+A(LYP1,LXP1)-A(LYP1,LX)-A(LY,LXP1))*DX*DY                    
      ORDP(K)=ANDP(K)                                                   
      IF(K-2)138,137,138                                                
  137 MM=1                                                              
      GO TO 140                                                         
  138 IF(K-3)143,139,143                                                
  139 MM=2                                                              
  140 DO 142 M=1,7                                                      
      KND=KSFC(M)                                                       
      XX=STAX(KND)                                                      
      YY=STAY(KND)                                                      
      JJ=XX                                                             
      II=YY                                                             
      DJ=XX-JJ                                                          
      DI=YY-II                                                          
      JJP1=JJ+1                                                         
      IIP1=II+1                                                         
      CON=A(II,JJ)                                                      
  142 SDP(MM,M)=CON+(A(II,JJP1)-CON)*DJ+(A(IIP1,JJ)-CON)*DI+            
     1(CON+A(IIP1,JJP1)-A(IIP1,JJ)-A(II,JJP1))*DJ*DI                    
  143 CONTINUE                                                          
      DO 1436 K=1,5                                                     
      DO 1430 II=1,26                                                   
      DO 1430 JJ=1,33                                                   
      A(II,JJ)=JPETM(II,JJ,K)                                           
 1430 CONTINUE                                                          
      CNR=A(LY,LX)                                                      
      ANTM(K)=CNR+(A(LY,LXP1)-CNR)*DX+(A(LYP1,LX)-CNR)*DY+              
     1(CNR+A(LYP1,LXP1)-A(LYP1,LX)-A(LY,LXP1))*DX*DY                    
      IF(K-2)1402,1400,1402                                             
 1400 MM=1                                                              
      GO TO 1406                                                        
 1402 IF(K-3)1436,1404,1436                                             
 1404 MM=2                                                              
 1406 DO 1410 M=1,7                                                     
      KND=KSFC(M)                                                       
      XX=STAX(KND)                                                      
      YY=STAY(KND)                                                      
      JJ=XX                                                             
      II=YY                                                             
      DJ=XX-JJ                                                          
      DI=YY-II                                                          
      JJP1=JJ+1                                                         
      IIP1=II+1                                                         
      CON=A(II,JJ)                                                      
 1410 STM(MM,M)=CON+(A(II,JJP1)-CON)*DJ+(A(IIP1,JJ)-CON)*DI+            
     1(CON+A(IIP1,JJP1)-A(IIP1,JJ)-A(II,JJP1))*DJ*DI                    
 1436 CONTINUE                                                          
      DO 1440 K=1,5                                                     
      ANDP(K)=ANTM(K)-ANDP(K)                                           
      IF(ANDP(K))1438,1440,1440                                         
 1438 ANDP(K)=0.0                                                       
 1440 CONTINUE                                                          
      DO 1442 M=1,2                                                     
      DO 1442 K=1,7                                                     
      SDP(M,K)=STM(M,K)-SDP(M,K)                                        
      IF(SDP(M,K))1441,1442,1442                                        
 1441 SDP(M,K)=0.0                                                      
 1442 CONTINUE                                                          
C        USE DECISION-TREE DATA TO MODIFY F00 1000-500 MB DEW POINT     
C        PROFILE (AT COORDINATES OF OP)                                 
      DO 1650 M=1,2                                                     
      ASUM=0.0                                                          
      WSUM=0.0                                                          
      KK=M+1                                                            
      DO 1620 K=1,7                                                     
      KND=KSFC(K)                                                       
      IF(M-2)1444,1500,1500                                             
C        850 MB DECISION TREE -- FIRST BRANCH -- PRESENT WEATHER (WW)   
 1444 JWW=SFC(KND,4)                                                    
      IF(JWW-22)1448,1446,1448                                          
 1446 DPS=3.0                                                           
      GO TO 1600                                                        
 1448 IF(JWW-21)1449,1456,1449                                          
 1449 IF(JWW-25)1450,1456,1450                                          
 1450 IF(JWW-26)1451,1456,1451                                          
 1451 IF(JWW-50)1453,1452,1452                                          
 1452 IF(JWW-75)1456,1456,1453                                          
 1453 IF(JWW-77)1454,1456,1454                                          
 1454 IF(JWW-79)1620,1455,1455                                          
 1455 IF(JWW-89)1456,1456,1620                                          
 1456 DPS=2.0                                                           
      GO TO 1600                                                        
C        700 MB DECISION TREE -- FIRST BRANCH -- PRESENT WEATHER (WW)   
 1500 IF(JWW-22)1505,1504,1505                                          
 1504 DPS=3.0                                                           
      GO TO 1600                                                        
 1505 IF(JWW-21)1506,1512,1506                                          
 1506 IF(JWW-79)1507,1512,1507                                          
 1507 IF(JWW-86)1508,1512,1508                                          
 1508 IF(JWW-60)1620,1509,1509                                          
 1509 IF(JWW-75)1512,1512,1620                                          
 1512 DPS=2.0                                                           
C        COMPUTE DISTANCE FROM SURFACE STATION TO OP                    
 1600 XDIS=STAX(KND)-PLX(I,J,N)                                         
      YDIS=STAY(KND)-PLY(I,J,N)                                         
      XSQ=XDIS*XDIS                                                     
      YSQ=YDIS*YDIS                                                     
      DSQ=XSQ+YSQ                                                       
C        COMPUTE WEIGHT FOR CORRECTION BY DECISION-TREE DATA            
      WTFN=0.75/(DSQ+0.75)                                              
C        SUM CORRECTIONS                                                
      DIFF=(DPS-SDP(M,K))                                               
      IF(DIFF)1610,1610,1604                                            
 1604 DIFF=0.0                                                          
 1610 ASUM=ASUM+WTFN*DIFF                                               
      WSUM=WSUM+WTFN                                                    
 1620 CONTINUE                                                          
C        APPLY CORRECTIONS TO 850-, AND 700-MB DEW POINTS               
      IF(WSUM)1650,1650,1630                                            
 1630 COR=ASUM/WSUM                                                     
      ANDP(KK)=ANDP(KK)+COR                                             
 1650 CONTINUE                                                          
      DO 1660 K=1,5                                                     
      ANDP(K)=ANTM(K)-ANDP(K)                                           
 1660 CONTINUE                                                          
      PR=PLP(I,J,N)                                                     
      IF(PR-1000.)1740,1740,1730                                        
 1730 PR=1000.                                                          
 1740 DO 145 K=1,5                                                      
      IF(PR-PLVL(K))145,145,144                                         
  144 LU=K                                                              
      GO TO 146                                                         
  145 CONTINUE                                                          
      GO TO 148                                                         
  146 LO=LU-1                                                           
      H=PLVL(LO)-PLVL(LU)                                               
      XO=PLVL(LO)                                                       
      G=(XO-PR)/H                                                       
      DANL=(1.-G)*ANDP(LO)+G*ANDP(LU)                                   
      TANL=(1.-G)*ANTM(LO)+G*ANTM(LU)                                   
      IF(DANL-TANL)1464,1464,1460                                       
 1460 DANL=TANL                                                         
 1464 DNEW=TANL-DANL                                                    
      DPAN=(1.-G)*ORDP(LO)+G*ORDP(LU)                                   
      IF(DPAN-TANL)1468,1466,1466                                       
 1466 BCN=0.20                                                          
      DIF=0.0                                                           
      GO TO 147                                                         
 1468 DOLD=TANL-DPAN                                                    
      DIF=DOLD-DNEW                                                     
      BCN=(DIF/DOLD)+0.05                                               
  147 L=L+1                                                             
      DXS(L)=0.0                                                        
      DYS(L)=0.0                                                        
      WTFX(L)=BCN+SKEW                                                  
      IF(WTFX(L)-1.0)1474,1474,1472                                     
 1472 WTFX(L)=1.0                                                       
 1474 SDEW(L)=DANL*WTFX(L)                                              
      DEW=DEW+SDEW(L)                                                   
      SUM=SUM+WTFX(L)                                                   
C        IF MORE THAN FOUR STATIONS MISSING DATA, ACCEPT WEIGHTED       
C        AVERAGE COMPUTED FROM RAOB AND F00 FILE DATA USING STATION     
C        DISTRIBUTION FUNCTION                                          
  148 IF(L)200,200,1480                                                 
 1480 DEWPT(I,J,N)=DEW/SUM                                              
      IF(L-4)200,1484,1484                                              
C        FORM LEAST SQUARES SUMS FOR RAOB AND F00 FILE DATA             
 1484 LL=L                                                              
      DXH=0.0                                                           
      DYH=0.0                                                           
      DXYH=0.0                                                          
      DXXH=0.0                                                          
      DYYH=0.0                                                          
      DXDP=0.0                                                          
      DYDP=0.0                                                          
      DO 149 K=1,L                                                      
      DYH=DYH+DYS(K)*WTFX(K)                                            
      DXH=DXH+DXS(K)*WTFX(K)                                            
      DXYH=DXYH+DXS(K)*DYS(K)*WTFX(K)                                   
      DXXH=DXXH+DXS(K)*DXS(K)*WTFX(K)                                   
      DYYH=DYYH+DYS(K)*DYS(K)*WTFX(K)                                   
      DXDP=DXDP+SDEW(K)*DXS(K)                                          
      DYDP=DYDP+SDEW(K)*DYS(K)                                          
  149 CONTINUE                                                          
      DO 1490 K=1,26                                                    
      DO 1490 M=1,33                                                    
      SFTD(K,M)=JPEDW(K,M,1)                                            
 1490 CONTINUE                                                          
      IF(JFL)172,1498,172                                               
 1498 CALL DIAG(I,J,N,ISTA,IGO,RDEWPT,RPRES)                            
      IF(IGO)172,150,172                                                
C        COMPUTE DISTANCE FROM SFC STATION TO TRAJECTORY ORIGIN POINT   
  150 DO 160 K=1,7                                                      
      IF(SFDP(K)-9999.) 154,160,160                                     
  154 KND=KSFC(K)                                                       
      L=L+1                                                             
      DYS(L)=STAY(KND)-PLY(I,J,N)                                       
      DXS(L)=STAX(KND)-PLX(I,J,N)                                       
      DYS2=DYS(L)*DYS(L)                                                
      DXS2=DXS(L)*DXS(L)                                                
      DSTN=SQRT(DYS2+DXS2)                                              
C        COMPUTE WEIGHTS FOR DEW POINTS DERIVED FROM SURFACE REPORTS    
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
      SDEW(L)=SFDP(K)*WTFX(L)                                           
      DEW=DEW+SDEW(L)                                                   
      IF(L-11) 160,162,162                                              
  160 CONTINUE                                                          
C        FORM LEAST SQUARES SUMS FOR SURFACE DATA AND ADD TO            
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
      DXDP=DXDP+SDEW(K)*DXS(K)                                          
      DYDP=DYDP+SDEW(K)*DYS(K)                                          
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
  174 C=DXDP*DYH-DEW*DXYH                                               
      F=DYDP*DYH-DEW*DYYH                                               
      PDEW=((B*F)-(C*E))/BDGE                                           
      DA=ABS(PDEW-DEWPT(I,J,N))                                         
      IF(DA-7.) 180,180,200                                             
  180 DEWPT(I,J,N)=PDEW                                                 
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                               
