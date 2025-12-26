      SUBROUTINE OBSFC(I,J,N,ISTA,IGO,RTEMP,RPRES)                      
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    OBSFC       COMPUTES LAPSE RATE AT SFC STATION         
C   PRGMMR:  R. M. REAP      ORG: W/OSD21            DATE: 95-11-02     
C                                                                       
C ABSTRACT:  COMPUTES LAPSE RATE AT SFC STATIONS BY WEIGHTED PLANE FIT  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   71-10-01  R. M. REAP                                                
C   74-04-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM OBSFC                                                     
C        NOV 1995         R. M. REAP           MDL          CRAY        
C        PURPOSE                                                        
C        COMPUTES LAPSE RATE AT SURFACE STATIONS BY APPLYING WEIGHTED   
C        PLANE FIT TO RAOB LAPSE RATES.  COMPUTES TEMPERATURE AT OPL    
C        (ORIGIN POINT LEVEL) BY APPLYING ANALYZED LAPSE RATE TO SURFACE
C        TEMPERATURE.  WHEN RAOB DENSITY INSUFFICIENT FOR LAPSE RATE    
C        ANALYSIS, APPLIES STANDARD LAPSE RATE TO SURFACE TEMPERATURE,  
C        PROVIDING OPL WITHIN 50 MB OF TERRAIN.                         
C        COMMON BLOCKS                                                  
C          KDMY,BLOCKB,BLOCKC,BLOCKE                                    
C        SUBPROGRAMS CALLED:                                            
C          INDXX                                                        
C         LIBRARY:                                                      
C          COMMON,W3LIB,TDLLIB                                           
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCKB/SFC(900,4),STAX(900),STAY(900),JSTA(900),JBLK(900), 
     1 KSTAP                                                         
      COMMON/BLOCKC/SFTD(26,33)                                         
      COMMON/BLOCKE/TERRA(51,65),SFTM(7),SFDP(7),DLPS(7),DIST(7),       
     1KSFC(7),PSFC(7)                                                   
      DIMENSION ISTA(7),RTEMP(150,40),RPRES(150,40)                     
C     ******************************************************************
      IGO=0                                                             
C        COMPUTE TERRAIN PRESSURE AT PARCEL ORIGIN POINT                
      XP=PLX(I,J,N)+PLX(I,J,N)-1.                                       
      YP=PLY(I,J,N)+PLY(I,J,N)-1.                                       
      LX=XP                                                             
      LY=YP                                                             
      DX=XP-LX                                                          
      DY=YP-LY                                                          
      LXP1=LX+1                                                         
      LYP1=LY+1                                                         
      CNR=TERRA(LY,LX)                                                  
      PSALT=CNR+(TERRA(LY,LXP1)-CNR)*DX+(TERRA(LYP1,LX)-CNR)*DY+        
     1(CNR+TERRA(LYP1,LXP1)-TERRA(LYP1,LX)-TERRA(LY,LXP1))*DX*DY        
      IF(PSALT-950.) 116,100,100                                        
  100 OPL=PLP(I,J,N)                                                    
      ALT=PSALT-100.                                                    
      IF(OPL-ALT) 116,104,104                                           
C        COMPUTE MEAN DISTANCE FROM RAOB TO TRAJECTORY ORIGIN POINT     
  104 DNM=0.0                                                           
      SUM=0.0                                                           
      YC=PLY(I,J,N)-26.0                                                
      YC=YC*YC                                                          
      XC=PLX(I,J,N)-24.0                                                
      XC=XC*XC                                                          
      F2=YC+XC                                                          
      SNLP1=1.0+((973.71-F2)/(973.71+F2))                               
      SCF=1.866/SNLP1                                                   
      DO 108 K=1,7                                                      
      IF(DIST(K)-9999.) 106,108,108                                     
  106 DIST(K)=(DIST(K)*SCF)*381.0                                       
      SUM=SUM+DIST(K)                                                   
      DNM=DNM+1                                                         
  108 CONTINUE                                                          
      IF(DNM)116,116,109                                                
  109 AVG=SUM/DNM                                                       
C        LOCATE SEVEN CLOSEST SURFACE STATIONS                          
      CALL INDXX(PLX(I,J,N),PLY(I,J,N),KSFC,1)                          
C        COMPUTE TERRAIN PRESSURE AT SURFACE STATIONS                   
      DO 111 K=1,7                                                      
      KND=KSFC(K)                                                       
      XP=STAX(KND)+STAX(KND)-1.                                         
      YP=STAY(KND)+STAY(KND)-1.                                         
      LX=XP                                                             
      LY=YP                                                             
      DX=XP-LX                                                          
      DY=YP-LY                                                          
      LXP1=LX+1                                                         
      LYP1=LY+1                                                         
      CNR=TERRA(LY,LX)                                                  
      PSFC(K)=CNR+(TERRA(LY,LXP1)-CNR)*DX+(TERRA(LYP1,LX)-CNR)*DY+      
     1(CNR+TERRA(LYP1,LXP1)-TERRA(LYP1,LX)-TERRA(LY,LXP1))*DX*DY        
  111 CONTINUE                                                          
C        CHECK OBSERVED SFC TEMPERATURES WITH NCEP 1000 MB 00-HR VALUES 
C        AND ELIMINATE REPORTS WHICH DIFFER BY 7C OR MORE               
      DO 1140 K=1,7                                                     
      IF(PSFC(K)-950.)1140,1102,1102                                    
 1102 KND=KSFC(K)                                                       
      TEST=SFC(KND,2)                                                   
      IF(TEST-9999.)1104,1140,1140                                      
 1104 EXT=ABS(TEST)                                                     
      IF(EXT-40.)1110,1106,1106                                         
 1106 SFC(KND,2)=SFC(KND,2)+99999.                                      
      GO TO 1140                                                        
 1110 XP=STAX(KND)                                                      
      YP=STAY(KND)                                                      
      LX=XP                                                             
      LY=YP                                                             
      DX=XP-LX                                                          
      DY=YP-LY                                                          
      LXP1=LX+1                                                         
      LYP1=LY+1                                                         
      CNR=SFTD(LY,LX)                                                   
      T1000=CNR+(SFTD(LY,LXP1)-CNR)*DX+(SFTD(LYP1,LX)-CNR)*DY+          
     1(CNR+SFTD(LYP1,LXP1)-SFTD(LYP1,LX)-SFTD(LY,LXP1))*DX*DY           
      DF=ABS(TEST-T1000)                                                
      IF(DF-7.0)1140,1112,1112                                          
 1112 SFC(KND,2)=SFC(KND,2)+99999.                                      
 1140 CONTINUE                                                          
      IF(AVG-600.) 120,112,112                                          
  112 ALT=PSALT-50.0                                                    
      IF(OPL-ALT) 116,180,180                                           
  116 IGO=1                                                             
      RETURN                                                            
C        COMPUTE RAOB LAPSE RATES FROM SURFACE TO OPL                   
  120 DO 140 K=1,7                                                      
      IF(DLPS(K)-9999.) 121,140,140                                     
  121 JND=ISTA(K)                                                       
      DO 130 L=1,2                                                      
      TBTM=RTEMP(JND,L)                                                 
      IF(TBTM-9999.)122,130,130                                         
  122 PBTM=RPRES(JND,L)                                                 
      IF(PBTM-950.)131,131,123                                          
  123 DELP=PBTM-OPL                                                     
      IF(DELP-10.)124,124,125                                           
  124 DLPS(K)=0.0                                                       
      GO TO 140                                                         
  125 DLPS(K)=(DLPS(K)-TBTM)/DELP                                       
C        CHECK FOR SUPERADIABATIC LAPSE RATE OR INVERSION GT 0.10 C/MB  
C             LOWEST 25 MB -- GROSS ERROR CHECK                         
C             25 MB-50 MB -- SUPERADIABATIC CHECK                       
C             50 MB AND ABOVE -- SUPERADIABATIC AND INVERSION CHECKS    
      IF(DLPS(K)-0.25)126,131,131                                       
  126 IF(DLPS(K)+0.25)131,131,1260                                      
 1260 IF(DELP-25.)140,140,127                                           
  127 IF(DELP-50.)129,129,128                                           
  128 IF(DLPS(K)-0.125)129,131,131                                      
  129 IF(DLPS(K)+0.125)131,131,140                                      
  130 CONTINUE                                                          
  131 DLPS(K)=9999.                                                     
  140 CONTINUE                                                          
C        COMPUTE LAPSE RATE AT SURFACE STATIONS BY COMPUTING FIVE       
C        STATION WEIGHTED AVERAGE FROM RAOB LAPSE RATES.                
C        SURFACE STATION IS NOT USED IF TERRAIN PRESSURE IS LESS THAN   
C        950 MB OR IF LOCATED WITHIN 0.2 GRID INTERVAL OF RAOB STATION, 
C        OR IF LAPSE RATES ARE MISSING AT FOUR (OR MORE) OF THE SEVEN   
C        NEARBY RAOB STATIONS.                                          
      DO 160 K=1,7                                                      
      SMN=0.0                                                           
      TMN=0.0                                                           
      SFTM(K)=9999.                                                     
      KK=0                                                              
      KND=KSFC(K)                                                       
      IF(PSFC(K)-950.)160,143,143                                       
  143 IF(SFC(KND,2)-9999.) 144,160,160                                  
  144 DO 150 L=1,7                                                      
      IF(DLPS(L)-9999.) 145,150,150                                     
  145 JND=ISTA(L)                                                       
      DY=YCORD(JND)-STAY(KND)                                           
      DX=XCORD(JND)-STAX(KND)                                           
      ADY=ABS(DY)                                                       
      ADX=ABS(DX)                                                       
      IF(ADY-0.2)146,146,148                                            
  146 IF(ADX-0.2)160,160,148                                            
  148 DYSQ=DY*DY                                                        
      DXSQ=DX*DX                                                        
      DSR=SQRT(DYSQ+DXSQ)                                               
      WTF=0.75/((DSR*DSR)+0.75)                                         
      SMN=SMN+WTF                                                       
      STM=DLPS(L)*WTF                                                   
      TMN=TMN+STM                                                       
      KK=KK+1                                                           
  150 CONTINUE                                                          
      IF(KK-3) 151,151,153                                              
  151 DO 152 M=1,7                                                      
      SFTM(M)=9999.                                                     
  152 CONTINUE                                                          
      IGO=1                                                             
      RETURN                                                            
  153 SFTM(K)=TMN/SMN                                                   
      IF(DELP-50.)156,156,155                                           
  155 IF(SFTM(K)-0.12)156,158,158                                       
  156 IF(SFTM(K)+0.12)158,158,160                                       
  158 SFTM(K)=9999.                                                     
  160 CONTINUE                                                          
C        COMPUTE TEMP AT OPL FOR SURFACE STATIONS                       
  162 DO 170 K=1,7                                                      
      KND=KSFC(K)                                                       
      IF(SFTM(K)-9999.) 164,170,170                                     
  164 DELP=PSFC(K)-OPL                                                  
      IF(DELP)166,167,167                                               
  166 DELP=0.0                                                          
  167 IF(SFC(KND,2)-9999.)169,168,168                                   
  168 SFTM(K)=9999.                                                     
      GO TO 170                                                         
  169 SFTM(K)=SFC(KND,2)+SFTM(K)*DELP                                   
  170 CONTINUE                                                          
      AI=0.0                                                            
      RETURN                                                            
C        COMPUTE TEMPERATURES AT OPL USING DRY-ADIABATIC LAPSE RATE OVER
C        WATER AND STANDARD ATMOSPHERE LAPSE RATE OVER LAND             
  180 DO 190 K=1,7                                                      
      SFTM(K)=9999.                                                     
      KND=KSFC(K)                                                       
      IF(SFC(KND,2)-9999.) 181,190,190                                  
  181 IF(PSFC(K)-950.)190,182,182                                       
  182 DELP=PSFC(K)-OPL                                                  
      IF(DELP)183,184,184                                               
  183 DELP=0.0                                                          
  184 KK=JSTA(KND)+JBLK(KND)                                            
      IF(KK)185,185,186                                                 
  185 SFTM(K)=SFC(KND,2)-.09*DELP                                       
      GO TO 190                                                         
  186 SFTM(K)=SFC(KND,2)-.054*DELP                                      
  190 CONTINUE                                                          
      AI=1.0                                                            
      RETURN                                                            
      END                                                               
