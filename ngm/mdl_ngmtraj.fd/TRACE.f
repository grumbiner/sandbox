      SUBROUTINE TRACE(MXNR)                                            
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    TRACE       COMPUTES TEMP/DEWPT CHANGE DUE TO NVD      
C   PRGMMR: R. M. REAP       ORG: W/OSD21           DATE: 95-11-02      
C                                                                       
C ABSTRACT:  COMPUTES 6-H CHANGE OF TEMP/DEWPT DUE TO VERTICAL MOTIONS  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   67-03-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   89-09-20  CONVERT TO 36 & 48 H FORECAST PROJECTIONS                 
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM TRACE                                                     
C        NOV 1995         R. M. REAP         MDL          CRAY          
C        PURPOSE                                                        
C        SIX-HOURLY VARIATIONS OF TEMPERATURE (DEG K) AND MOISTURE (GM/ 
C        KG) ARE COMPUTED FOR PARCELS MOVING ALONG 3-D TRAJECTORIES. THE
C        PARCELS ARE LIFTED DRY-ADIABATICALLY PROVIDED THE MIXING RATIO 
C        DEFICIT REMAINS POSITIVE DURING THE PERIOD. LIFTING CONDENSA-  
C        TION LEVEL TEMPERATURES AND PRESSURES ARE COMPUTED WHEN SATURA-
C        TION OCCURS AS INDICATED BY A NEGATIVE MIXING RATIO DEFICIT.   
C        THE PARCELS ARE THEN LIFTED DRY-ADIABATICALLY TO THE LCL AND   
C        PSEUDO-ADIABATICALLY THEREAFTER. THE DEFINING EQUATION (ROSSBY,
C        1932) FOR THE PSEUDO-ADIABATIC PROCESS IS GIVEN BY THETA(PE)=  
C        THETA(P)*EXP(LR/CPT) WHERE THETA(PE) IS THE PSEUDO-EQUIVALENT  
C        POTENTIAL TEMPERATURE, THETA(P) IS THE PARTIAL POTENTIAL       
C        TEMPERATURE, L IS THE LATENT HEAT OF CONDENSATION, R IS THE    
C        MIXING RATIO, CP IS THE SPECIFIC HEAT OF AIR AT CONSTANT       
C        PRESSURE AND T IS THE TEMPERATURE (DEG K). SATURATION VAPOR    
C        PRESSURES (VAP1,SVAP1,SVAP2,VAP) ARE COMPUTED BY TETENS EQUA-  
C        TION AS DESCRIBED BY MURRAY (JAM,1967). MIXING RATIOS ARE GIVEN
C        BY R=0.622(E/P-E) WHERE E IS THE SATURATION VAPOR PRESSURE AND 
C        P IS THE TOTAL PRESSURE.                                       
C        DATA SET USE                                                   
C           FT82,FT87,FT88 (INPUT,OUTPUT)                               
C        COMMON BLOCKS                                                  
C           KDMY,BLOCKG                                                 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCKG/TEMP(13,17,4),DEWPT(13,17,4)                        
      COMMON/BLOCKP/KPRINT                                              
      DIMENSION PPP(11),PTEM(16),ERR(20),BUF1(13,17,4),BUF2(13,17,4),   
     1PREC(13,17)                                                       
C     ***************************************************************** 
      SFX=-0.42262                                                      
      CFX=0.90631                                                       
      CH=10.1                                                           
      CK=10.5                                                           
C        TEMPERATURE CORRECTION FOR DIABATIC UPSLOPE DISPLACEMENT       
      DO 100 I=1,13                                                     
      DO 100 J=1,17                                                     
      PREC(I,J)=0.0                                                     
      RHUM(I,J,2)=-RHUM(I,J,2)                                          
      RHUM(I,J,3)=-RHUM(I,J,3)                                          
      IF(RHUM(I,J,2)-20.)94,90,90                                       
  90  DT850=0.0625*RHUM(I,J,2)+2.75                                     
      GO TO 98                                                          
  94  DT850=0.2*RHUM(I,J,2)                                             
  98  DT700=0.09*RHUM(I,J,3)                                            
      IF(DT850-12.0)982,982,980                                         
  980 DT850=12.0                                                        
  982 IF(DT700-12.0)986,986,984                                         
  984 DT700=12.0                                                        
  986 TEMP(I,J,2)=TEMP(I,J,2)+DT850                                     
      TEMP(I,J,3)=TEMP(I,J,3)+DT700                                     
  100 CONTINUE                                                          
      DO 990 I=1,13                                                     
      DO 990 J=1,17                                                     
      DO 990 N=1,3                                                      
      IF(DEWPT(I,J,N)-TEMP(I,J,N))990,990,988                           
  988 DEWPT(I,J,N)=TEMP(I,J,N)                                          
  990 CONTINUE                                                          
      NR1=MXNR                                                          
      MTAB=INCR/6                                                       
      MM=MTAB-2                                                         
      ND=82                                                             
      IF(INCR.EQ.36) ND=87                                              
      IF(INCR.EQ.48) ND=88                                              
      KHR=0                                                             
      IF(KPRINT.EQ.0) GO TO 301                                         
      PRINT 300                                                         
  300 FORMAT(53X,'TEMPERATURE AND DEWPOINT')                            
  301 DO 60 M=1,MTAB                                                    
C        WRITE 6 HR 3D TEMPERATURES ON DISK                             
      NNT=22+M                                                          
      IF(INCR.EQ.36) NNT=25+M                                           
      IF(INCR.EQ.48) NNT=31+M                                           
      WRITE(ND,REC=NNT) (((TEMP(I,J,N),N=1,4),J=1,17),I=1,13)           
      IF(KPRINT.EQ.0) GO TO 306                                         
C        PRINT 6-HR TEMP AND DEW PT                                     
      PRINT 302, KHR                                                    
  302 FORMAT(1H0,61X,I2,' HR DATA')                                     
      KLVL=1000                                                         
      DO 305 N=1,3                                                      
      PRINT 304, KLVL,((I,J,TEMP(I,J,N),DEWPT(I,J,N),J=1,17),I=1,13)    
  304 FORMAT(1H0,63X,I5,//,(6(2I4,2F6.1,2X)))                           
      KLVL=KLVL-150                                                     
  305 CONTINUE                                                          
      PRINT 3050                                                        
 3050 FORMAT(1H1)                                                       
  306 KHR=KHR+6                                                         
      NR2=NR1-3                                                         
      IF(M-MM)210,210,200                                               
C        READ SUCCESSIVE 6-HR PARCEL X,Y COORDINATES FROM DISK          
  200 NR3=NR1-2                                                         
      NR4=NR1-1                                                         
      KST=0                                                             
      READ(ND,REC=NR3,ERR=201) (((PLX(I,J,N),N=1,4),J=1,17),I=1,13)     
      GO TO 202                                                         
  201 KST=1                                                             
  202 READ(ND,REC=NR4,ERR=203) (((PLY(I,J,N),N=1,4),J=1,17),I=1,13)     
      GO TO 204                                                         
  203 KST=1                                                             
  204 NR5=NR2-2                                                         
      NR6=NR2-1                                                         
      READ(ND,REC=NR5,ERR=205) (((BUF1(I,J,N),N=1,4),J=1,17),I=1,13)    
      GO TO 206                                                         
  205 KST=1                                                             
  206 READ(ND,REC=NR6,ERR=207) (((BUF2(I,J,N),N=1,4),J=1,17),I=1,13)    
      IF(KST)207,2072,207                                               
  207 PRINT 2070                                                        
 2070 FORMAT(10X,'TRACE DISK READ PROBLEM -- PARCEL X,Y CORD',//)       
C        COMPUTE SEGMENT MID-POINT                                      
 2072 DO 208 I=1,13                                                     
      DO 208 J=1,17                                                     
      DO 208 N=1,3                                                      
      PLX(I,J,N)=(PLX(I,J,N)+BUF1(I,J,N))*0.5                           
      PLY(I,J,N)=(PLY(I,J,N)+BUF2(I,J,N))*0.5                           
      PLP(I,J,N)=0.0                                                    
  208 CONTINUE                                                          
  210 CONTINUE                                                          
C        READ 6-HR PARCEL PRESSURES FROM DISK                           
      KST=0                                                             
      READ(ND,REC=NR1,ERR=220) (((BUF1(I,J,N),N=1,4),J=1,17),I=1,13)    
      GO TO 222                                                         
  220 KST=1                                                             
  222 READ(ND,REC=NR2,ERR=224) (((BUF2(I,J,N),N=1,4),J=1,17),I=1,13)    
      IF(KST)224,230,224                                                
  224 PRINT 226                                                         
  226 FORMAT(10X,'DISK READ PROBLEM--PARCEL 6-HR PRESSURES',//)         
  230 DO 50 I=1,13                                                      
      DO 50 J=1,17                                                      
      DO 50 N=1,3                                                       
      NN=N                                                              
      IF(DEWPT(I,J,N)-TEMP(I,J,N))8,8,6                                 
   6  DEWPT(I,J,N)=TEMP(I,J,N)                                          
   8  DEWPT1=DEWPT(I,J,N)+273.16                                        
      TEMP1=TEMP(I,J,N)+273.16                                          
      PRES1=BUF1(I,J,N)*0.1                                             
      PRES2=BUF2(I,J,N)*0.1                                             
      THETA=TEMP1*((100/PRES1)**0.286)                                  
C        COMPUTE MIXING RATIOS,VAPOR PRESSURES AND MIXING RATIO DEFICITS
      VAP1=0.61078*2.71828**((17.269*(DEWPT1-273.16))/(DEWPT1-35.86))   
      XXR1=0.622*(VAP1/(PRES1-VAP1))                                    
      SVAP1=0.61078*2.71828**((17.269*(TEMP1-273.16))/(TEMP1-35.86))    
      SMXR1=0.622*(SVAP1/(PRES1-SVAP1))                                 
      TEMP2=THETA*((PRES2/100.)**0.286)                                 
      SVAP2=0.61078*2.71828**((17.269*(TEMP2-273.16))/(TEMP2-35.86))    
      SMXR2=0.622*(SVAP2/(PRES2-SVAP2))                                 
      RHUM(I,J,N)=(XXR1/SMXR1)*100.                                     
      IF(PRES2-PRES1)9,11,11                                            
   9  IF(ABS(TEMP1-DEWPT1)-0.2)20,20,10                                 
  10  XXRDF1=SMXR1-XXR1                                                 
      XXRDF2=SMXR2-XXR1                                                 
      IF(XXRDF2)13,11,11                                                
C        COMPUTE DRY-ADIABATIC TEMPERATURE, DEW POINT AND REL. HUMIDITY 
  11  TEMP(I,J,N)=TEMP2-273.16                                          
      RHUM(I,J,N)=(XXR1/SMXR2)*100.                                     
      AVAP=(XXR1*PRES2)/(XXR1+0.622)                                    
      DDD=ALOG(AVAP)+0.49299                                            
      DEWPT(I,J,N)=((4717.20-(35.86*DDD))/(17.269-DDD))-273.16          
      GO TO 50                                                          
C        FIRST GUESS LCL PRESSURE, BARNES METHOD                        
  13  DEW=DEWPT(I,J,N)                                                  
      TEM=TEMP(I,J,N)                                                   
      CN=(0.001296*DEW)+0.1963                                          
      TLCL=DEW-CN*(TEM-DEW)                                             
      TLCL=TLCL+273.16                                                  
      PPP(1)=100.*((TLCL/THETA)**3.497)                                 
C        SOLVE POISSONS EQUATION FOR LCL PRESSURE (ITERATIVE METHOD).   
C        Q(1)=G(P(1)),P(2)=H(Q(1)),Q(2)=G(P(2)),P(3)=H(Q(2)).....       
      DO 16 K=1,10                                                      
      KP1=K+1                                                           
      QQQ=ALOG((PPP(K)*XXR1)/(0.61078*(0.622+XXR1)))                    
      PPP(KP1)=100.*(((35.86*QQQ)-4717.20)/(THETA*(QQQ-17.269)))**3.497 
      IF(ABS(PPP(KP1)-PPP(K))-0.1)17,17,16                              
  16  CONTINUE                                                          
  17  PLCL=PPP(KP1)                                                     
      TLCL=((35.86*QQQ)-4717.20)/(QQQ-17.269)                           
      GO TO 24                                                          
  20  TLCL=TEMP1                                                        
      PLCL=PRES1                                                        
C        COMPUTE PSEUDO-EQUIVALENT POTENTIAL TEMPERATURE                
  24  VAPL=0.61078*2.71828**((17.269*(TLCL-273.16))/(TLCL-35.86))       
      THETAP=TLCL*(100/(PLCL-VAPL))**0.286                              
      XXLCL=XXR1                                                        
      CP=0.238                                                          
      HL=596.73-0.601*(TLCL-273.16)                                     
      EQPTEM=THETAP*2.71828**((HL*XXLCL)/(CP*TLCL))                     
C        COMPUTE PSEUDO-ADIABATIC TEMP., DEW POINT AND REL. HUMIDITY    
      KP1=1                                                             
      PTEM(KP1)=TEMP2+0.4*(PRES1-PRES2)                                 
      DT=1.                                                             
      VAPL=0.61078*2.71828**((17.269*(PTEM(1)-273.16))/(PTEM(1)-35.86)) 
      THETAP=PTEM(1)*(100./(PRES2-VAPL))**0.286                         
      XXR2=0.622*(VAPL/(PRES2-VAPL))                                    
      HL=596.73-0.601*(PTEM(1)-273.16)                                  
      ERR(1)=(THETAP*2.71828**((HL*XXR2)/(CP*PTEM(1))))-EQPTEM          
      IF(ABS(ERR(1))-0.10)42,42,30                                      
  30  DO 40 K=1,15                                                      
      KP1=K+1                                                           
      PTEM(KP1)=PTEM(K)+DT                                              
      VAPL=0.61078*2.71828**((17.269*(PTEM(KP1)-273.16))/(PTEM(KP1)-    
     135.86))                                                           
      THETAP=PTEM(KP1)*(100./(PRES2-VAPL))**0.286                       
      XXR2=0.622*(VAPL/(PRES2-VAPL))                                    
      HL=596.73-0.601*(PTEM(KP1)-273.16)                                
      ERR(KP1)=(THETAP*2.71828**((HL*XXR2)/(CP*PTEM(KP1))))-EQPTEM      
      IF(ABS(ERR(KP1))-0.10)42,42,303                                   
  303 IF(ERR(KP1))32,33,31                                              
  31  IF(ERR(K))33,33,34                                                
  32  IF(ERR(K))34,33,33                                                
  33  IF(ABS(ERR(KP1))-1.0)331,332,332                                  
  331 DT=-DT*0.25                                                       
      GO TO 40                                                          
  332 DT=-DT*0.5                                                        
      GO TO 40                                                          
  34  IF(ABS(ERR(K))-ABS(ERR(KP1)))38,35,40                             
  35  PRINT 36                                                          
  36  FORMAT('TEMPERATURE ITERATION DOES NOT CONVERGE')                 
  38  DT=-2.*DT                                                         
  40  CONTINUE                                                          
  42  TEMP(I,J,N)=PTEM(KP1)-273.16                                      
      DEWPT(I,J,N)=PTEM(KP1)-273.16                                     
      RHUM(I,J,N)=100.                                                  
      IF(M-MM)50,50,44                                                  
C        SUM MIXING RATIO DEFICITS AT SURFACE, 850 MB, AND 700 MB       
  44  XM=PLX(I,J,N)                                                     
      YM=PLY(I,J,N)                                                     
      XPS=((XM-CH)*CFX+(YM-CK)*SFX)+1.0                                 
      YPS=((YM-CK)*CFX-(XM-CH)*SFX)+1.0                                 
      XJ=XPS+0.5                                                        
      YI=YPS+0.5                                                        
      JJ=XJ                                                             
      II=YI                                                             
      IF(JJ-1)50,45,45                                                  
  45  IF(JJ-17)46,46,50                                                 
  46  IF(II-1)50,47,47                                                  
  47  IF(II-13)48,48,50                                                 
  48  XDEF=XXR1-XXR2                                                    
      IF(XDEF)480,482,482                                               
  480 XDEF=0.0                                                          
  482 PLP(II,JJ,N)=PLP(II,JJ,N)+XDEF                                    
  50  CONTINUE                                                          
      NR1=NR1-3                                                         
      IF(M-MM)60,60,500                                                 
C        COMPUTE PRECIPITATION AMOUNTS (INCHES) FOR SURFACE-850 AND     
C        850-700 LAYERS AND SUM                                         
  500 DO 530 I=1,13                                                     
      DO 530 J=1,17                                                     
      PAV1=(BUF1(I,J,1)+BUF2(I,J,1))*0.5                                
      PAV2=(BUF1(I,J,2)+BUF2(I,J,2))*0.5                                
      PAV3=(BUF1(I,J,3)+BUF2(I,J,3))*0.5                                
      DP1=PAV1-PAV2                                                     
      DP2=PAV2-PAV3                                                     
      IF(DP1)510,512,512                                                
  510 DP1=0.0                                                           
  512 IF(DP2)514,516,516                                                
  514 DP2=0.0                                                           
  516 XBAR1=(PLP(I,J,1)+PLP(I,J,2))*0.5                                 
      XBAR2=(PLP(I,J,2)+PLP(I,J,3))*0.5                                 
      PREC(I,J)=PREC(I,J)+(((XBAR1*DP1)+(XBAR2*DP2))/2.49)              
      IF(NN-4)530,518,530                                               
  518 PAV4=(BUF1(I,J,4)+BUF2(I,J,4))*0.5                                
      DP3=PAV3-PAV4                                                     
      IF(DP3)520,522,522                                                
  520 DP3=0.0                                                           
  522 XBAR3=(PLP(I,J,3)+PLP(I,J,4))*0.5                                 
      PREC(I,J)=PREC(I,J)+((XBAR3*DP3)/2.49)                            
  530 CONTINUE                                                          
  60  CONTINUE                                                          
C        WRITE PRECIPITATION AMOUNTS ON DISK (LAST 12 H IN PROJECTION)  
      DO 62 I=1,13                                                      
      DO 62 J=1,17                                                      
      PLP(I,J,1)=PREC(I,J)                                              
  62  CONTINUE                                                          
      NR8=22                                                            
      IF(INCR.EQ.36) NR8=33                                             
      IF(INCR.EQ.48) NR8=41                                             
      WRITE(ND,REC=NR8) (((PLP(I,J,N),N=1,4),J=1,17),I=1,13)            
      IF(KPRINT.EQ.0) GO TO 310                                         
C        PRINT 24, 36, OR 48-HR TEMP AND DEW PT                         
      PRINT 302, KHR                                                    
      KLVL=1000                                                         
      DO 309 N=1,3                                                      
      PRINT 304, KLVL,((I,J,TEMP(I,J,N),DEWPT(I,J,N),J=1,17),I=1,13)    
      KLVL=KLVL-150                                                     
  309 CONTINUE                                                          
C        WRITE 24, 36, OR 48 H 3D TEMPERATURES ON DISK                  
  310 NR9=27                                                            
      IF(INCR.EQ.36) NR9=32                                             
      IF(INCR.EQ.48) NR9=40                                             
      WRITE(ND,REC=NR9) (((TEMP(I,J,N),N=1,4),J=1,17),I=1,13)           
      RETURN                                                            
      END
