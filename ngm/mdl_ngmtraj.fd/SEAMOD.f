      SUBROUTINE SEAMOD(MXNR)                                           
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    SEAMOD      COMPUTES AIR-SEA HEAT/MOISTURE FLUX        
C   PRGMMR:  R. M. REAP      ORG: W/OSD21           DATE: 95-11-02      
C                                                                       
C ABSTRACT:  COMPUTES AIR MASS TRANSFORMATION DUE TO AIR-SEA HEAT FLUX  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   70-06-01 R. M. REAP                                                 
C   74-03-01 CONVERT TO IBM SYSTEM                                      
C   89-09-20 CONVERT TO 36 & 48 H FORECAST PROJECTIONS                  
C   92-09-18 CONVERT TO FORTRAN 77                                      
C   95-11-02 CONVERT TO CRAY                                            
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM SEAMOD                                                    
C        NOV 1995       R. M. REAP         MDL         CRAY             
C        PURPOSE                                                        
C        COMPUTES AIR MASS TRANSFORMATION RESULTING FROM VERTICAL FLUX  
C        OF LATENT AND SENSIBLE HEAT FROM OCEAN TO OVERLYING ATMOSPHERE.
C        TRANSFORMATION PROCEDURE IS BASED ON PHYSICAL-EMPIRICAL MODEL  
C        DEVELOPED BY BURKE (J. OF MET. 1945) AND LATER SIMPLIFIED FOR  
C        COMPUTER BY DUQUET (J. OF MET. 1960).                          
C        VARIABLES                                                      
C             XDIF (DEG K) = FINAL SFC AIR TEMP - INITIAL SFC AIR TEMP  
C             YDIF (DEG K) = MEAN SEA-SFC TEMP - INITIAL SFC AIR TEMP   
C             VDT (KM) = DISTANCE COVERED BY OVERWATER TRAJECTORY       
C             BLPS (DEG) = INITIAL LAPSE RATE                           
C             BETA (DEG) = VALUE OF BLPS WHEN LAPSE RATE IS ADIABATIC   
C             STM (DEG C) = SEA-SURFACE TEMPERATURE                     
C        DATA SET USE                                                   
C           FT80,FT82 (INPUT)                                           
C        COMMON BLOCKS                                                  
C           KDMY,BLOCKG                                                 
C        SUBPROGRAMS CALLED:                                            
C           DELTA                                                       
C         LIBRARY                                                       
C           COMMON,W3LIB,TDLLIB                                          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCKG/TEMP(13,17,4),DEWPT(13,17,4)                        
      EQUIVALENCE (STM(1,1),DTM(1))                                     
      DIMENSION STM(26,33),DTM(858),REM(50),VDT(13,17),CRT(13,17),      
     1XPOS(13,17),YPOS(13,17),ZPOS(13,17),TOT(13,17),TMC(13,17,3),      
     2STAV(13,17,8),PSTAR(13,17)                                        
C     ***************************************************************** 
C        READ SEA-SURFACE TEMPERATURES FROM DISK                        
      NRCD=76                                                           
      READ(80,REC=NRCD,ERR=10) (DTM(K),K=1,858)                         
      GO TO 130                                                         
  10  PRINT 11                                                          
  11  FORMAT(10X,'SEAMOD DISK READ PROBLEM -- SEA SFC TEMPS',//)        
 130  CALL DELTA(XPOS,YPOS,ZPOS,CRT,VDT,STM,TOT,TMC,STAV,PSTAR,MXNR)    
C        READ PARCEL PRESSURE AT TERMINAL POINT FROM DISK               
      NRCD=3                                                            
      READ(82,REC=NRCD,ERR=200) (((PLP(I,J,N),N=1,4),J=1,17),I=1,13)    
      GO TO 220                                                         
 200  PRINT 210                                                         
 210  FORMAT(10X,'SEAMOD DISK READ PROBLEM--TERMINAL POINT PRESSURE',//)
 220  DO 100 I=1,13                                                     
      DO 100 J=1,17                                                     
      ITAB=0                                                            
      KTAB=0                                                            
      TMSF=TMC(I,J,1)                                                   
      IF(TMSF-9999.)22,100,100                                          
  22  PRSF=ZPOS(I,J)                                                    
      TM1L=TMC(I,J,2)                                                   
      IF(TM1L-9999.)30,100,100                                          
  30  PR1L=850.                                                         
      TM2L=TMC(I,J,3)                                                   
      PR2L=700.                                                         
C        EXTRAPOLATE SFC TEMPERATURE AND PRESSURE                       
      DP=1013.-PRSF                                                     
      DELT=TMSF-TM1L                                                    
      DELP=PRSF-PR1L                                                    
      IF(DELP-25.0)100,100,332                                          
 332  DT=(DELT/DELP)*DP                                                 
      TMSF=TMSF+DT                                                      
      PRSF=1013.                                                        
C        COMPUTE ADIABATIC WARMING (DEG C) ALONG OVERWATER PATH         
      TEMP1=TMC(I,J,1)+273.16                                           
      PRES1=ZPOS(I,J)*0.1                                               
      THETA=TEMP1*((100./PRES1)**0.286)                                 
      PRESF=PLP(I,J,1)*0.1                                              
      SPT=THETA*((PRESF/100.)**0.286)                                   
      SPT=SPT-273.16                                                    
      TCOR=SPT-TMSF                                                     
      IF(TCOR)334,336,336                                               
 334  TCOR=0.0                                                          
C        COMPUTE BLPS                                                   
 336  PRWT=PR1L                                                         
      TMWT=TM1L                                                         
      TDF=TMWT-TMSF                                                     
  34  PLN=ALOG(PRWT/PRSF)                                               
      BLPS=TDF/PLN                                                      
C        COMPUTE BETA                                                   
      TMPSF=TMSF+273.16                                                 
      PRESF=PRSF*0.1                                                    
      PRES1=PRWT*0.1                                                    
      THETA=TMPSF*((100./PRESF)**0.286)                                 
      TEM1L=THETA*((PRES1/100.)**0.286)                                 
      TMP1=TEM1L-273.16                                                 
      BETA=(TMP1-TMSF)/PLN                                              
      DIFF=BLPS-BETA                                                    
      IF(DIFF)35,36,36                                                  
  35  IF(ITAB)378,378,48                                                
  36  BLPS=BETA                                                         
      DIFF=0.0                                                          
  37  IF(ITAB)370,370,100                                               
C        ADJUST INITIAL ADIABATIC LAPSE RATE BY INCORPORATING THIRD     
C        LEVEL TEMPERATURES                                             
 370  IF(KTAB)372,372,100                                               
 372  IF(TM2L-9999.)373,100,100                                         
 373  IF(PR1L-PR2L)100,100,374                                          
 374  PRWT=PR2L                                                         
      DPP=PRSF-PR2L                                                     
      TM1=(TM2L-TM1L)*(PR1L-PR2L)                                       
      TM2=(TM1L-TMSF)*(PRSF-PR1L)                                       
      TDD=TM1+TM2                                                       
      PM1=(PR1L-PR2L)*(PR1L-PR2L)                                       
      PM2=(PRSF-PR1L)*(PRSF-PR1L)                                       
      PDD=PM1+PM2                                                       
      TDF=DPP*(TDD/PDD)                                                 
      KTAB=1                                                            
      GO TO 34                                                          
C        COMPUTE MEAN SEA-SURFACE TEMPERATURE                           
 378  KTM=0                                                             
      MTAB=INCR/6                                                       
      DO 386 K=1,MTAB                                                   
      ST=STAV(I,J,K)                                                    
      IF(ST-9999.)380,386,386                                           
 380  SST=ST                                                            
      IF(KTM)386,384,386                                                
 384  TIN=ST+273.16                                                     
      KTM=1                                                             
 386  CONTINUE                                                          
      SST=SST+273.16                                                    
      DIS=VDT(I,J)                                                      
      IF(DIS-650.)388,47,47                                             
 388  SST=(TIN+(3.*SST))/4.                                             
C        COMPUTE MEAN OVER-WATER VELOCITY (M/SEC) ALONG SFC TRAJECTORY  
  47  TIME=TOT(I,J)                                                     
      VBAR=DIS/TIME                                                     
      VMS=VBAR*0.2777                                                   
C        COMPUTE DRAG COEFFICIENT (AFTER DEACON AND WEBB (1962))        
      DRAG=(1.00+(0.07*VMS))*.001                                       
      IF(DRAG-0.0026)472,472,470                                        
 470  DRAG=0.0026                                                       
 472  GDR=0.03414                                                       
      GDDR=GDR*DRAG                                                     
      GDDR=GDDR*1000.                                                   
C        COMPUTE RHS OF DUQUETS EQUATION, ITERATE FOR FINAL SFC AIR TEMP
  48  RHS=GDDR*(DIS/SST)*DIFF                                           
      CNCR=0.1                                                          
      IF(SST-(TMPSF+1.0))100,100,490                                    
 490  YDIF=SST-TMPSF                                                    
      DT=YDIF/5.0                                                       
      FSAT=SST+DT                                                       
      DO 64 M=1,10                                                      
      N=M-1                                                             
      FSAT=FSAT-DT                                                      
      XDIF=FSAT-TMPSF                                                   
      IF(YDIF-(XDIF+0.01))496,500,500                                   
 496  XDIF=YDIF-0.01                                                    
      FSAT=XDIF+TMPSF                                                   
 500  ADIF=YDIF/(YDIF-XDIF)                                             
      TRM=ALOG(ADIF)                                                    
      SLH=XDIF-(YDIF*TRM)                                               
      REM(M)=RHS-SLH                                                    
      ABRM=ABS(REM(M))                                                  
      IF(ABRM-CNCR)66,66,50                                             
  50  IF(M-1)64,64,52                                                   
  52  IF(REM(M))54,56,53                                                
  53  IF(REM(N))56,56,60                                                
  54  IF(REM(N))60,56,56                                                
  56  IF(ABRM-2.0)57,57,58                                              
  57  DT=-DT*0.25                                                       
      GO TO 64                                                          
  58  DT=-DT*0.5                                                        
      GO TO 64                                                          
  60  ABRN= ABS(REM(N))                                                 
      IF(ABRM-ABRN)64,66,62                                             
  62  DT=-2.0*DT                                                        
  64  CONTINUE                                                          
  66  IF(ITAB)70,70,90                                                  
C        DETERMINE TEMPERATURE AND PRESSURE AT INTERSECTION OF INITIAL  
C        SOUNDING WITH DRY ADIABAT THROUGH FINAL SFC AIR TEMPERATURE.   
C        IF INTERSECTION IS ABOVE UPPERMOST (SECOND) LEVEL USED IN      
C        DETERMINING THE INITIAL LAPSE RATE, REPEAT FORECAST PROCEDURE  
C        WITH NEW INITIAL LAPSE RATE OBTAINED BY USING THIRD LEVEL      
C        INITIAL TEMPERATURE.                                           
C                                                                       
C                                                                       
C        DETERMINE IF INTERSECTION OCCURS ABOVE SECOND INITIAL LEVEL    
  70  PRF=PRSF*0.1                                                      
      TMF=FSAT                                                          
      PT1=TMF*((100./PRF)**0.286)                                       
      PRI=PR1L*0.1                                                      
      TMI=TM1L+273.16                                                   
      PT2=TMI*((100./PRI)**0.286)                                       
      IF(PT1-PT2)90,90,72                                               
  72  IF(TM2L-9999.)74,90,90                                            
C        COMPUTE PRESSURE AND TEMPERATURE AT INTERSECTION               
  74  DELP=PR1L-PR2L                                                    
      IF(DELP)90,90,75                                                  
  75  DELT=TM1L-TM2L                                                    
      DP=10.0                                                           
      DT=(DELT/DELP)*DP                                                 
      CDT=0.0                                                           
      CDP=0.0                                                           
      PM2=PR2L*0.1                                                      
      DO 84 K=1,50                                                      
      KP=K-1                                                            
      PM=(PR1L-CDP)*0.1                                                 
      IF(PM-PM2)86,76,76                                                
  76  TM=(TM1L-CDT)+273.16                                              
      PTX=TM*((100./PM)**0.286)                                         
      REM(K)=PT1-PTX                                                    
      IF(K-1)82,82,78                                                   
  78  REM(K)= ABS(REM(K))                                               
      REM(KP)= ABS(REM(KP))                                             
      IF(REM(KP)-REM(K))80,80,82                                        
  80  XPR=(PM*10.0)+DP                                                  
      XTM=(TM-273.16)+DT                                                
C        COMPUTE MEAN LAPSE RATE FROM SURFACE TO LEVEL OF INTERSECTION  
      PRWT=XPR                                                          
      DPP=PRSF-XPR                                                      
      TM1=(XTM-TM1L)*(PR1L-XPR)                                         
      TM2=(TM1L-TMSF)*(PRSF-PR1L)                                       
      TDD=TM1+TM2                                                       
      PM1=(PR1L-XPR)*(PR1L-XPR)                                         
      PM2=(PRSF-PR1L)*(PRSF-PR1L)                                       
      PDD=PM1+PM2                                                       
      TDF=DPP*(TDD/PDD)                                                 
      ITAB=1                                                            
      GO TO 34                                                          
  82  CDT=CDT+DT                                                        
      CDP=CDP+DP                                                        
  84  CONTINUE                                                          
  86  PRWT=PR2L                                                         
      DPP=PRSF-PR2L                                                     
      TM1=(TM2L-TM1L)*(PR1L-PR2L)                                       
      TM2=(TM1L-TMSF)*(PRSF-PR1L)                                       
      TDD=TM1+TM2                                                       
      PM1=(PR1L-PR2L)*(PR1L-PR2L)                                       
      PM2=(PRSF-PR1L)*(PRSF-PR1L)                                       
      PDD=PM1+PM2                                                       
      TDF=DPP*(TDD/PDD)                                                 
      ITAB=1                                                            
      GO TO 34                                                          
C        COMPUTE FINAL SFC AIR TEMP (AND DERIVE DEW POINT USING         
C        STATISTICAL ESTIMATE FOR RELATIVE HUMIDITY).   850 MB TEMPS    
C        AND DEW POINTS ARE ALSO MODIFIED IF BELOW INTERSECTION LEVEL.  
  90  FAC=XDIF-TCOR                                                     
      IF(FAC)100,902,902                                                
 902  TEMP(I,J,1)=TEMP(I,J,1)+FAC                                       
      TEMP1=FSAT                                                        
      PRES1=PRSF*0.1                                                    
      SVAP1=0.61078*2.71828**((17.269*(TEMP1-273.16))/(TEMP1-35.86))    
      SMXR1=0.622*(SVAP1/(PRES1-SVAP1))                                 
      XXR1=SMXR1*0.65                                                   
      PRES1=PLP(I,J,1)*0.1                                              
      AVAP=(XXR1*PRES1)/(XXR1+0.622)                                    
      DDD=ALOG(AVAP)+0.49299                                            
      DEWPT1=(4717.20-(35.86*DDD))/(17.269-DDD)                         
      DPT=DEWPT1-273.16                                                 
      IF(DEWPT(I,J,1)-DPT)904,906,906                                   
 904  DEWPT(I,J,1)=DPT                                                  
      GO TO 908                                                         
 906  DEWP=DEWPT(I,J,1)+273.16                                          
      VAP1=0.61078*2.71828**((17.269*(DEWP-273.16))/(DEWP-35.86))       
      XXR1=0.622*(VAP1/(PRES1-VAP1))                                    
C        COMPUTE FINAL SFC RELATIVE HUMIDITY                            
 908  TEMP1=TEMP(I,J,1)+273.16                                          
      SVAP1=0.61078*2.71828**((17.269*(TEMP1-273.16))/(TEMP1-35.86))    
      SMXR1=0.622*(SVAP1/(PRES1-SVAP1))                                 
      RHUM(I,J,1)=(XXR1/SMXR1)*100.                                     
      IF(DEWPT(I,J,1)-TEMP(I,J,1))92,92,91                              
  91  DEWPT(I,J,1)=TEMP(I,J,1)                                          
      RHUM(I,J,1)=100.                                                  
C        COMPUTE LCL TEMPERATURE AND PRESSURE (BARNES METHOD)           
  92  THETA=TEMP1*((100./PRES1)**0.286)                                 
      DEW=DEWPT(I,J,1)                                                  
      TEM=TEMP(I,J,1)                                                   
      CN=(0.001296*DEW)+0.1963                                          
      TLCL=(DEW-CN*(TEM-DEW))+273.16                                    
      PLCL=100.*((TLCL/THETA)**3.497)                                   
C        IF 850MB AND/OR 700MB LEVEL BELOW TERRAIN, SET TEMP, DEW POINT,
C        AND REL HUMDITY EQUAL TO SFC VALUE                             
      IF(PRES1-70.)94,94,960                                            
  94  TEMP(I,J,3)=TEMP(I,J,1)                                           
      TEMP(I,J,2)=TEMP(I,J,1)                                           
      DEWPT(I,J,3)=DEWPT(I,J,1)                                         
      DEWPT(I,J,2)=DEWPT(I,J,1)                                         
      RHUM(I,J,3)=RHUM(I,J,1)                                           
      RHUM(I,J,2)=RHUM(I,J,1)                                           
      GO TO 100                                                         
 960  IF(PRES1-85.)962,962,964                                          
 962  TEMP(I,J,2)=TEMP(I,J,1)                                           
      DEWPT(I,J,2)=DEWPT(I,J,1)                                         
      RHUM(I,J,2)=RHUM(I,J,1)                                           
      GO TO 100                                                         
C        IF LCL ABOVE 850MB, SUBSTITUTE DRY-ADIABATIC TEMP FOR INITIAL  
C        850MB TEMP AND ADJUST DEW POINT AND REL HUMIDITY               
 964  IF(PLCL-85.)965,965,976                                           
 965  PRES=85.                                                          
      TM85=THETA*((PRES/100.)**0.286)                                   
      TMP=TM85-273.16                                                   
      IF(TMP-TM1L)966,966,967                                           
 966  IF(TMP-TEMP(I,J,2))100,100,967                                    
 967  TEMP(I,J,2)=TMP                                                   
      AVAP=(XXR1*PRES)/(XXR1+0.622)                                     
      DDD=ALOG(AVAP)+0.49299                                            
      DEWPT1=(4717.20-(35.86*DDD))/(17.269-DDD)                         
      DPT=DEWPT1-273.16                                                 
      IF(DEWPT(I,J,2)-DPT)968,968,970                                   
 968  DEWPT(I,J,2)=DPT                                                  
 970  DEWP=DEWPT(I,J,2)+273.16                                          
      VAP2=0.61078*2.71828**((17.269*(DEWP-273.16))/(DEWP-35.86))       
      XXR2=0.622*(VAP2/(PRES-VAP2))                                     
      TEMP2=TEMP(I,J,2)+273.16                                          
      SVAP2=0.61078*2.71828**((17.269*(TEMP2-273.16))/(TEMP2-35.86))    
      SMXR2=0.622*(SVAP2/(PRES-SVAP2))                                  
      RHUM(I,J,2)=(XXR2/SMXR2)*100.                                     
      IF(RHUM(I,J,2)-90.)100,100,972                                    
 972  XXR2=(0.9*SMXR2)                                                  
      AVAP=(XXR2*PRES)/(XXR2+0.622)                                     
      DDD=ALOG(AVAP)+0.49299                                            
      DEWPT2=(4717.20-(35.86*DDD))/(17.269-DDD)                         
      DEWPT(I,J,2)=DEWPT2-273.16                                        
      RHUM(I,J,2)=90.                                                   
      GO TO 100                                                         
C        COMPUTE PSEUDO-EQUIVALENT POTENTIAL TEMPERATURE                
 976  VAPL=0.61078*2.71828**((17.269*(TLCL-273.16))/(TLCL-35.86))       
      THETAP=TLCL*(100./(PLCL-VAPL))**0.286                             
      DEWPT1=DEWPT(I,J,1)+273.16                                        
      VAP1=0.61078*2.71828**((17.269*(DEWPT1-273.16))/(DEWPT1-35.86))   
      XXLCL=0.622*(VAP1/(PRES1-VAP1))                                   
      CP=0.238                                                          
      HL=596.73-0.601*(TLCL-273.16)                                     
      EQPTEM=THETAP*2.71828**((HL*XXLCL)/(CP*TLCL))                     
C        ITERATE FOR 850 MB PSEUDO-ADIABATIC TEMPERATURE AND,           
C        IF GREATER, SUBSTITUTE FOR FORECAST TEMPERATURE. ADJUST        
C        DEW POINT AND RELATIVE HUMIDITY.                               
      DT=2.0                                                            
      PST=TEMP(I,J,2)+273.16                                            
      PRES2=PLP(I,J,2)*0.1                                              
      DO 995 K=1,20                                                     
      KM=K-1                                                            
      PST=PST+DT                                                        
      VAPL=0.61078*2.71828**((17.269*(PST-273.16))/(PST-35.86))         
      THETAP=PST*(100./(PRES2-VAPL))**0.286                             
      XXR2=0.622*(VAPL/(PRES2-VAPL))                                    
      HL=596.73-0.601*(PST-273.16)                                      
      REM(K)=(THETAP*2.71828**((HL*XXR2)/(CP*PST)))-EQPTEM              
      IF(K-1)995,995,980                                                
 980  IF(ABS(REM(K))-0.10)997,997,982                                   
 982  IF(REM(K))984,986,983                                             
 983  IF(REM(KM))986,986,992                                            
 984  IF(REM(KM))992,986,986                                            
 986  IF(ABS(REM(K))-1.0)988,990,990                                    
 988  DT=-DT*0.25                                                       
      GO TO 995                                                         
 990  DT=-DT*0.5                                                        
      GO TO 995                                                         
 992  IF(ABS(REM(KM))-ABS(REM(K)))994,995,995                           
 994  DT=-2.*DT                                                         
 995  CONTINUE                                                          
      PRINT 996,REM(KM)                                                 
 996  FORMAT(10X,'SEAMOD--TEMP ITERATION DOES NOT CONVERGE',5X,F10.2,//)
 997  PST=PST-273.16                                                    
      IF(TEMP(I,J,2)-PST)998,999,999                                    
 998  TEMP(I,J,2)=PST                                                   
 999  RHUM(I,J,2)=90.                                                   
      XXR1=(0.9*XXR2)                                                   
      AVAP=(XXR1*PRES2)/(XXR1+0.622)                                    
      DDD=ALOG(AVAP)+0.49299                                            
      DEWPT1=(4717.20-(35.86*DDD))/(17.269-DDD)                         
      DEWPT(I,J,2)=DEWPT1-273.16                                        
 100  CONTINUE                                                          
      RETURN                                                            
      END                                                               
