                             SUBROUTINE PRECPD                          
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    PRECPD      LARGE SCALE PRECIPITATION
C   PRGRMMR: ZHAO            ORG: W/NP22     DATE: ??-??-??
C     
C ABSTRACT:
C     PRECPD COMPUTES THE GRID SCALE PRECIPITATION.
C     
C PROGRAM HISTORY LOG:
C   94-??-??  ZHAO       - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-11-20  ABELES     - PARALLEL OPTIMIZATION
C   96-03-29  BLACK      - REMOVED SCRCH COMMON
C   96-07-18  ZHAO       - NEW WMIN CALCULATION
C   96-09-25  BALDWIN    - NEW SR CALCULATION
C   98-11-02  BLACK      - MODIFICATION FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL PRECPD FROM MAIN PROGRAM EBU
C
C   INPUT ARGUMENT LIST:
C       NONE     
C  
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE: NONE
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  PHYS
C                  VRBLS
C                  CLDWTR
C                  PVRBLS
C                  ACMCLH
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
                             P A R A M E T E R                          
     & (A1=610.78,A2=17.2693882,A3=273.16,A4=35.86              
     &, PQ0=379.90516,TRESH=.95,R=287.04,C0=0.15
     &, CP=1004.6,ELWV=2.50E6,ELIV=2.834E6,ROW=1.E3,G=9.8           
     &, EPSQ=2.E-12,DLDT=2274.0,ELIW=ELIV-ELWV)          
C
                             P A R A M E T E R                          
     & (ARCP=A2*(A3-A4)/CP,RCP=1./CP,PQ0C=PQ0*TRESH,RROG=1./(ROW*G)     
     &, RROW=1./ROW)                                                    
C---------------------------------------------------------------------- 
      INCLUDE "parmeta"                                                 
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
#include "sp.h"
C---------------------------------------------------------------------- 
                             P A R A M E T E R                          
     & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)                                 
     &, LP1=LM+1,LTOP=1,LBOT=LM)                                         
C
                             P A R A M E T E R                          
     & (IMJM_LOC=IDIM2*JDIM2,LDA=(IDIM2-IDIM1+1)*(JDIM2-JDIM1+1))
C-----------------------------------------------------------------------
                             L O G I C A L                              
     & RUN,FIRST,RESTRT,SIGMA,NOZ                                       
C---------------------------------------------------------------------- 
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMCLH.comm"
C-----------------------------------------------------------------------
      INCLUDE "PPTASM.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N                          
     & IPREC(IMJM_LOC),JPREC(IMJM_LOC) 
     &,T_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,TRAIN_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,HTM_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,CWM_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,TLAT_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
                             D I M E N S I O N                          
     & PRECRL1(IMJM_LOC),PRECSL1(IMJM_LOC),IWL1(IMJM_LOC)                     
C-----------------------------------------------------------------------
                             R E A L                                    
     & KE,INIT,MI0                                                   
C-----------------------------------------------------------------------
C***********************************************************************
C--------------PREPARATORY CALCULATIONS---------------------------------
      DTPH  = NPHS * DT
      RDTPH = 1. / DTPH
      TWODT= DTPH
      RTWODT=1./TWODT                                                   
      KE=2.0E-5                                                         
      US=1.
      EPS=0.622E0                                                       
      CCLIMIT=1.0E-3                                                    
      CLIMIT=1.0E-20                                                    
      CWS=0.025                                                         
      CSM1=5.0000E-8                                                    
      CRS1=5.00000E-6                                                   
      CRS2=6.66600E-10                                                  
      CR=5.0E-4                                                         
      MI0=5.0E-4                                                        
      AA2=1.25E-3                                                       
      AVRAIN=AVRAIN+1.
      ARATIM=ARATIM+1
C-------------------PADDING CLOUD MIXING RATIO IF TOO SMALL-------------
!$omp parallel do
      DO 20 L=1,LM                                                      
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        CWM(I,J,L)=CWM(I,J,L)*HTM(I,J,L)*HBM2(I,J)                              
        IF(CWM(I,J,L).LT.0.)CWM(I,J,L)=0.
C------------------PADDING SPECIFIC HUMIDITY IF TOO SMALL---------------
        IF(Q(I,J,L).LT.EPSQ)Q(I,J,L)=EPSQ*HTM(I,J,L)                       
      ENDDO
      ENDDO
   20 CONTINUE                                                          
C
      UTIM=1.
C-----------------------------------------------------------------------
!$omp parallel do
      DO N=1,IMJM_LOC
        IWL1(N)=0
        PRECRL1(N)=0.
        PRECSL1(N)=0.
      ENDDO
C------------CHOOSE THE COLUMNS WHERE PREC CAN BE PRODUCED--------------
      NPRE=0
      DO 35 J=MYJS2,MYJE2
      DO 35 I=MYIS,MYIE
C
      DO L=2,LM
        TTEMP=0.025*(T(I,J,L)-273.16)
        WFIX=0.9814*EXP(0.01873*L)
        WMIN=0.1E-3*EXP(TTEMP)*WFIX
        IF(CWM(I,J,L).GT.WMIN)GO TO 33
      ENDDO
C
      GO TO 35
   33 NPRE=NPRE+1
      IPREC(NPRE)=I
      JPREC(NPRE)=J
   35 CONTINUE
C------------------------------------------------------------------------
C***
C***  TRANSPOSE ARRAYS
C***
!$omp parallel sections
!$omp section
      CALL SGETMO(T,LDA,LDA,LM,T_T,LM)
!$omp section
      CALL SGETMO(Q,LDA,LDA,LM,Q_T,LM)
!$omp section
      CALL SGETMO(CWM,LDA,LDA,LM,CWM_T,LM)
!$omp section
      CALL SGETMO(HTM,LDA,LDA,LM,HTM_T,LM)
!$omp section
      CALL SGETMO(TRAIN,LDA,LDA,LM,TRAIN_T,LM)
!$omp section
      CALL SGETMO(TLAT,LDA,LDA,LM,TLAT_T,LM)
!$omp end parallel sections
C-----------------------------------------------------------------------
C-----------------BEGINING OF PRECIPITATION CALCULATION-----------------
C-----------------------------------------------------------------------
C***
C***  LOOP OVER ALL POSSIBLE PRECIPITATION POINTS
C***
!$omp parallel do 
!$omp& private(aai,aetal,ai,amaxcm,amaxps,amaxrq,bi,ccr,conde,
!$omp&         const,cpdr,cs,cwmk,detal,erk,err,errt,ers,erst,expf,fi,
!$omp&         fiw,hbm2k,hh,htmk,i,iwl,j,lml,mi0,pdsl,pid,ppr,pps,
!$omp&         pracw,praut,precrk,precrl,precsk,precsl,precss,psaci,
!$omp&         psaut,psm,psm1,psm2,qc,qct,qi,qint,qintt,qit,qk,qq,
!$omp&         qtemp,qw,qwt,rconde,rprs,rq,rqkll,rqt,rqtt,tk,tmt0,
!$omp&         tmt0k,tmt0t,tmt15,tmt15t,totppt,tt,ttemp,ttlat,u00ij,
!$omp&         u00kl,u00klt,ull,wfix,wmink,ww)
C
      DO 300 N=1,NPRE
C
      I=IPREC(N)
      J=JPREC(N)
      HBM2K=HBM2(I,J)
      PDSL=RES(I,J)*PD(I,J)                                              
      CONST=PDSL/G*TWODT                                        
      LML=LM-LMH(I,J)
      U00IJ=U00(I,J)
C
      DO 180 L=2,LM  
C
      DETAL=DETA(L)
      ULL=UL(L)
      AETAL=AETA(L)
      WFIX=0.9814*EXP(0.01873*L)
C
      PRECRL=0.                                                      
      PRECSL=0.                                                      
      PRAUT=0.                                                      
      PSAUT=0.                                                      
      PRACW=0.                                                      
      PSACI=0.                                                      
      ERR  =0.                                                      
      ERS  =0.                                                      
      PSM  =0.                                                      
      PSM1 =0.                                                      
      PSM2 =0.                                                      
      PPR  =0.
      PPS  =0.
      CPDR =0.
      HH   =0.
      PID  =0.
      IWL  =0.
      CONDE=0.
      RCONDE=0.
C-----------------------------------------------------------------------
      TT=T_T(L,I,J)
      QQ=Q_T(L,I,J)
      WW=CWM_T(L,I,J)
      HTMK=HTM_T(L,I,J)
      TTLAT=TLAT_T(L,I,J)
C-----------------------------------------------------------------------
      U00KL=U00IJ+UL(L+LML)*(0.95-U00IJ)*UTIM
      TTEMP=0.025*(TT-273.16)
      WMINK=0.1E-3*EXP(TTEMP)*WFIX
C-----------------------------------------------------------------------
C----------CHOOSE THE POINTS WHERE PRECIPITATION CAN BE PRODUCED--------
C-----------------------------------------------------------------------
      PRECRK=AMAX1(0.,PRECRL1(N))                                         
      PRECSK=AMAX1(0.,PRECSL1(N))                                         
      HH=HTMK*HBM2K
      IF(WW.LT.WMINK.AND.(PRECRK+PRECSK).EQ.0.)THEN
        PID=0.
      ELSE
        PID=HH
      ENDIF
C-----------------------------------------------------------------------
C-------------------QW, QI AND QINT-------------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CONDE=CONST*DETAL                                               
        RCONDE=1./CONDE                                                   
        TK=TT
        QK=QQ
        TMT0=(TK-273.16)*HH
        TMT15=AMIN1(TMT0,-15.)*HH
        AI=0.008855
        BI=1.
C
        IF(TMT0.LT.-20.)THEN
          AI=0.007225
          BI=0.9674
        ENDIF
C
        QW=HH*PQ0/(PDSL*AETAL+PT)                              
     1               *EXP(HH*A2*(TK-A3)/(TK-A4))                  
        QI=QW*(BI+AI*AMIN1(TMT0,0.))                                 
        QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))                          
        IF(TMT0.LE.-40.)QINT=QI
C-------------------ICE-WATER ID NUMBER IW------------------------------
        IF(TMT0.LT.-15.)THEN                                             
          FI=QK-U00KL*QI
          IF(FI.GT.0..OR.WW.GT.CLIMIT) THEN                    
            IWL=1                                                   
          ELSE                                                           
            IWL=0                                                   
          ENDIF                                                         
        ENDIF                                                            
C
        IF(TMT0.LT.0.0.AND.TMT0.GE.-15.0)THEN                            
          IWL=0                                                   
          IF(IWL1(N).EQ.1.AND.WW.GT.CLIMIT)IWL=1 
        ENDIF                                                            
C
        IF(TMT0.GE.0.)THEN                                               
          IWL=0                                                      
        ENDIF                                                            
C----------------THE SATUATION SPECIFIC HUMIDITY------------------------
        FIW=FLOAT(IWL)                                                
        QC=(1.-FIW)*QINT+FIW*QI
C----------------THE RELATIVE HUMIDITY----------------------------------
        IF(QC.LE.0.)THEN                                                
          RQ=1.E-10
        ELSE                                                             
          RQ=QK/QC
        ENDIF                                                             
C----------------CLOUD COVER RATIO CCR----------------------------------
        IF(RQ.LE.U00KL)THEN                                      
          CCR=0.
        ELSE                                                            
          RQKLL=AMIN1(US,RQ)                                         
          CCR=1.-SQRT((US-RQKLL)/(US-U00KL))                     
        ENDIF                                                             
C-----------CORRECT CCR IF IT IS TOO SMALL IN LARGE CWM REGIONS--------
        IF(CCR.GE.0.01.AND.CCR.LE.0.2.AND
     1                       .WW.GE.0.2E-3)THEN
          CCR=AMIN1(1.,WW*1.0E3)
        ENDIF
      ENDIF
   60              CONTINUE
C-----------------------------------------------------------------------
C------------------PRECIPITATION PRODUCTION RATES-----------------------
C------------------AUTO-CONVERT RATES-----------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        IWK=IWL
        CWMK=AMAX1(0.,WW-CLIMIT)                                 
        MI0=WMINK
C
        IF(IWK.EQ.1)THEN                                                 
          EXPF=EXP(0.025*TMT0)                                           
          AA1=1.E-3*EXPF                                                 
          PSAUT=AA1*AMAX1(0.,CWMK-MI0)                                
          CPDR=-PSAUT*TWODT                                           
          IF(-CPDR.GE.CWMK)THEN                                         
            CPDR=-CWMK                                                 
            PSAUT=-CPDR*RTWODT                                      
          ENDIF                                                         
        ELSE                                                              
          AMAXCM=AMAX1(0.,CWMK-MI0)                                      
          PRAUT=C0*AMAXCM*AMAXCM                                   
          CPDR=-PRAUT*TWODT                                           
          IF(-CPDR.GE.CWMK)THEN                                         
            CPDR=-CWMK                                                  
            PRAUT=-CPDR*RTWODT                                       
          ENDIF                                                         
        ENDIF                                                            
        PPR=PRAUT*CONDE
        PPS=PSAUT*CONDE
      ENDIF
C
      IF(PID.EQ.1.)THEN
        WW=CPDR*HH+WW                                   
        PRECRL=PRECRL1(N)+PPR*HH
        PRECSL=PRECSL1(N)+PPS*HH
      ENDIF
C-----------------------------------------------------------------------
C-----------------------ACCRETIONS--------------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        IWK=IWL
        CWMK=WW
        PRECRK=AMAX1(0.,PRECRL1(N))                                         
        PRECSK=AMAX1(0.,PRECSL1(N))                                         
        IF(IWK.EQ.1)THEN                                                 
          EXPF=EXP(0.025*TMT0)                                           
          CS=AA2*EXPF                                                    
          PSACI=CS*AMAX1(0.,CWMK)*PRECSK                          
          CPDR=-PSACI*TWODT                                           
          IF(-CPDR.GE.CWMK)THEN                                         
            CPDR=-CWMK                                                 
            PSACI=-CPDR*RTWODT                                      
          ENDIF                                                         
        ELSE                                                              
          PRACW=CR*AMAX1(0.,CWMK)*(PRECRK+PRECSK)             
          CPDR=-PRACW*TWODT                                           
          IF(-CPDR.GE.CWMK)THEN                                         
            CPDR=-CWMK                                                  
            PRACW=-CPDR*RTWODT                                       
          ENDIF                                                         
        ENDIF                                                            
        PPR=PRACW*CONDE
        PPS=PSACI*CONDE
      ENDIF
C
      IF(PID.EQ.1.)THEN
        WW=CPDR*HH+WW                                   
        PRECRL=PRECRL+PPR*HH                                       
        PRECSL=PRECSL+PPS*HH                                       
      ENDIF
C-----------------------------------------------------------------------
C-----EVAPORATION/CONDENSATION OF PRECIPITATION-------------------------
C***** ERR & ERS POSITIVE--EVAPORATION                                  
C***** ERR & ERS NEGTIVE---CONDENSATION                                 
C-----------------------------------------------------------------------
      IF(PID.EQ.1.0)THEN
        QK=QQ
        TMT0K=TMT0
        IF(TMT0K.LT.-30.)TMT0K=-30.                                        
        PRECRK=AMAX1(0.,PRECRL)                                         
        PRECSK=AMAX1(0.,PRECSL)                                         
C---------------------------------------------------------------------- 
C INCREASE THE EVAPORATION/CONDENSATION FOR STRONG/LIGHT PREC           
C---------------------------------------------------------------------- 
        U00KLT=U00KL
        AMAXRQ=AMAX1(0.,U00KL-RQ)   
        ERR=KE*AMAXRQ*PRECRK**0.5                                      
C
        IF(TMT0.GE.0.)THEN                                               
          ERS=0.                                                         
        ELSE                                                              
          ERS=(CRS1+CRS2*TMT0K)*AMAXRQ*PRECSK/U00KLT                      
        ENDIF                                                            
C
        IF(ERR+ERS.LE.1.E-20) GO TO 125
C---------------CORRECT IF OVER-EVAPO./COND. OCCURS-------------------- 
        HHT=HH*TWODT
        TTEMP=TT-RCP*(ELWV*ERR+ELIV*ERS)*HHT
        QTEMP=QQ+HHT*(ERR+ERS)
        TMT0T=(TTEMP-273.16)*HH
        IF(TMT0T.LT.-30.)TMT0T=-30.                                        
        TMT15T=AMIN1(TMT0T,-15.)*HH
        AI=0.008855
        BI=1.
C
        IF(TMT0T.LT.-20.)THEN
          AI=0.007225
          BI=0.9674
        ENDIF
C
        QWT=HH*PQ0/(PDSL*AETAL+PT)                              
     1               *EXP(HH*A2*(TTEMP-A3)/(TTEMP-A4))                  
        QIT=QWT*(BI+AI*AMIN1(TMT0T,0.))                                 
        QINTT=QWT*(1.-0.00032*TMT15T*(TMT15T+15.))                          
        IF(TMT0T.LE.-40.)QINTT=QIT                                    
        FIW=FLOAT(IWL)
        QCT=(1.-FIW)*QINTT+FIW*QIT
C
        IF(QCT.LE.1.E-10) THEN
          RQT=1.E-10
          RQTT=1.E-10
        ELSE
          RQT=QTEMP/QCT
          RQTT=QQ/QCT
        ENDIF
C
        IF(RQT.LE.U00KL) GO TO 125
C
        ERK=(U00KL-RQTT)*QCT*RTWODT
        RPRS=ERK/(PRECRK+PRECSK)                                          
        ERRT=PRECRK*RPRS                                                
        ERST=PRECSK*RPRS                                                
        ERR=AMAX1(0.,0.5*(ERR+ERRT))
        ERS=AMAX1(0.,0.5*(ERS+ERST))
C         
 125    CONTINUE
C
        PPR=-ERR*CONDE                                                 
        PPS=-ERS*CONDE                                                 
C
        IF(-PPR.GE.PRECRK)THEN                                         
          PPR=-PRECRK                                                  
          ERR=-PPR*RCONDE
        ENDIF                                                            
C
        IF(-PPS.GE.PRECSK)THEN                                         
          PPS=-PRECSK                                                  
          ERS=-PPS*RCONDE
        ENDIF                                                            
C
      ENDIF                                                            
C
      IF(PID.EQ.1.)THEN
        PRECRL=PRECRL+PPR*HH
        PRECSL=PRECSL+PPS*HH
      ENDIF                                                            
C-----------------------------------------------------------------------
C--------------------MELTING OF THE SNOW--------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CWMK=WW                                                     
        AMAXPS=AMAX1(0.,PRECSL)                                      
C
        IF(TMT0.GT.0.)THEN                                               
          PSM1=CSM1*TMT0*TMT0*AMAXPS                                  
          PSM2=CWS*CR*CWMK*AMAXPS                                     
          PSM=PSM1+PSM2                                         
        ELSE                                                              
          PSM1=0.
          PSM2=0.
          PSM=0.
        ENDIF                                                            
C
        PPR=PSM*CONDE
        PPS=-PSM*CONDE
C
        IF(-PPS.GE.AMAXPS)THEN                                         
          PPS=-AMAXPS                                                  
          PPR=AMAXPS                                                   
          PSM1=-PPS*RCONDE                                            
          PSM2=0.
          PSM=PSM1
        ENDIF                                                            
C
      ENDIF                                                            
C
      IF(PID.EQ.1.)THEN
        PRECRL=PRECRL+PPR*HH                                       
        PRECSL=PRECSL+PPS*HH                                       
      ENDIF                                                            
C-----------------------------------------------------------------------
C---------------UPDATE T AND Q------------------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        HHT=HH*TWODT                                        
        TT=-RCP*(ELWV*ERR+ELIV*ERS+ELIW*PSM1)                
     1             *HHT+TT
        QQ=(ERR+ERS)*HHT+QQ
        TTLAT=-RCP*(ELWV*ERR+ELIV*ERS+ELIW*PSM1
     &                -ELWV*(PRAUT+PRACW)
     &                -ELIV*(PSAUT+PSACI))*HHT+TTLAT
      ENDIF                                                            
C
      IF(HH.EQ.1.)THEN
        IWL1(N)=IWL
        PRECRL1(N)=PRECRL
        PRECSL1(N)=PRECSL
      ENDIF
C     
C     ACCUMULATE LATENT HEATING DUE TO GRID-SCALE PRECIP/EVAP.
C     SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
C     IS CALLED.  THIS PERIOD IS THE PHYSICS TIMESTEP.
C
      TRAIN_T(L,I,J)=TRAIN_T(L,I,J)+(TT-T_T(L,I,J))*RDTPH
      T_T(L,I,J)=TT
      Q_T(L,I,J)=QQ
      CWM_T(L,I,J)=WW
      TLAT_T(L,I,J)=TTLAT
  180                      CONTINUE
C-----------------------------------------------------------------------
C-------------------THE PRECIPITATION ON SFC----------------------------
C-----------------------------------------------------------------------
      PRECRS=PRECRL1(N)*RROW                                         
      PRECSS=PRECSL1(N)*RROW                                         
C
      APREC(I,J)=PRECRS+PRECSS
      PREC(I,J)=PREC(I,J)+PRECRS+PRECSS                              
      ACPREC(I,J)=ACPREC(I,J)+APREC(I,J)
C-----------------------------------------------------------------------
C---------------THE SNOW AND RAIN RATIO OF SFC PREC---------------------
C----SR IS THE RATIO OF SNOW TO THE TOTAL PRECIP------------------------
C----IF TOTAL PRECIP IS ZERO, SR IS ZERO--------------------------------
C-----------------------------------------------------------------------
      TOTPPT=PRECRS+PRECSS
      IF (TOTPPT.GT.1.E-8) THEN
       SR(I,J)=PRECSS/TOTPPT
      ELSE
       SR(I,J)=0.
      ENDIF
C-----------------------------------------------------------------------
  300 CONTINUE
C-----------------------------------------------------------------------
C***
C***  TRANSPOSE BACK
C***
!$omp parallel sections
!$omp section
      CALL SGETMO(T_T,LM,LM,LDA,T,LDA)
!$omp section
      CALL SGETMO(Q_T,LM,LM,LDA,Q,LDA)
!$omp section
      CALL SGETMO(CWM_T,LM,LM,LDA,CWM,LDA)
!$omp section
      CALL SGETMO(TRAIN_T,LM,LM,LDA,TRAIN,LDA)
!$omp section
      CALL SGETMO(TLAT_T,LM,LM,LDA,TLAT,LDA)
!$omp end parallel sections
C-----------------------------------------------------------------------
                             RETURN                                     
                             END                                        
