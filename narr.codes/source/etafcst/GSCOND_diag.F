                             SUBROUTINE GSCOND                          
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GSCOND      GRID SCALE CONDENSATION AND EVAPORATION
C   PRGRMMR: ZHAO            ORG: W/NP22     DATE: ??-??-??
C     
C ABSTRACT:
C     GSCOND COMPUTES THE GRID SCALE EVAPORATION AND CONDENSATION
C     
C PROGRAM HISTORY LOG:
C   94-??-??  ZHAO       - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-03-28  BLACK      - ADDED EXTERNAL EDGE
C   98-11-02  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL GSCOND FROM MAIN PROGRAM EBU
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
C                  TEMPV
C                  PVRBLS
C                  ACMCLH
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
                             P A R A M E T E R                          
     & (A1=610.78,A2=17.2693882,A3=273.16,A4=35.86              
     &, PQ0=379.90516,TRESH=.95
     &, CP=1004.6,ELWV=2.50E6,ELIV=2.834E6,ROW=1.E3,G=9.8
     &, EPSQ=2.E-12,DLDT=2274.0,TM10=263.16,R=287.04
     &, CPR=CP*R,RCPR=1./(CPR))   
                             P A R A M E T E R                          
     & (ARCP=A2*(A3-A4)/CP,RCP=1./CP,PQ0C=PQ0*TRESH,RROG=1./(ROW*G))    
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
     & (LDA=(IDIM2-IDIM1+1)*(JDIM2-JDIM1+1))
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
      INCLUDE "TEMPV.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMCLH.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N                          
     & IW(LM)
     &,PDSL(IDIM1:IDIM2,JDIM1:JDIM2)
     &,T_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,T0_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q0_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,TRAIN_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,CWM_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,HTM_T(LM,IDIM1:IDIM2,JDIM1:JDIM2)
C---------------------------------------------------------------------- 
                             R E A L                                    
     & MR,KE,INIT                                                       
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C--------------PREPARATORY CALCULATIONS---------------------------------
      DTPH =NPHS*DT
      RDTPH=1./DTPH
      TWODT=DTPH
      RTWODT=1./TWODT                                                   
      C0=1.5E-4                                                         
      C1=300.                                                          
      C2=0.5                                                            
      MR=3.0E-4                                                         
      KE=2.0E-5                                                         
      US=1.
      EPS=0.622
      CCLIMIT=1.0E-3                                                    
      CLIMIT =1.0E-20                                                   
C-----------------------------------------------------------------------
C------------------PADDING SPECIFIC HUMIDITY & CWM IF TOO SMALL---------
C-----------------------------------------------------------------------
!$omp parallel do
      DO 30 L=1,LM                              
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        IF(Q(I,J,L).LT.EPSQ)Q(I,J,L)=EPSQ*HTM(I,J,L)                       
        IF(CWM(I,J,L).LT.CLIMIT)CWM(I,J,L)=CLIMIT*HTM(I,J,L)                   
      ENDDO
      ENDDO
   30 CONTINUE
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDSL(I,J)=RES(I,J)*PD(I,J)                                              
      ENDDO
      ENDDO
C
      IW(1)=0
      UTIM=1.
C
C-----------------------------------------------------------------------
C*************BEGINNING OF GRID-SCALE CONDENSATION/EVAP. LOOP***********
C-----------------------------------------------------------------------
C***
C***  TRANSPOSE ARRAYS
C***
!$omp parallel sections
!$omp section
      CALL SGETMO(T,LDA,LDA,LM,T_T,LM)
      CALL SGETMO(Q,LDA,LDA,LM,Q_T,LM)
      CALL SGETMO(HTM,LDA,LDA,LM,HTM_T,LM)
      CALL SGETMO(CWM,LDA,LDA,LM,CWM_T,LM)
!$omp section
      CALL SGETMO(T0,LDA,LDA,LM,T0_T,LM)
      CALL SGETMO(Q0,LDA,LDA,LM,Q0_T,LM)
      CALL SGETMO(TRAIN,LDA,LDA,LM,TRAIN_T,LM)
!$omp end parallel sections
C
C-----------------------------------------------------------------------
C------------------QW, QI AND QINT--------------------------------------
C-----------------------------------------------------------------------
!$omp parallel do
!$omp& private(aa,ab,ac,ad,ae,af,ag,ai,ap,aq,at,bi,ccr,ccrkl,
!$omp&         ccrkl1,cond,condk,cone0,cwmkl,e0,ec,elv,fi,fiw,
!$omp&         hbm2ij,hh,iwkl,lmhij,lml,p0ij,pdslij,pp,pp0,
!$omp&         qc,qi,qint,qkl,qtemp,qw,resij,rqkl,rqkll,rqtmp,
!$omp&         thh,tkl,tmt0,tmt15,u00ij,u00kl,us00)
!$omp& firstprivate(iw)
C
      DO 100 J=MYJS2,MYJE2
      DO 100 I=MYIS,MYIE
C-----------------------------------------------------------------------
C
      LMHIJ=LMH(I,J)
      HBM2IJ=HBM2(I,J)
      U00IJ=U00(I,J)
      P0IJ=P0(I,J)
      RESIJ=RES(I,J)
      PDSLIJ=PDSL(I,J)
C
      DO 90 L=2,LM
C
      TKL=T_T(L,I,J)
      QKL=Q_T(L,I,J)
      CWMKL=CWM_T(L,I,J)
C
      COND=0.
      E0=0.
      LML=LM-LMHIJ
      HH=HTM_T(L,I,J)*HBM2IJ
      TMT0=(TKL-273.16)*HH                                                
      TMT15=AMIN1(TMT0,-15.)*HH                                            
      AI=0.008855
      BI=1.
C
      IF(TMT0.LT.-20.)THEN
        AI=0.007225
        BI=0.9674
      ENDIF
C
      QW=HH*PQ0/(PDSLIJ*AETA(L)+PT)                             
     1          *EXP(HH*A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))                  
      QI=QW*(BI+AI*AMIN1(TMT0,0.))                               
      QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))                        
      IF(TMT0.LE.-40.)QINT=QI                                    
C-----------------------------------------------------------------------
C-------------------ICE-WATER ID NUMBER IW------------------------------
C-----------------------------------------------------------------------
      IF(TMT0.LT.-15.)THEN
        U00KL=U00IJ+UL(L+LML)*(0.95-U00IJ)*UTIM
        FI=Q(I,J,L)-U00KL*QI
        IF(FI.GT.0..OR.CWMKL.GT.CLIMIT)THEN                    
          IW(L)=1                                                   
        ELSE                                                           
          IW(L)=0                                                   
        ENDIF                                                         
      ENDIF
C
      IF(TMT0.GE.0.)THEN
        IW(L)=0                                                      
      ENDIF
C
      IF(TMT0.LT.0.0.AND.TMT0.GE.-15.)THEN
        IW(L)=0
        IF(IW(L-1).EQ.1.AND.CWMKL.GT.CLIMIT)IW(L)=1 
      ENDIF
C-----------------------------------------------------------------------
C--------------CONDENSATION AND EVAPORATION OF CLOUD--------------------
C------------------------AT, AQ AND DP/DT-------------------------------
C-----------------------------------------------------------------------
      THH=TWODT*HH                                                      
      PP=PDSLIJ*AETA(L)+PT                                             
      PP0=P0IJ*RESIJ*AETA(L)+PT
      AT=(TKL-T0_T(L,I,J))*RTWODT                                        
      AQ=(QKL-Q0_T(L,I,J))*RTWODT                                        
      AP=(PP-PP0)*RTWODT         
      IWKL=IW(L)                                                      
      U00KL=U00IJ+UL(L+LML)*(0.95-U00IJ)*UTIM
C-----------------------------------------------------------------------
C----------------THE SATUATION SPECIFIC HUMIDITY------------------------
C-----------------------------------------------------------------------
      FIW=FLOAT(IWKL)                                                   
      ELV=(1.-FIW)*ELWV+FIW*ELIV                                     
      QC =(1.-FIW)*QINT+FIW*QI
C-----------------------------------------------------------------------
C----------------THE RELATIVE HUMIDITY----------------------------------
C-----------------------------------------------------------------------
      IF(QC.LE.0.)THEN                                                
        RQKL=0.
      ELSE                                                             
        RQKL=QKL/QC                                                 
      ENDIF                                                             
C-----------------------------------------------------------------------
C----------------CLOUD COVER RATIO CCR----------------------------------
C-----------------------------------------------------------------------
      IF(RQKL.LE.U00KL)THEN                                      
        CCR=0.
      ELSE                                                            
        RQKLL=AMIN1(US,RQKL)                                         
        CCR=1.-SQRT((US-RQKLL)/(US-U00KL))                     
      ENDIF                                                             
C-----------------------------------------------------------------------
C-----------CORRECT CCR IF IT IS TOO SMALL IN LARGE CWM REGIONS--------
C-----------------------------------------------------------------------
      IF(CCR.GE.0.01.AND.CCR.LE.0.2.AND.CWMKL.GE.0.2E-3)THEN
        CCR=AMIN1(1.,CWMKL*1.E3)
      ENDIF
C
      CCRKL=CCR
C-----------------------------------------------------------------------
C-------GIVE UP THIS POINT  IF NO CLOUD NOR CONDENSATION EXIST--------- 
C-----------------------------------------------------------------------
      IF(CCRKL.LE.CCLIMIT.AND.CWMKL.LE.CLIMIT)GO TO 90
C-----------------------------------------------------------------------
C----------------EVAPORATION OF CLOUD WATER-----------------------------
C-----------------------------------------------------------------------
        EC=0.
        IF(CCRKL.LE.CCLIMIT.AND.CWMKL.GT.CLIMIT)THEN 
          EC=QC*(U00KL-RQKL)*RTWODT                           
          E0=AMAX1(EC,0.0)                                             
          E0=AMIN1(CWMKL*RTWODT,E0)*HH                           
          E0=AMAX1(0.,E0)                                          
        ENDIF
C-----------------------------------------------------------------------
C----------------CONDENSATION OF CLOUD----------------------------------
C-----------------------------------------------------------------------
      IF(CCRKL.LE.0.20.OR.QC.LE.EPSQ)THEN                        
C     IF(CCRKL.LE.CCLIMIT.OR.QC.LE.EPSQ)THEN                        
        COND=0.                                                    
        GO TO 80                                                      
      ENDIF                                                            
C-----------------------------------------------------------------------
C-----------THE EQS. FOR COND. HAS BEEN REORGANIZED TO REDUCE CPU------
C-----------------------------------------------------------------------
      US00=US-U00KL 
      CCRKL1=1.-CCRKL
      AA=EPS*ELV*PP*QKL
      AB=CCRKL*CCRKL1*QC*US00
      AC=AB+0.5*CWMKL
      AD=AB*CCRKL1
      AE=CPR*TKL*TKL
      AF=AE*PP
      AG=AA*ELV
      AI=CP*AA
      COND=(AC-AD)*(AF*AQ-AI*AT+AE*QKL*AP)/(AC*(AF+AG))
      cond1=cond        !--- 6/26/01 change
C-----------------------------------------------------------------------
C-----------CHECK & CORRECT IF OVER CONDENSATION OCCURS-----------------
C-----------------------------------------------------------------------
      CONDK=(QKL-U00KL*QC*0.1)*RTWODT                             
C     CONDK=(QKL-U00KL*QC*0.6)*RTWODT                             
      IF(COND.GT.CONDK)THEN
        COND=CONDK                           
      ENDIF
C-----------------------------------------------------------------------
C----------CHECK & CORRECT IF SUPERSATUATION IS TOO HIGH----------------
C-----------------------------------------------------------------------
      QTEMP=QKL-AMAX1(0.,(COND-E0))*THH
      RQTMP=QTEMP/QC
      IF(RQTMP.GE.1.10)THEN
        COND=(QKL-1.10*QC)*RTWODT
        cond2=cond        !--- 6/26/01 change
      ENDIF
C-----------------------------------------------------------------------
      IF(COND.LT.0.)THEN                                           
        COND=0.
      ENDIF                                                             
C-----------------------------------------------------------------------
C-------------------UPDATE OF T, Q AND CWM------------------------------
C-----------------------------------------------------------------------
   !
   !--- Begin: 6/26/01 diagnostics
   !
      if (MYPE.EQ.1 .AND. I.EQ.25 .AND. J.EQ.31 .AND. K.GE.30 .AND. 
     &    K.LE.32) then
        write(6,"(3(a2,i2))") 'i=',i,'j=',j,'k=',k
        write(6,"(4(a10,g11.4))") 
     &  'tkl=',tkl,'qkl=',qkl,'cwmkl=',cwmkl,'p=',pp,
     &  'pp0=',pp0,'u00kl=',u00kl,'at=',at,'aq=',aq,
     &  'ap=',ap,'elv=',elv,'qc=',qc,'rqkl=',rqkl,
     &  'ccr=',ccr,'fiw=',fiw,'ec=',ec,'rtwodt=',RTWODT,
     &  'us00=',US00,'aa=',aa,'ab=',ab,'ac=',ac,
     &  'ad=',ad,'ae=',ae,'af=',af,'ag=',ag,
     &  'ai=',ai,'cond1=',cond1,'condk=',condk,'qtemp=',qtemp,
     &  'rqtmp=',rqtmp,'cond2=',cond2,'thh=',thh,'cond=',cond,
     &  'eo=',e0,'tnum1=',ac-ad,'tnum2=',AF*AQ-AI*AT+AE*QKL*AP,
     &    'denom=',AC*(AF+AG)
      endif
   !
   !--- End:   6/26/01 diagnostics
   !
   80 CONTINUE                                                          
      CONE0=COND-E0
      CWM_T(L,I,J)=CONE0*THH+CWMKL                                          
C     
C-----------------------------------------------------------------------
C     ACCUMULATE LATENT HEATING DUE TO GRID-SCALE PRECIP/EVAP.
C     SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
C     IS CALLED.  THIS PERIOD IS THE PHYSICS TIMESTEP.
C-----------------------------------------------------------------------
C
      T_T(L,I,J)=ELV*RCP*CONE0*THH+TKL                                      
      TRAIN_T(L,I,J)=ELV*RCP*CONE0*THH*RDTPH+TRAIN_T(L,I,J)
      Q_T(L,I,J)=-CONE0*THH+QKL                                             
      IF(CWM_T(L,I,J).LE.0.)CWM_T(L,I,J)=0.                            
C
   90 CONTINUE
C
  100 CONTINUE                                     
C-----------------------------------------------------------------------
C-------------------SAVE T, Q AND P FOR THIS STEP-----------------------
C-----------------------------------------------------------------------
C***
C***  TRANSPOSE BACK THE NEEDED ARRAYS
C***
!$omp parallel sections
!$omp section
      CALL SGETMO(T_T,LM,LM,LDA,T,LDA)
      CALL SGETMO(Q_T,LM,LM,LDA,Q,LDA)
!$omp section
      CALL SGETMO(TRAIN_T,LM,LM,LDA,TRAIN,LDA)
      CALL SGETMO(CWM_T,LM,LM,LDA,CWM,LDA)
!$omp end parallel sections
C
!$omp parallel do
      DO 125 L=1,LM
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        Q0(I,J,L)=Q(I,J,L)                                                   
        T0(I,J,L)=T(I,J,L)                                                   
      ENDDO
      ENDDO
  125 CONTINUE                                                         
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        P0(I,J)=PD(I,J)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
                             RETURN                                     
                             END                                        
