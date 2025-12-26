      SUBROUTINE TURBL
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    TURBL       VERTICAL TURBULENT EXCHANGE
C   PRGRMMR: JANJIC          ORG: W/NP2      DATE: 95-03-20       
C     
C ABSTRACT:
C     TURBL UPDATES THE TURBULENT KINETIC ENERGY WITH THE PROD-
C     UCTION/DISSIPATION TERM AND THE VERTICAL DIFFUSION TERM
C     DIFFUSION TERM (USING AN IMPLICIT FORMULATION).  EXCHANGE
C     COEFFICIENTS FOR THE SURFACE AND FOR ALL LAYER INTERFACES
C     ARE THEN COMPUTED AND THE EXCHANGE IS EXECUTED.
C     
C PROGRAM HISTORY LOG:
C   95-03-15  JANJIC     - ORIGINATOR
C   95-03-28  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-29  BLACK      - ADDED EXTERNAL EDGE; REMOVED SCRCH COMMON
C   96-07-19  MESINGER   - ADDED Z0 EFFECTIVE
C   98-??-??  TUCCILLO   - MODIFIED FOR CLASS VIII PARALLELISM
C   98-10-27  BLACK      - PARALLEL CHANGES INTO MOST RECENT CODE
C     
C USAGE: CALL TURBL FROM MAIN PROGRAM EBU
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
C     UNIQUE: MIXLEN
C             PRODQ2
C             DIFCOF
C             SFCDIF
C             VDIFH
C             VDIFQ
C             VDIFV
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  PHYS2
C                  VRBLS
C                  PVRBLS
C                  INDX
C                  Z0EFFT
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
C-----------------------------------------------------------------------
C
      INCLUDE "EXCHM.h"
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (KTMQ2=1,CAPA=0.28589641,G=9.8,RG=1./G,ROG=287.04/G
     &, EPSZ=1.E-4,EPSQ2=0.2
     &, IMJM=IM*JM-JM/2,LM1=LM-1,LP1=LM+1,JAM=6+2*(JM-10)
     &, ITB=76,JTB=134,ITBQ=152,JTBQ=440
     &, NHRZ=(IDIM2-IDIM1+1)*(JDIM2-JDIM1+1))
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS2.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
      INCLUDE "Z0EFFT.comm"
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
                             R E A L
     & CKLQ(IDIM1:IDIM2,JDIM1:JDIM2)
     &,CT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,APE (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,AKH (IDIM1:IDIM2,JDIM1:JDIM2,LM1)
     &,AKM (IDIM1:IDIM2,JDIM1:JDIM2,LM1)
     &,ZINT(IDIM1:IDIM2,JDIM1:JDIM2,LP1)
     &,UZ0H(IDIM1:IDIM2,JDIM1:JDIM2)
     &,VZ0H(IDIM1:IDIM2,JDIM1:JDIM2)
C
                             R E A L
     & AKMCOL(IDIM1:IDIM2,JDIM1:JDIM2,LM1) 
     &,AKHCOL(IDIM1:IDIM2,JDIM1:JDIM2,LM1) 
     &,AKMSV (IDIM1:IDIM2,JDIM1:JDIM2) 
     &,ZCOL  (IDIM1:IDIM2,JDIM1:JDIM2,LP1) 
     &,UCOL  (IDIM1:IDIM2,JDIM1:JDIM2,LM) 
     &,VCOL  (IDIM1:IDIM2,JDIM1:JDIM2,LM) 
C
                             R E A L
     & AKH_T   (LM1,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,AKM_T   (LM1,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,APECOL_T(LM,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,ZCOL_T  (LP1,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,ZCOL_T2 (LP1,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,UCOL_T  (LM,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,VCOL_T  (LM,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,TCOL_T  (LM,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,QCOL_T  (LM,IDIM1:IDIM2,JDIM1:JDIM2) 
     &,Q2COL_T (LM,IDIM1:IDIM2,JDIM1:JDIM2)
C
                             R E A L 
     & GM(LM1),GH(LM1),EL(LM1),ZEFF(4)
C-----------------------------------------------------------------------
C***
C***  THE FOLLOWING ARE USED FOR TIMIMG PURPOSES ONLY
C***
      real*8 timef
      real nhb_tim,mpp_tim,init_tim
      common/timing/surfce_tim,nhb_tim,res_tim,exch_tim
C***********************************************************************
C-----------------------------------------------------------------------
      CALL ZERO3(AKM,LM1)
      CALL ZERO3(ZINT,LP1)
      CALL ZERO3_T(AKH_T,LM1)
      CALL ZERO3_T(AKM_T,LM1)
      CALL ZERO2(UZ0H)
      CALL ZERO2(VZ0H)
C-----------------------------------------------------------------------
C***
C***  COMPUTE THE HEIGHTS OF THE LAYER INTERFACES AND THE EXNER FUNCTION
C***
!$omp parallel do 
      DO J=MYJS_P1,MYJE_P1          ! This line is correct
c     DO J=MYJS2_P1,MYJE2_P1        ! This line matches operations
      DO I=MYIS_P1,MYIE_P1
        ZINT(I,J,LP1)=EPSZ 
        IF(SIGMA)ZINT(I,J,LP1)=RG*FIS(I,J)
      ENDDO
      ENDDO
C
      DO 10 L=LM,1,-1
!$omp parallel do private (apests,pdsl)
      DO J=MYJS1_P1,MYJE1_P1          ! This line is correct
c     DO J=MYJS2_P1,MYJE2_P1        ! This line matches operations
      DO I=MYIS_P1,MYIE_P1
        PDSL=PD(I,J)*RES(I,J)
        APESTS=PDSL*AETA(L)+PT
C
        ZINT(I,J,L)=ZINT(I,J,L+1)+T(I,J,L)/APESTS
     1             *PDSL*DETA(L)*ROG*(Q(I,J,L)*0.608+1.)
        ZINT(I,J,L)=(ZINT(I,J,L)-DFRLG(L))*HTM(I,J,L)+DFRLG(L)
C
        APE(I,J,L)=(1.E5/APESTS)**CAPA
      ENDDO
      ENDDO
   10 CONTINUE
C-----------------------------------------------------------------------
C***
C***  REMOVE NEGATIVE Q2
C***
!$omp parallel do 
      DO 40 L=1,LM
      DO J=MYJS_P1,MYJE_P1
      DO I=MYIS_P1,MYIE_P1
        Q2(I,J,L)=AMAX1(Q2(I,J,L)*HBM2(I,J),EPSQ2)
      ENDDO
      ENDDO
   40 CONTINUE
C-----------------------------------------------------------------------
!$omp parallel do 
      DO J=MYJS2_P1,MYJE2_P1
      DO I=MYIS_P1,MYIE_P1
        UZ0H(I,J)=(UZ0(I+IHE(J),J)+UZ0(I+IHW(J),J)
     1            +UZ0(I,J+1)+UZ0(I,J-1))*HBM2(I,J)*0.25
        VZ0H(I,J)=(VZ0(I+IHE(J),J)+VZ0(I+IHW(J),J)
     1            +VZ0(I,J+1)+VZ0(I,J-1))*HBM2(I,J)*0.25
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***               PREPARE THE EXCHANGE COEFFICIENTS
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***
C***  COMPUTE VELOCITY COMPONENTS AT H POINTS
C***
!$omp parallel do private(rwmsk,wmsk)
      DO 60 L=1,LM
C
      DO J=MYJS2_P1,MYJE2_P1
      DO I=MYIS_P1,MYIE_P1
        WMSK=VTM(I+IHE(J),J,L)+VTM(I+IHW(J),J,L)
     1                        +VTM(I,J+1,L)+VTM(I,J-1,L)
        IF(WMSK.GT.0.)THEN
          RWMSK=1./WMSK
          UCOL(I,J,L)=(U(I+IHE(J),J,L)*VTM(I+IHE(J),J,L)
     1                +U(I+IHW(J),J,L)*VTM(I+IHW(J),J,L)
     2                +U(I,J+1,L)*VTM(I,J+1,L)+U(I,J-1,L)*VTM(I,J-1,L))
     3                *RWMSK
          VCOL(I,J,L)=(V(I+IHE(J),J,L)*VTM(I+IHE(J),J,L)
     1                +V(I+IHW(J),J,L)*VTM(I+IHW(J),J,L)
     2                +V(I,J+1,L)*VTM(I,J+1,L)+V(I,J-1,L)*VTM(I,J-1,L))
     3                *RWMSK
        ELSE
          UCOL(I,J,L)=0.
          VCOL(I,J,L)=0.
        ENDIF
      ENDDO
      ENDDO
   60 CONTINUE
C***
C***  FILL TRANSPOSED ARRAYS
C***
!$omp parallel sections
!$omp section
      CALL SGETMO(T,NHRZ,NHRZ,LM,TCOL_T,LM)
!$omp section
      CALL SGETMO(Q,NHRZ,NHRZ,LM,QCOL_T,LM)
!$omp section
      CALL SGETMO(APE,NHRZ,NHRZ,LM,APECOL_T,LM)
!$omp section
      CALL SGETMO(Q2,NHRZ,NHRZ,LM,Q2COL_T,LM)
!$omp section
      CALL SGETMO(ZINT,NHRZ,NHRZ,LP1,ZCOL_T,LP1)
!$omp section
      CALL SGETMO(UCOL,NHRZ,NHRZ,LM,UCOL_T,LM)
!$omp section
      CALL SGETMO(VCOL,NHRZ,NHRZ,LM,VCOL_T,LM)
!$omp end parallel sections
C----------------------------------------------------------------------
C***
C***  FIND THE MIXING LENGTH
C***
!$omp parallel do private(el,gh,gm,hpbl,lmhk,lmhm,lmhp,lpbl)
!$omp& private(ulm,vlm,wstar,zeff)
      DO 100 J=MYJS2_P1,MYJE2_P1
      DO 100 I=MYIS_P1,MYIE1_P1

      LMHK=LMH(I,J)
      LMHP=LMHK+1
      LMHM=LMHK-1
C
      CALL MIXLEN(LMHK,LPBL,HPBL,UCOL_T(1,I,J),VCOL_T(1,I,J)
     1,           TCOL_T(1,I,J),QCOL_T(1,I,J),Q2COL_T(1,I,J)
     2,           APECOL_T(1,I,J),ZCOL_T(1,I,J),GM,GH,EL)
C
C-----------------------------------------------------------------------
C***
C***  SOLVE FOR THE PRODUCTION/DISSIPATION OF 
C***  THE TURBULENT KINETIC ENERGY
C***
C
      CALL PRODQ2(LMHK,DTQ2,USTAR(I,J),GM,GH,EL,Q2COL_T(1,I,J))
C
C-----------------------------------------------------------------------
C***
C***  FIND THE EXCHANGE COEFFICIENTS IN THE FREE ATMOSPHERE
C***
      CALL DIFCOF(LMHK,GM,GH,EL,Q2COL_T(1,I,J)
     1,           ZCOL_T(1,I,J),AKM_T(1,I,J),AKH_T(1,I,J))
C-----------------------------------------------------------------------
C***
C***  CARRY OUT THE VERTICAL DIFFUSION OF
C***  TURBULENT KINETIC ENERGY
C***
C
      CALL VDIFQ(LMHK,KTMQ2,DTQ2,Q2COL_T(1,I,J),EL,ZCOL_T(1,I,J))
C-----------------------------------------------------------------------
C***
C***  FIND THE Z0 EFFECTIVE
C***
      ZEFF(1)=ZEFFIJ(I,J,1)
      ZEFF(2)=ZEFFIJ(I,J,2)
      ZEFF(3)=ZEFFIJ(I,J,3)
      ZEFF(4)=ZEFFIJ(I,J,4)
C-----------------------------------------------------------------------
C***
C***  FIND THE SURFACE EXCHANGE COEFFICIENTS
C***
      ULM=UCOL(I,J,LMHK)
      VLM=VCOL(I,J,LMHK)
C
      CALL SFCDIF(LMHK,SM(I,J),THS(I,J),QS(I,J)
     1,           UZ0H(I,J),VZ0H(I,J),THZ0(I,J),QZ0(I,J)
     2,           USTAR(I,J),WSTAR
     3,           Z0(I,J),ZEFF,AKMS(I,J),AKHS(I,J),HPBL,CT(I,J)
     4,           U10(I,J),V10(I,J),TSHLTR(I,J),TH10(I,J)
     5,           QSHLTR(I,J),Q10(I,J)
     5,           TH30(I,J),Q30(I,J),U30(I,J),V30(I,J)
     6,           ULM,VLM,TCOL_T(1,I,J),QCOL_T(1,I,J)
     7,           APECOL_T(1,I,J),ZCOL_T(1,I,J),PD(I,J),PT
     8,           T(I,J,LMHK))
C

      PDSL=PD(I,J)*RES(I,J)
      DPLM=PDSL*DETA(LMHK)*0.5
      DZLM=ROG*DPLM*T(I,J,LMHK)*(1.+0.608*Q(I,J,LMHK))
     &      /(PDSL*AETA(LMHK)+PT)

      IF ((2.0*DZLM).LT.30.0) THEN

      DPLM1=PDSL*DETA(LMHK-1)*0.5
      DZLM1=ROG*DPLM1*T(I,J,LMHK-1)*(1.+0.608*Q(I,J,LMHK-1))
     &      /(PDSL*AETA(LMHK-1)+PT)

      FAC1=(30.-DZLM)/(DZLM+DZLM1)
      FAC2=((2.*DZLM+DZLM1)-30.)/(DZLM+DZLM1)

      if (FAC1.GT.1.0 .or. FAC1.LT.0.0) then
         print *, " *****************************", i,j,FAC1
      end if
      if (FAC2.GT.1.0 .or. FAC2.LT.0.0) then
         print *, " *****************************", i,j,FAC2
      end if

      T30=FAC2*T(I,J,LMHK)+FAC1*T(I,J,LMHK-1)
      P30=(PD(I,J)+PT)*EXP(-30.0*G/(287.04*T(I,J,LMHK)))
      TH30(I,J)=T30*(1.E5/P30)**0.28589641
      Q30(I,J)=FAC2*Q(I,J,LMHK)+FAC1*Q(I,J,LMHK-1)
      U30(I,J)=FAC2*U(I,J,LMHK)+FAC1*U(I,J,LMHK-1)
      V30(I,J)=FAC2*V(I,J,LMHK)+FAC1*V(I,J,LMHK-1)

      END IF

  100 CONTINUE
C------------------------------------------------------------------------
C***
C***  FILL STANDARD ARRAYS FROM TRANSPOSED ARRAYS
C***
!$omp parallel sections
!$omp section
       CALL SGETMO(Q2COL_T,LM,LM,NHRZ,Q2,NHRZ)
!$omp section
       CALL SGETMO(AKH_T,LM1,LM1,NHRZ,AKH,NHRZ)
!$omp section
       CALL SGETMO(AKM_T,LM1,LM1,NHRZ,AKM,NHRZ)
!$omp end parallel sections
C-----------------------------------------------------------------------
C***
C***  UNCOMPUTED LOCATIONS MUST BE FILLED IN FOR THE POST-PROCESSOR
C***
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1
C
C***  EASTERN GLOBAL BOUNDARY
C
      IF(MY_IE_GLB.EQ.IM)THEN
        DO J=1,JM
          IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
            JJ=J-MY_JS_GLB+1
            TH10(IIM,JJ)=TH10(IIM-1,JJ)
            Q10(IIM,JJ)=Q10(IIM-1,JJ)
            U10(IIM,JJ)=U10(IIM-1,JJ)
            V10(IIM,JJ)=V10(IIM-1,JJ)
            TSHLTR(IIM,JJ)=TSHLTR(IIM-1,JJ)
            QSHLTR(IIM,JJ)=QSHLTR(IIM-1,JJ)
            TH30(IIM,JJ)=TH30(IIM-1,JJ)
            Q30(IIM,JJ)=Q30(IIM-1,JJ)
            U30(IIM,JJ)=U30(IIM-1,JJ)
            V30(IIM,JJ)=V30(IIM-1,JJ)
          ENDIF
        ENDDO
      ENDIF
C
C***  SOUTHERN GLOBAL BOUNDARY
C
      IF(MY_JS_GLB.EQ.1)THEN
        DO J=1,2
        DO I=1,IM
          IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
            II=I-MY_IS_GLB+1
            TH10(II,J)=TH10(II,3)
            Q10(II,J)=Q10(II,3)
            U10(II,J)=U10(II,3)
            V10(II,J)=V10(II,3)
            TSHLTR(II,J)=TSHLTR(II,3)
            QSHLTR(II,J)=QSHLTR(II,3)
            TH30(II,J)=TH30(II,3)
            Q30(II,J)=Q30(II,3)
            U30(II,J)=U30(II,3)
            V30(II,J)=V30(II,3)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
C
C***  NORTHERN GLOBAL BOUNDARY
C
      IF(MY_JE_GLB.EQ.JM)THEN
        DO J=JM-1,JM
        JJ=J-MY_JS_GLB+1
        DO I=1,IM
          IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
            II=I-MY_IS_GLB+1
            TH10(II,JJ)=TH10(II,JJM-2)
            Q10(II,JJ)=Q10(II,JJM-2)
            U10(II,JJ)=U10(II,JJM-2)
            V10(II,JJ)=V10(II,JJM-2)
            TSHLTR(II,JJ)=TSHLTR(II,JJM-2)
            QSHLTR(II,JJ)=QSHLTR(II,JJM-2)
            TH30(II,JJ)=TH30(II,JJM-2)
            Q30(II,JJ)=Q30(II,JJM-2)
            U30(II,JJ)=U30(II,JJM-2)
            V30(II,JJ)=V30(II,JJM-2)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      btim=timef()
      CALL EXCH(UZ0H,1,VZ0H,1,1,1)
      exch_tim=exch_tim+timef()-btim
C***
C***  AVERAGE UZ0 AND VZ0 BACK TO V POINTS
C***
!$omp parallel do 
      DO 125 J=MYJS2,MYJE2
      DO 125 I=MYIS,MYIE
      UZ0(I,J)=(UZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)
     1         +UZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)
     2         +UZ0H(I,J+1)*HBM2(I,J+1)+UZ0H(I,J-1)*HBM2(I,J-1))*0.25
      VZ0(I,J)=(VZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)
     1         +VZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)
     2         +VZ0H(I,J+1)*HBM2(I,J+1)+VZ0H(I,J-1)*HBM2(I,J-1))*0.25
  125 CONTINUE
C-----------------------------------------------------------------------
C***
C***  EXECUTE THE GROUND PROCESSES
C***
      CALL SURFCE(APE(IDIM1,JDIM1,1),ZINT(IDIM1,JDIM1,1)
     1,           CKLQ(IDIM1,JDIM1))
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***                 EXECUTE THE VERTICAL EXCHANGE
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      btim=timef()
      CALL EXCH(AKMS,1,AKM,LM1,ZINT,LP1,1,1)
      exch_tim=exch_tim+timef()-btim
C
!$omp parallel do
      DO L=1,LM1
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
          AKMCOL(I,J,L)=(AKM(I+IVE(J),J,L)*HBM2(I+IVE(J),J)
     1        +AKM(I+IVW(J),J,L)*HBM2(I+IVW(J),J)
     2        +AKM(I,J+1,L)*HBM2(I,J+1)+AKM(I,J-1,L)*HBM2(I,J-1))
     3        *VTM(I,J,L)*VBM2(I,J)*0.25
          AKHCOL(I,J,L)=AKH(I,J,L)*HTM(I,J,L)*HBM2(I,J)
        ENDDO
        ENDDO
      ENDDO
C
!$omp parallel do
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        THZ0(I,J)=(1.-SM(I,J))*THS(I,J)+SM(I,J)*THZ0(I,J)
        QZ0 (I,J)=(1.-SM(I,J))*QS (I,J)+SM(I,J)*QZ0 (I,J)
        AKMSV(I,J)=(AKMS(I+IVE(J),J)*HBM2(I+IVE(J),J)
     1             +AKMS(I+IVW(J),J)*HBM2(I+IVW(J),J)
     2             +AKMS(I,J+1)*HBM2(I,J+1)+AKMS(I,J-1)*HBM2(I,J-1))
     3             *VBM2(I,J)*0.25
      ENDDO
      ENDDO
C
!$omp parallel do
      DO L=1,LP1
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
          ZCOL(I,J,L)=0.25*(ZINT(I+IVE(J),J,L)+ZINT(I+IVW(J),J,L)
     1                     +ZINT(I,J+1,L)+ZINT(I,J-1,L))
        ENDDO
        ENDDO
      ENDDO
C***
C***  TRANSPOSE ARRAYS
C***
!$omp parallel sections
!$omp section
      CALL SGETMO(ZCOL,NHRZ,NHRZ,LP1,ZCOL_T2,LP1)
!$omp section
      CALL SGETMO(U,NHRZ,NHRZ,LM,UCOL_T,LM)
!$omp section
      CALL SGETMO(V,NHRZ,NHRZ,LM,VCOL_T,LM)
!$omp section
      CALL SGETMO(AKHCOL,NHRZ,NHRZ,LM1,AKH_T,LM1)
!$omp section
      CALL SGETMO(AKMCOL,NHRZ,NHRZ,LM1,AKM_T,LM1)
!$omp end parallel sections
C-----------------------------------------------------------------------
!$omp parallel do private(lmhk,lmvk)
      DO 200 J=MYJS2,MYJE2
      DO 200 I=MYIS,MYIE1
C
      LMHK=LMH(I,J)
      LMVK=LMV(I,J)
C***
C***  CARRY OUT THE VERTICAL DIFFUSION OF
C***  TEMPERATURE AND WATER VAPOR
C***
      CALL VDIFH(LMHK,KTMQ2,DTQ2,THZ0(I,J),QZ0(I,J),AKHS(I,J)
     1,          CT(I,J),CKLQ(I,J)
     2,          TCOL_T(1,I,J),QCOL_T(1,I,J),AKH_T(1,I,J)
     3,          APECOL_T(1,I,J),ZCOL_T(1,I,J))
C     
C-----------------------------------------------------------------------
C***
C***  CARRY OUT THE VERTICAL DIFFUSION OF
C***  VELOCITY COMPONENTS
C***
       CALL VDIFV(LMVK,KTMQ2,DTQ2,UZ0(I,J),VZ0(I,J)
     1,          AKMSV(I,J),UCOL_T(1,I,J),VCOL_T(1,I,J)
     2,          AKM_T(1,I,J),ZCOL_T2(1,I,J))
C
C-----------------------------------------------------------------------
  200 CONTINUE
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***
C***  FILL STANDARD ARRAYS FROM TRANSPOSED ARRAYS
C***
!$omp parallel sections
!$omp section
      CALL SGETMO(QCOL_t,LM,LM,NHRZ,Q,NHRZ)
!$omp section
      CALL SGETMO(TCOL_t,LM,LM,NHRZ,T,NHRZ)
!$omp section
      CALL SGETMO(UCOL_t,LM,LM,NHRZ,U,NHRZ)
!$omp section
      CALL SGETMO(VCOL_t,LM,LM,NHRZ,V,NHRZ)
!$omp end parallel sections
C
C***
C***  REMOVE NEGATIVE Q2
C***
!$omp parallel do 
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          Q2(I,J,L)=AMAX1(Q2(I,J,L)*HBM2(I,J),EPSQ2)
        ENDDO
        ENDDO
      ENDDO
C----------------------------------------------------------------------
                                 RETURN
			         END
