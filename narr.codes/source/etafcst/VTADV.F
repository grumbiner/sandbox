c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE VTADV
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    VTADV       VERTICAL ADVECTION
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-11-17
C     
C ABSTRACT:
C     VTADV CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION
C     TO THE TENDENCIES OF TEMPERATURE, SPECIFIC HUMIDITY, WIND
C     COMPONENTS, AND TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE
C     VARIABLES.  FOR ALL VARIABLES EXCEPT SPECIFIC HUMIDITY A
C     SIMPLE CENTERED DIFFERENCE SCHEME IN SPACE IS USED IN
C     CONJUNCTION WITH THE PURE EULER-BACKWARD TIME SCHEME.  
C     A PIECEWISE LINEAR SCHEME IS USED TO CALCULATE THE VERTICAL
C     ADVECTION OF SPECIFIC HUMIDITY SO THAT NO FALSE MAXIMA OR
C     MINIMA ARE PRODUCED.
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   90-??-??  MESINGER   - INSERTED PIECEWISE LINEAR SCHEME FOR
C                          SPECIFIC HUMIDITY
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-11-20  ABELES     - PARALLEL OPTIMIZATION
C   96-03-29  BLACK      - ADDED EXTERNAL EDGE; REMOVED SCRCH COMMON
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C   00-02-04  BLACK      - ADDED CLOUD WATER/ICE
C   01-12-11  BLACK      - SMOOTHING FOR CFL VIOLATION
C     
C USAGE: CALL VTADV FROM MAIN PROGRAM EBU
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
C                  MASKS
C                  DYNAM
C                  VRBLS
C                  CONTIN
C                  PVRBLS
C                  CLDWTR
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
                             P A R A M E T E R
     & (EDQMX=2.E-5,EDQMN=-2.E-5,EPSQ=1.E-12,EPSQ2=0.2,KSMUD=0)
C
                             P A R A M E T E R
     & (CFL_MAX=0.97)
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)
     &, LM1=LM-1,LM2=LM-2,LP1=LM+1)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,NOSLA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & WFA   ( LM1),WFB   ( LM1)
C
                             D I M E N S I O N
     & ETADTL(IDIM1:IDIM2,JDIM1:JDIM2)
     &,TTA   (IDIM1:IDIM2,JDIM1:JDIM2),TQ2A  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TUA   (IDIM1:IDIM2,JDIM1:JDIM2),TVA   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TTB   (IDIM1:IDIM2,JDIM1:JDIM2),TQ2B  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TUB   (IDIM1:IDIM2,JDIM1:JDIM2),TVB   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ASTI  (IDIM1:IDIM2,JDIM1:JDIM2),ASBI  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DQTI  (IDIM1:IDIM2,JDIM1:JDIM2),DQBI  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,VM    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,RPDX  (IDIM1:IDIM2,JDIM1:JDIM2),RPDY  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,QDEDT (IDIM1:IDIM2,JDIM1:JDIM2),QDEUT (IDIM1:IDIM2,JDIM1:JDIM2)
     &,QDEDB (IDIM1:IDIM2,JDIM1:JDIM2),QDEUB (IDIM1:IDIM2,JDIM1:JDIM2)
     &,EDTD  (IDIM1:IDIM2,JDIM1:JDIM2),EDBD  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,EDBF  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DQDE  (IDIM1:IDIM2,JDIM1:JDIM2),DQDEB (IDIM1:IDIM2,JDIM1:JDIM2)
     &,SEDB  (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & FNE (IDIM1:IDIM2,JDIM1:JDIM2),FSE (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & TSTL  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,USTL  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,VSTL  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,SAM   (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,QBI   (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,Q2ST  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,ARRAY1(IDIM1:IDIM2,JDIM1:JDIM2,LM1)
     &,ARRAY2(IDIM1:IDIM2,JDIM1:JDIM2,LM1) 
C-----------------------------------------------------------------------
                             R E A L
     &,ALLOCATABLE,DIMENSION(:,:,:) :: WATER
C-----------------------------------------------------------------------
                             R E A L
     & VAD_TEND1(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,VAD_TEND2(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,VAD_TNDX1(LM),VAD_TNDX2(LM)
!
                             I N T E G E R
     & LBOT_CFL_T(IDIM1:IDIM2,JDIM1:JDIM2)
     &,LTOP_CFL_T(IDIM1:IDIM2,JDIM1:JDIM2)
     &,LBOT_CFL_U(IDIM1:IDIM2,JDIM1:JDIM2)
     &,LTOP_CFL_U(IDIM1:IDIM2,JDIM1:JDIM2)
     &,LBOT_CFL_V(IDIM1:IDIM2,JDIM1:JDIM2)
     &,LTOP_CFL_V(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DTAD=IDTAD*DT
C-----------------------------------------------------------------------
C--------------DEFINE ADDED UPSTREAM ADVECTION CONSTANTS----------------
C-----------------------------------------------------------------------
                             DO 25 L=1,LM1
      WFA(L)=DETA(L  )/(DETA(L)+DETA(L+1))
      WFB(L)=DETA(L+1)/(DETA(L)+DETA(L+1))
   25 CONTINUE
C--------------NO MOISTURE SLOPE ADJUSTMENT IF NOT WANTED---------------
      NOSLA=.FALSE.
C       IF FALSE, NUMBER OF MOISTURE SLOPE ADJUSTMENT PASSES
      NMSAP=3
C--------------SMOOTHING VERTICAL VELOCITY AT H POINTS------------------
      IF(KSMUD.GT.0)THEN
!$omp parallel do 
                             DO 90 L=1,LM1
          DO 50 J=MYJS_P4,MYJE_P4
          DO 50 I=MYIS_P4,MYIE_P4
      ETADT(I,J,L)=ETADT(I,J,L)*HBM2(I,J)
   50 CONTINUE
C-----------------------------------------------------------------------
      NSMUD=KSMUD
C***
C***  THE FNE, FSE, ETADTL, AND ETADT ARRAYS
C***  ARE ON OR ASSOCIATED WITH H POINTS
C***
                             DO 90 KS=1,NSMUD
          DO 80 J=MYJS_P3,MYJE1_P3
          DO 80 I=MYIS_P3,MYIE_P3
      FNE(I,J)=(ETADT(I+IHE(J),J+1,L)-ETADT(I,J,L))
     1         *HTM(I,J,L+1)*HTM(I+IHE(J),J+1,L+1)
   80 CONTINUE
          DO 82 J=MYJS1_P3,MYJE_P3
          DO 82 I=MYIS_P3,MYIE_P3
      FSE(I,J)=(ETADT(I+IHE(J),J-1,L)-ETADT(I,J,L))
     1         *HTM(I+IHE(J),J-1,L+1)*HTM(I,J,L+1)
   82 CONTINUE
          DO 84 J=MYJS2_P1,MYJE2_P1
          DO 84 I=MYIS_P1,MYIE_P1
      ETADTL(I,J)=(FNE(I,J)-FNE(I+IHW(J),J-1)
     1            +FSE(I,J)-FSE(I+IHW(J),J+1))*HBM2(I,J)
   84 CONTINUE
          DO 86 J=MYJS2_P1,MYJE2_P1
          DO 86 I=MYIS_P1,MYIE_P1
      ETADT(I,J,L)=ETADTL(I,J)*0.125+ETADT(I,J,L)
   86 CONTINUE
   90 CONTINUE
C-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------
!
!***  IF THE CFL CRITERION IS VIOLATED THEN LOCATE VERTICAL LIMITS
!***  BETWEEN WHICH TO SMOOTH THE TENDENCIES
!
!-----------------------------------------------------------------------
!$omp parallel do
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        LTOP_CFL_T(I,J)=0 
        LBOT_CFL_T(I,J)=0 
        LTOP_CFL_U(I,J)=0 
        LBOT_CFL_U(I,J)=0 
        LTOP_CFL_V(I,J)=0 
        LBOT_CFL_V(I,J)=0 
      ENDDO
      ENDDO
!
      DO L=1,LM1
!
!$omp parallel do private(cfl)
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
!
!***  MASS POINTS
!
          CFL=ETADT(I,J,L)*DTAD*HBM2(I,J)/(0.5*(DETA(L)+DETA(L+1)))
          IF(ABS(CFL).GT.CFL_MAX)THEN
            IF(LTOP_CFL_T(I,J).EQ.0)LTOP_CFL_T(I,J)=MAX(L,2)
            IF(LBOT_CFL_T(I,J).LT.L)LBOT_CFL_T(I,J)=MAX(L,2)
          ENDIF
!
!***  U COMPONENT
!
          CFL=(ETADT(I+IVW(J),J,L)+ETADT(I+IVE(J),J,L))*DTAD*VBM2(I,J)
     1       /(DETA(L)+DETA(L+1))
!
          IF(ABS(CFL).GT.CFL_MAX)THEN
            IF(LTOP_CFL_U(I,J).EQ.0)LTOP_CFL_U(I,J)=MAX(L,2)
            IF(LBOT_CFL_U(I,J).LT.L)LBOT_CFL_U(I,J)=MAX(L,2)
          ENDIF
!
!***  V COMPONENT
!
          CFL=(ETADT(I,J-1,L)+ETADT(I,J+1,L))*DTAD*VBM2(I,J)
     1       /(DETA(L)+DETA(L+1))
!
          IF(ABS(CFL).GT.CFL_MAX)THEN
            IF(LTOP_CFL_V(I,J).EQ.0)LTOP_CFL_V(I,J)=MAX(L,2)
            IF(LBOT_CFL_V(I,J).LT.L)LBOT_CFL_V(I,J)=MAX(L,2)
          ENDIF
!
        ENDDO
        ENDDO
!
      ENDDO
!
C-----------------------------------------------------------------------
C--------------VERTICAL (MATSUNO) ADVECTION OF T------------------------
C-----------------------------------------------------------------------
!$omp parallel do 
          DO 100 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
      TTB(I,J)=0.
  100 CONTINUE
!
      DO L=1,LM1
!$omp parallel do private(ttak)
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
          TTAK  =(T(I,J,L+1)-T(I,J,L))*ETADT(I,J,L)*F4D
          TSTL(I,J,L)=(TTAK  +TTB(I,J))*RDETA(L)+T(I,J,L)
          VAD_TEND1(I,J,L)=(TTAK  +TTB(I,J))*RDETA(L)
          TTB(I,J)=TTAK
        ENDDO
        ENDDO
      ENDDO
!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        VAD_TEND1(I,J,LM)=TTB(I,J)*RDETA(LM)
      ENDDO
      ENDDO
!
                             DO 110 L=1,LM1
!$omp parallel do private(ttak)
          DO 110 J=MYJS2,MYJE2
          DO 110 I=MYIS,MYIE
      TSTL(I,J,L)=VAD_TEND1(I,J,L)+T(I,J,L)
  110 CONTINUE
C
!$omp parallel do 
          DO 120 J=MYJS2,MYJE2
          DO 120 I=MYIS,MYIE
      TSTL(I,J,LM)=T(I,J,LM)+VAD_TEND1(I,J,LM)
  120 CONTINUE
C-----------------------------------------------------------------------
C--------------SECOND (BACKWARD) MATSUNO STEP---------------------------
C-----------------------------------------------------------------------
!$omp parallel do
          DO 125 J=MYJS,MYJE
          DO 125 I=MYIS,MYIE
      TTB(I,J)=0.
  125 CONTINUE
C
      DO L=1,LM1
!$omp parallel do private(ttak)
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
          TTAK  =(TSTL(I,J,L+1)-TSTL(I,J,L))*ETADT(I,J,L)*F4D
          VAD_TEND1(I,J,L)=(TTAK  +TTB(I,J))*RDETA(L)
          TTB(I,J)=TTAK
        ENDDO
        ENDDO
      ENDDO
!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        VAD_TEND1(I,J,LM)=TTB(I,J)*RDETA(LM)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  IF THE CFL CRITERION IS VIOLATED THEN VERTICALLY SMOOTH
!***  THE TENDENCY
!
!-----------------------------------------------------------------------
!$omp parallel do
!$omp& private(lstart,lstop,vad_tndx1)
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
!
        IF(LTOP_CFL_T(I,J).GT.0)THEN
          LSTART=LTOP_CFL_T(I,J)
          LSTOP =MIN(LBOT_CFL_T(I,J),LM-1)
!
          DO L=LSTART,LSTOP
            VAD_TNDX1(L)=(VAD_TEND1(I,J,L-1)+VAD_TEND1(I,J,L+1)
     1                   +2.*VAD_TEND1(I,J,L))*0.25
          ENDDO
          DO L=LSTART,LSTOP
            VAD_TEND1(I,J,L)=VAD_TNDX1(L)
          ENDDO
        ENDIF
!
      ENDDO
      ENDDO
!
                             DO 140 L=1,LM1
!$omp parallel do 
          DO 140 J=MYJS2,MYJE2
          DO 140 I=MYIS,MYIE
      T(I,J,L)=VAD_TEND1(I,J,L)+T(I,J,L)
!
  140 CONTINUE
C
!$omp parallel do
          DO 150 J=MYJS2,MYJE2
          DO 150 I=MYIS,MYIE
      T(I,J,LM)=T(I,J,LM)+VAD_TEND1(I,J,LM)
  150 CONTINUE
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C--------------PIECEWISE LINEAR UPSTREAM VERTICAL ADVECTION ------------
C-------------------------- OF Q AND CLOUD -----------------------------
C-----------------------------------------------------------------------
      ALLOCATE(WATER(IDIM1:IDIM2,JDIM1:JDIM2,LM),STAT=I)
C-----------------------------------------------------------------------
C       INTIALIZE Q AT THE BOTTOM INTERFACE AND THE SLOPE ADJUSTMENT
C       MASK (SAM=1 FOR SA PERMITTED, 0 FOR NOT PERMITTED)
C-----------------------------------------------------------------------
C
C***  LOOP OVER WATER VARIABLES
C
      DO 400 NWTR=1,2
C-----------------------------------------------------------------------
      IF(NWTR.EQ.1)THEN
!$omp parallel do
        DO L=1,LM
          DO J=JDIM1,JDIM2
          DO I=IDIM1,IDIM2
            WATER(I,J,L)=Q(I,J,L)
          ENDDO
          ENDDO
        ENDDO
      ELSE
!$omp parallel do
        DO L=1,LM
          DO J=JDIM1,JDIM2
          DO I=IDIM1,IDIM2
            WATER(I,J,L)=CWM(I,J,L)
          ENDDO
          ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
!$omp parallel do
                             DO 175 L=1,LM
          DO 175 J=MYJS2,MYJE2
          DO 175 I=MYIS,MYIE
      QBI(I,J,L)=WATER(I,J,L)
      SAM(I,J,L)=1.
  175 CONTINUE
      IF(NOSLA) GO TO 290
C--------------THE SLOPE ADJUSTMENT CODE--------------------------------
C       NO SLOPE PERMITTED AT THE TOP AND AT THE BOTTOM LAYER
C-----------------------------------------------------------------------
!$omp parallel do
          DO 190 J=MYJS2,MYJE2
          DO 190 I=MYIS,MYIE
      SAM(I,J, 1)=0.
      SAM(I,J,LM)=0.
  190 CONTINUE
C
!$omp parallel do
          DO 200 L=1,LM1
      DO 200 J=MYJS2,MYJE2
      DO 200 I=MYIS,MYIE
      SAM(I,J,L)=SAM(I,J,L)*HTM(I,J,L+1)
  200 CONTINUE
C-----------------------------------------------------------------------
C       NOW, SEARCH FOR THE MAXIMA AND MINIMA OF Q (AT THE FIRST
C       PASS) AND FOR LAYERS WHICH HAD OVERADJUSTED (AT SUBSEQUENT
C       PASSES) DUE TO ROUND-OFF ERRORS
C-----------------------------------------------------------------------
!$omp parallel do private(dqbi,dqti,extrem)
                             DO 220 L=2,LM1
      DO 220 J=MYJS2,MYJE2
      DO 220 I=MYIS,MYIE
        DQTI(I,J)=WATER(I,J,L)-WATER(I,J,L-1)
        DQBI(I,J)=WATER(I,J,L+1)-WATER(I,J,L)
        EXTREM=DQTI(I,J)*DQBI(I,J)
        IF(EXTREM.LE.0.)SAM(I,J,L)=0.
  220 CONTINUE
C
!$omp parallel do 
      DO 230 L=2,LM1
      DO 230 J=MYJS2,MYJE2
      DO 230 I=MYIS,MYIE
        ARRAY1(I,J,L)=WFA(L-1)*(1.-SAM(I,J,L-1))+WFB(L-1)
        ARRAY2(I,J,L)=WFA(L)+WFB(L)*(1.-SAM(I,J,L+1))
  230 CONTINUE
                             DO 260 MSA=1,NMSAP
C-----------------------------------------------------------------------
C       CALCULATE DQ AT INTERFACES AND ADJUST THE SLOPES WHERE
C       AND TO THE EXTENT PERMITTED OBSERVING THE MONOTONICITY
C       CONDITION (E.G. VAN LEER, J. COMP. PHYS. 1977, 276-299)
C-----------------------------------------------------------------------
!$omp parallel do
          DO 240 J=MYJS2,MYJE2
          DO 240 I=MYIS,MYIE
      DQBI(I,J)=2.*WATER(I,J,2)-QBI(I,J,2) -QBI(I,J,1)
  240 CONTINUE
C
                             DO 250 L=2,LM1
!$omp parallel do private(asbik,astik,dqtik)
          DO 250 J=MYJS2,MYJE2
          DO 250 I=MYIS,MYIE
      DQTIK  =DQBI(I,J)
      ASTIK  =ARRAY1(I,J,L)*DQTIK
      DQBI(I,J)=2.*WATER(I,J,L+1)-QBI(I,J,L+1)-QBI(I,J,L)
      ASBIK  =ARRAY2(I,J,L)*DQBI(I,J)
      QBI(I,J,L)=QBI(I,J,L)
     1         +(ASTIK-SIGN(1.,ASTIK)
     2         *DIM(ABS(ASTIK),ABS(ASBIK)))*SAM(I,J,L)
  250 CONTINUE
  260                        CONTINUE
C-----------------------------------------------------------------------
C       SLOPE ADJUSTMENT OF THE LAYERS ABOVE THAT NEXT TO THE SURFACE
C       IS DONE; NOW ADJUST THE LOWERMOST LAYER
C-----------------------------------------------------------------------
                             DO 270 L=9,LM1
!$omp parallel do 
          DO 270 J=MYJS2,MYJE2
          DO 270 I=MYIS,MYIE
      IF(HTM(I,J,L+1).EQ.0.)QBI(I,J,L)=2.*WATER(I,J,L)-QBI(I,J,L-1)
  270 CONTINUE
C
!$omp parallel do
          DO 280 J=MYJS2,MYJE2
          DO 280 I=MYIS,MYIE
      QBI(I,J,LM)=2.*WATER(I,J,LM)-QBI(I,J,LM1)
  280 CONTINUE
C-----------------------------------------------------------------------
C--------------END OF THE SLOPE ADJUSTMENT CODE-------------------------
C-----------------------------------------------------------------------
  290 CONTINUE
!$omp parallel do
          DO 300 J=MYJS2,MYJE2
          DO 300 I=MYIS,MYIE
      QDEDB(I,J)=0.
      QDEUB(I,J)=0.
      DQDEB(I,J)=2.*(QBI(I,J,1)-WATER(I,J,1))*RDETA(1)
      EDBD (I,J)=0.
  300 CONTINUE
C
                             DO 320 L=1,LM1
!$omp parallel do private(dqdek,edbfk,edtdk,qdedtk,qdeutk,sedbk)
          DO 320 J=MYJS2,MYJE2
          DO 320 I=MYIS,MYIE
      QDEDTK  =QDEDB(I,J)
      QDEUTK  =QDEUB(I,J)
      SEDBK   =SIGN(1.,ETADT(I,J,L))
      DQDEK   =DQDEB(I,J)
      DQDEB(I,J)=2.*(QBI(I,J,L+1)-WATER(I,J,L+1))*RDETA(L+1)
      EDBFK   =ETADT(I,J,L)*F4D
      QDEDB(I,J)=(1.+SEDBK)*(QBI(I,J,L)+DQDEK*EDBFK)*(-EDBFK)
      QDEUB(I,J)=(1.-SEDBK)*(2.*WATER(I,J,L+1)-QBI(I,J,L+1)
     1           +DQDEB(I,J)*EDBFK)*EDBFK
      EDTDK   =EDBD(I,J)
      EDBD (I,J)=ETADT(I,J,L)*(-F4Q)
      WATER(I,J,L)=WATER(I,J,L)+(QDEDTK-QDEUTK-QDEDB(I,J)+QDEUB(I,J)
     1                     +WATER(I,J,L)*(EDBD(I,J)-EDTDK))*RDETA(L)
  320 CONTINUE
C
!$omp parallel do 
          DO 330 J=MYJS2,MYJE2
          DO 330 I=MYIS,MYIE
      WATER(I,J,LM)=WATER(I,J,LM)+(QDEDB(I,J)-QDEUB(I,J)
     1                    +WATER(I,J,LM)*(-EDBD(I,J)))*RDETA(LM)
  330 CONTINUE
C-------NEGATIVE MOISTURE MAY OCCUR DUE TO VIOLATION OF THE CFL---------
                             DO 350 L=1,LM1
!$omp parallel do
          DO 350 J=MYJS2,MYJE2
          DO 350 I=MYIS,MYIE
      IF(WATER(I,J,L).LT.EPSQ)THEN
        DQBI(I,J)=WATER(I,J,L)
        WATER(I,J,L)=EPSQ
        WATER(I,J,L+1)=WATER(I,J,L+1)+DETA(L)*RDETA(L+1)*DQBI(I,J)
      ENDIF
  350 CONTINUE
C
!$omp parallel do
          DO 360 J=MYJS2,MYJE2
          DO 360 I=MYIS,MYIE
      IF(WATER(I,J,LM).LT.EPSQ)WATER(I,J,LM)=EPSQ
  360 CONTINUE
C-----------------------------------------------------------------------
      IF(NWTR.EQ.1)THEN
!$omp parallel do
        DO L=1,LM
          DO J=JDIM1,JDIM2
          DO I=IDIM1,IDIM2
            Q(I,J,L)=WATER(I,J,L)
          ENDDO
          ENDDO
        ENDDO
      ELSE
!$omp parallel do
        DO L=1,LM
          DO J=JDIM1,JDIM2
          DO I=IDIM1,IDIM2
            CWM(I,J,L)=WATER(I,J,L)
          ENDDO
          ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
  400 CONTINUE
C
      DEALLOCATE(WATER,STAT=IER)
C-----------------------------------------------------------------------
C--------------VERTICAL (MATSUNO) ADVECTION OF Q2-----------------------
C-----------------------------------------------------------------------
!$omp parallel do
          DO 420 J=MYJS2,MYJE2
          DO 420 I=MYIS,MYIE
      TQ2B(I,J)=Q2(I,J,1)*ETADT(I,J,1)*F4Q2(1)
  420 CONTINUE
C
                             DO 425 L=1,LM2
!$omp parallel do private(tq2ak)
          DO 425 J=MYJS2,MYJE2
          DO 425 I=MYIS,MYIE
      TQ2AK=(Q2(I,J,L+1)-Q2(I,J,L))*(ETADT(I,J,L)+ETADT(I,J,L+1))
     1                              *F4Q2(L+1)
      Q2ST(I,J,L)=TQ2AK+TQ2B(I,J)+Q2(I,J,L)
      TQ2B(I,J)=TQ2AK
  425 CONTINUE
C
!$omp parallel do private(tq2ak)
          DO 440 J=MYJS2,MYJE2
          DO 440 I=MYIS,MYIE
      TQ2AK=(Q2(I,J,LM)-Q2(I,J,LM1))*ETADT(I,J,LM1)*F4Q2(LM)
      Q2ST(I,J,LM1)=TQ2AK+TQ2B(I,J)+Q2(I,J,LM1)
      Q2ST(I,J,LM )=Q2(I,J,LM)
  440 CONTINUE
C-----------------------------------------------------------------------
C--------------SECOND (BACKWARD) MATSUNO STEP---------------------------
C-----------------------------------------------------------------------
!$omp parallel do 
          DO 450 J=MYJS2,MYJE2
          DO 450 I=MYIS,MYIE
      TQ2B(I,J)=Q2ST(I,J,1)*ETADT(I,J,1)*F4Q2(1)
  450 CONTINUE
C
      DO L=1,LM2
!$omp parallel do private(tq2ak)
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
          TQ2AK  =(Q2ST(I,J,L+1)-Q2ST(I,J,L))
     1           *(ETADT(I,J,L)+ETADT(I,J,L+1))*F4Q2(L+1)
          VAD_TEND1(I,J,L)=TQ2AK+TQ2B(I,J)
          TQ2B(I,J)=TQ2AK
        ENDDO
        ENDDO
      ENDDO
!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        TQ2AK  =(Q2ST(I,J,LM)-Q2ST(I,J,LM1))*ETADT(I,J,LM1)*F4Q2(LM)
        VAD_TEND1(I,J,LM1)=TQ2AK+TQ2B(I,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  IF THE CFL CRITERION IS VIOLATED THEN VERTICALLY SMOOTH
!***  THE TENDENCY
!
!-----------------------------------------------------------------------
!
!$omp parallel do
!$omp& private(cfl,lbot_cfl,lstart,lstop,ltop_cfl,vad_tend1,vad_tndx1)
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
!
        IF(LTOP_CFL_T(I,J).GT.0)THEN
          LSTART=LTOP_CFL_T(I,J)
          LSTOP =MIN(LBOT_CFL_T(I,J),LM-2)
!
          DO L=LSTART,LSTOP
            VAD_TNDX1(L)=(VAD_TEND1(I,J,L-1)+VAD_TEND1(I,J,L+1)
     1                   +2.*VAD_TEND1(I,J,L))*0.25
          ENDDO
          DO L=LSTART,LSTOP
            VAD_TEND1(I,J,L)=VAD_TNDX1(L)
          ENDDO
        ENDIF
!
      ENDDO
      ENDDO
C
                             DO 470 L=1,LM2
!$omp parallel do 
          DO 470 J=MYJS2,MYJE2
          DO 470 I=MYIS,MYIE
      Q2(I,J,L)=VAD_TEND1(I,J,L)+Q2(I,J,L)
      Q2(I,J,L)=AMAX1(Q2(I,J,L),EPSQ2)
  470 CONTINUE
C
!$omp parallel do 
          DO 480 J=MYJS2,MYJE2
          DO 480 I=MYIS,MYIE
      Q2(I,J,LM1)=VAD_TEND1(I,J,LM1)+Q2(I,J,LM1)
      Q2(I,J,LM1)=AMAX1(Q2(I,J,LM1),EPSQ2)
  480 CONTINUE
C-----------------------------------------------------------------------
C--------------DEFINITION OF VARIABLES NEEDED AT V POINTS---------------
C-----------------------------------------------------------------------
!$omp parallel do 
                             DO 500 L=1,LM1
          DO 500 J=MYJS_P1,MYJE_P1
          DO 500 I=MYIS_P1,MYIE_P1
      ETADT(I,J,L)=ETADT(I,J,L)*PDSL(I,J)*HBM2(I,J)
  500 CONTINUE
C
!$omp parallel do
          DO 510 J=MYJS2,MYJE2
          DO 510 I=MYIS,MYIE
      RPDX(I,J)=1./(PDSL(I+IVW(J),J)+PDSL(I+IVE(J),J))
      RPDY(I,J)=1./(PDSL(I,J-1)+PDSL(I,J+1))
  510 CONTINUE
C-----------------------------------------------------------------------
C--------------VERTICAL (MATSUNO) ADVECTION OF U & V--------------------
C-----------------------------------------------------------------------
!$omp parallel do
          DO 520 J=MYJS,MYJE
          DO 520 I=MYIS,MYIE
      TUB(I,J)=0.
      TVB(I,J)=0.
  520 CONTINUE
C
      DO L=1,LM1
!$omp parallel do private(tuak,tvak,vmk)
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
          VMK   =VTM(I,J,L+1)*VBM2(I,J)
          TUAK  =(ETADT(I+IVW(J),J,L)+ETADT(I+IVE(J),J,L))
     1           *(U(I,J,L+1)-U(I,J,L))*RPDX(I,J)*(VMK*F4D)
          VAD_TEND1(I,J,L)=(TUAK+TUB(I,J))*RDETA(L)
          TUB(I,J)=TUAK
          TVAK  =(ETADT(I,J-1,L)+ETADT(I,J+1,L))*(V(I,J,L+1)-V(I,J,L))
     1           *RPDY(I,J)*(VMK*F4D)
          VAD_TEND2(I,J,L)=(TVAK+TVB(I,J))*RDETA(L)
          TVB(I,J)=TVAK
        ENDDO
        ENDDO
      ENDDO
!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        VAD_TEND1(I,J,LM)=TUB(I,J)*RDETA(LM)
        VAD_TEND2(I,J,LM)=TVB(I,J)*RDETA(LM)
      ENDDO
      ENDDO
!
                             DO 540 L=1,LM1
!$omp parallel do private(tuak,tvak,vmk)
          DO 540 J=MYJS2,MYJE2
          DO 540 I=MYIS,MYIE
      USTL(I,J,L)=VAD_TEND1(I,J,L)+U(I,J,L) 
      VSTL(I,J,L)=VAD_TEND2(I,J,L)+V(I,J,L)
      TVB(I,J)=TVAK
  540 CONTINUE
C
!$omp parallel do 
          DO 550 J=MYJS2,MYJE2
          DO 550 I=MYIS,MYIE
      USTL(I,J,LM)=U(I,J,LM)+VAD_TEND1(I,J,LM)
      VSTL(I,J,LM)=V(I,J,LM)+VAD_TEND2(I,J,LM)
  550 CONTINUE
C-----------------------------------------------------------------------
C--------------SECOND (BACKWARD) MATSUNO STEP---------------------------
C-----------------------------------------------------------------------
!$omp parallel do
          DO 560 J=MYJS,MYJE
          DO 560 I=MYIS,MYIE
      TUB(I,J)=0.
      TVB(I,J)=0.
  560 CONTINUE
C
      DO L=1,LM1
!$omp parallel do private(tuak,tvak,vmk)
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
          VMK   =VTM(I,J,L+1)*VBM2(I,J)
          TUAK  =(ETADT(I+IVW(J),J,L)+ETADT(I+IVE(J),J,L))
     1           *(USTL(I,J,L+1)-USTL(I,J,L))*RPDX(I,J)*(VMK*F4D)
          VAD_TEND1(I,J,L)=(TUAK+TUB(I,J))*RDETA(L)
          TUB(I,J)=TUAK
          TVAK  =(ETADT(I,J-1,L)+ETADT(I,J+1,L))
     1           *(VSTL(I,J,L+1)-VSTL(I,J,L))*RPDY(I,J)*(VMK*F4D)
          VAD_TEND2(I,J,L)=(TVAK+TVB(I,J))*RDETA(L)
          TVB(I,J)=TVAK
        ENDDO
        ENDDO
      ENDDO
!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        VAD_TEND1(I,J,LM)=TUB(I,J)*RDETA(LM)
        VAD_TEND2(I,J,LM)=TVB(I,J)*RDETA(LM)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  IF THE CFL CRITERION IS VIOLATED THEN VERTICALLY SMOOTH
!***  THE TENDENCIES
!
!-----------------------------------------------------------------------
!
!$omp parallel do
!$omp& private(lstart,lstop,vad_tndx1,vad_tndx2)
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
!
!***  U COMPONENT
!
        IF(LTOP_CFL_U(I,J).GT.0)THEN
          LSTART=LTOP_CFL_U(I,J)
          LSTOP =MIN(LBOT_CFL_U(I,J),LM-1)
!
          DO L=LSTART,LSTOP
            VAD_TNDX1(L)=(VAD_TEND1(I,J,L-1)+VAD_TEND1(I,J,L+1)
     1                   +2.*VAD_TEND1(I,J,L))*0.25
          ENDDO
          DO L=LSTART,LSTOP
            VAD_TEND1(I,J,L)=VAD_TNDX1(L)
          ENDDO
        ENDIF
!
!***  V COMPONENT
!
        IF(LTOP_CFL_V(I,J).GT.0)THEN
          LSTART=LTOP_CFL_V(I,J)
          LSTOP =MIN(LBOT_CFL_V(I,J),LM-1)
!
          DO L=LSTART,LSTOP
            VAD_TNDX2(L)=(VAD_TEND2(I,J,L-1)+VAD_TEND2(I,J,L+1)
     1                   +2.*VAD_TEND2(I,J,L))*0.25
          ENDDO
          DO L=LSTART,LSTOP
            VAD_TEND2(I,J,L)=VAD_TNDX2(L)
          ENDDO
        ENDIF
!
      ENDDO
      ENDDO
C
                             DO 580 L=1,LM1
!$omp parallel do
          DO 580 J=MYJS2,MYJE2
          DO 580 I=MYIS,MYIE
      U(I,J,L)=VAD_TEND1(I,J,L)+U(I,J,L)
      V(I,J,L)=VAD_TEND2(I,J,L)+V(I,J,L)
  580 CONTINUE
C
!$omp parallel do
          DO 590 J=MYJS2,MYJE2
          DO 590 I=MYIS,MYIE
      U(I,J,LM)=U(I,J,LM)+VAD_TEND1(I,J,LM)
      V(I,J,LM)=V(I,J,LM)+VAD_TEND2(I,J,LM)
  590 CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
