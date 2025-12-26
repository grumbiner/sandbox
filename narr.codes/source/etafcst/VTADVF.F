C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE VTADVF
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    VTADVF      VERTICAL ADVECTION
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-11-17
C     
C ABSTRACT:
C     VTADVF CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION
C     TO THE TENDENCIES OF TEMPERATURE, WIND COMPONENTS, AND TURBULENT
C     KINETIC ENERGY AND THEN UPDATES THOSE VARIABLES.  FOR ALL THESE
C     VARIABLES A SIMPLE CENTERED DIFFERENCE SCHEME IN SPACE IS USED
C     IN CONJUNCTION WITH THE PURE EULER-BACKWARD TIME SCHEME.  
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   90-??-??  MESINGER   - INSERTED PIECEWISE LINEAR SCHEME FOR
C                          SPECIFIC HUMIDITY
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-11-20  ABELES     - PARALLEL OPTIMIZATION
C   96-03-29  BLACK      - ADDED EXTERNAL EDGE; REMOVED SCRCH COMMON
C   98-11-24  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL VTADVF FROM MAIN SUBROUTINE DIGFILT
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
C                  PVRBLS
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
                             P A R A M E T E R
     & (EDQMX=2.E-5,EDQMN=-2.E-5,EPSQ=1.E-12,KSMUD=0)
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
     &,VM    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,RPDX  (IDIM1:IDIM2,JDIM1:JDIM2),RPDY  (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & FNE (IDIM1:IDIM2,JDIM1:JDIM2),FSE (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & TSTL  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,USTL  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,VSTL  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,Q2ST  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
C-----------------------------------------------------------------------
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
C--------------VERTICAL (MATSUNO) ADVECTION OF T------------------------
!$omp parallel do 
          DO 100 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
      TTB(I,J)=0.
  100 CONTINUE
C
                             DO 110 L=1,LM1
!$omp parallel do private(ttak)
          DO 110 J=MYJS2,MYJE2
          DO 110 I=MYIS,MYIE
      TTAK  =(T(I,J,L+1)-T(I,J,L))*ETADT(I,J,L)*F4D
      TSTL(I,J,L)=(TTAK  +TTB(I,J))*RDETA(L)+T(I,J,L)
      TTB(I,J)=TTAK
  110 CONTINUE
C
!$omp parallel do 
          DO 120 J=MYJS2,MYJE2
          DO 120 I=MYIS,MYIE
      TSTL(I,J,LM)=T(I,J,LM)+TTB(I,J)*RDETA(LM)
  120 CONTINUE
C--------------SECOND (BACKWARD) MATSUNO STEP---------------------------
!$omp parallel do
          DO 125 J=MYJS,MYJE
          DO 125 I=MYIS,MYIE
      TTB(I,J)=0.
  125 CONTINUE
C
                             DO 140 L=1,LM1
!$omp parallel do private(ttak)
          DO 140 J=MYJS2,MYJE2
          DO 140 I=MYIS,MYIE
      TTAK  =(TSTL(I,J,L+1)-TSTL(I,J,L))*ETADT(I,J,L)*F4D
      T(I,J,L)=(TTAK  +TTB(I,J))*RDETA(L)+T(I,J,L)
      TTB(I,J)=TTAK
  140 CONTINUE
C
!$omp parallel do 
          DO 150 J=MYJS2,MYJE2
          DO 150 I=MYIS,MYIE
      T(I,J,LM)=T(I,J,LM)+TTB(I,J)*RDETA(LM)
  150 CONTINUE
C--------------VERTICAL (MATSUNO) ADVECTION OF Q2-----------------------
!$omp parallel do
          DO 400 J=MYJS2,MYJE2
          DO 400 I=MYIS,MYIE
      TQ2B(I,J)=Q2(I,J,1)*ETADT(I,J,1)*F4Q2(1)
  400 CONTINUE
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
C--------------SECOND (BACKWARD) MATSUNO STEP---------------------------
!$omp parallel do 
          DO 450 J=MYJS2,MYJE2
          DO 450 I=MYIS,MYIE
      TQ2B(I,J)=Q2ST(I,J,1)*ETADT(I,J,1)*F4Q2(1)
  450 CONTINUE
C
                             DO 470 L=1,LM2
!$omp parallel do private(tq2ak)
          DO 470 J=MYJS2,MYJE2
          DO 470 I=MYIS,MYIE
      TQ2AK  =(Q2ST(I,J,L+1)-Q2ST(I,J,L))
     1       *(ETADT(I,J,L)+ETADT(I,J,L+1))*F4Q2(L+1)
      Q2(I,J,L)=TQ2AK+TQ2B(I,J)+Q2(I,J,L)
      TQ2B(I,J)=TQ2AK
  470 CONTINUE
C
!$omp parallel do private(tq2ak)
          DO 480 J=MYJS2,MYJE2
          DO 480 I=MYIS,MYIE
      TQ2AK  =(Q2ST(I,J,LM)-Q2ST(I,J,LM1))*ETADT(I,J,LM1)*F4Q2(LM)
      Q2(I,J,LM1)=TQ2AK+TQ2B(I,J)+Q2(I,J,LM1)
  480 CONTINUE
C--------------DEFINITION OF VARIABLES NEEDED AT V POINTS---------------
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
C--------------VERTICAL (MATSUNO) ADVECTION OF U & V--------------------
!$omp parallel
          DO 520 J=MYJS,MYJE
          DO 520 I=MYIS,MYIE
      TUB(I,J)=0.
      TVB(I,J)=0.
  520 CONTINUE
C
                             DO 540 L=1,LM1
!$omp parallel do private(tuak,tvak,vmk)
          DO 540 J=MYJS2,MYJE2
          DO 540 I=MYIS,MYIE
      VMK   =VTM(I,J,L+1)*VBM2(I,J)
      TUAK  =(ETADT(I+IVW(J),J,L)+ETADT(I+IVE(J),J,L))
     1       *(U(I,J,L+1)-U(I,J,L))*RPDX(I,J)*(VMK*F4D)
      USTL(I,J,L)=(TUAK+TUB(I,J))*RDETA(L)+U(I,J,L)
      TUB(I,J)=TUAK
      TVAK  =(ETADT(I,J-1,L)+ETADT(I,J+1,L))*(V(I,J,L+1)-V(I,J,L))
     1       *RPDY(I,J)*(VMK*F4D)
      VSTL(I,J,L)=(TVAK+TVB(I,J))*RDETA(L)+V(I,J,L)
      TVB(I,J)=TVAK
  540 CONTINUE
C
!$omp parallel do
          DO 550 J=MYJS2,MYJE2
          DO 550 I=MYIS,MYIE
      USTL(I,J,LM)=U(I,J,LM)+TUB(I,J)*RDETA(LM)
      VSTL(I,J,LM)=V(I,J,LM)+TVB(I,J)*RDETA(LM)
  550 CONTINUE
C--------------SECOND (BACKWARD) MATSUNO STEP---------------------------
!$omp parallel do
          DO 560 J=MYJS,MYJE
          DO 560 I=MYIS,MYIE
      TUB(I,J)=0.
      TVB(I,J)=0.
  560 CONTINUE
C
                             DO 580 L=1,LM1
!$omp parallel do private(tuak,tvak,vmk)
          DO 580 J=MYJS2,MYJE2
          DO 580 I=MYIS,MYIE
      VMK   =VTM(I,J,L+1)*VBM2(I,J)
      TUAK  =(ETADT(I+IVW(J),J,L)+ETADT(I+IVE(J),J,L))
     1       *(USTL(I,J,L+1)-USTL(I,J,L))*RPDX(I,J)*(VMK*F4D)
      U(I,J,L)=(TUAK+TUB(I,J))*RDETA(L)+U(I,J,L)
      TUB(I,J)=TUAK
      TVAK  =(ETADT(I,J-1,L)+ETADT(I,J+1,L))
     1       *(VSTL(I,J,L+1)-VSTL(I,J,L))*RPDY(I,J)*(VMK*F4D)
      V(I,J,L)=(TVAK+TVB(I,J))*RDETA(L)+V(I,J,L)
      TVB(I,J)=TVAK
  580 CONTINUE
C
!$omp parallel do 
          DO 590 J=MYJS2,MYJE2
          DO 590 I=MYIS,MYIE
      U(I,J,LM)=U(I,J,LM)+TUB(I,J)*RDETA(LM)
      V(I,J,LM)=V(I,J,LM)+TVB(I,J)*RDETA(LM)
  590 CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
