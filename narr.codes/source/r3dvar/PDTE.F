C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE PDTE
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    PDTE        SURFACE PRESSURE TENDENCY CALC
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08       
C     
C ABSTRACT:
C     PDTE VERTICALLY INTEGRATES THE MASS FLUX DIVERGENCE TO
C     OBTAIN THE SURFACE PRESSURE TENDENCY AND ETADOT ON THE
C     LAYER INTERFACES.
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-11-20  ABELES     - PARALLEL OPTIMIZATION
C   96-03-29  BLACK      - REMOVED SCRCH
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL PDTE FROM MAIN PROGRAM EBU
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
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
!                            P A R A M E T E R
!    & (CP=1004.6)
C-----------------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R
!    & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10),LM1=LM-1,LP1=LM+1)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & PRET  (IDIM1:IDIM2,JDIM1:JDIM2),RPSL  (IDIM1:IDIM2,JDIM1:JDIM2)
C    &,TT    (IDIM1:IDIM2,JDIM1:JDIM2),CPEV  (IDIM1:IDIM2,JDIM1:JDIM2)
C    &,DPDE  (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------

      cp=1004.6
C-----------------------------------------------------------------------
C--------------COMPUTATION OF PRESSURE TENDENCY & PREPARATIONS----------
                             DO 100 L=2,LM
! !$omp parallel do 
      DO 100 J=MYJS,MYJE2
      DO 100 I=MYIS,MYIE
      DIV(I,J,L)=DIV(I,J,L-1)+DIV(I,J,L)
  100 CONTINUE
C
! !$omp parallel do 
      DO 110 J=MYJS2,MYJE2
      DO 110 I=MYIS,MYIE
      PSDT(I,J)=-DIV(I,J,LM)
      PRET(I,J)=PSDT(I,J)*RES(I,J)
      RPSL(I,J)=1./PDSL(I,J)
  110 CONTINUE
C--------------COMPUTATION OF ETADT-------------------------------------
! !$omp parallel do 
                             DO 120 L=1,LM1
      DO 120 J=MYJS2,MYJE2
      DO 120 I=MYIS,MYIE
      ETADT(I,J,L)=-(PRET(I,J)*ETA(L+1)+DIV(I,J,L))
     1              *HTM(I,J,L+1)*RPSL(I,J)
  120 CONTINUE
C--------------KINETIC ENERGY GENERATION TERMS IN T EQUATION------------
! !$omp parallel do 
      DO 130 J=MYJS2,MYJE2
      DO 130 I=MYIS,MYIE
C     DPDE(I,J)=DETA(1)*PDSL(I,J)
C     TT(I,J)=T(I,J,1)
      OMGALF(I,J,1)=OMGALF(I,J,1)-DIV(I,J,1)*RTOP(I,J,1)*EF4T
      T(I,J,1)=T(I,J,1)-DIV(I,J,1)*RTOP(I,J,1)*EF4T
C     CPEV(I,J)=CP*(T(I,J,1)-TT(I,J))*DPDE(I,J)
  130 CONTINUE
C-----------------------------------------------------------------------
! !$omp parallel do
                             DO 145 L=2,LM1
      DO 140 J=MYJS2,MYJE2
      DO 140 I=MYIS,MYIE
C     DPDE(I,J)=DETA(L)*PDSL(I,J)
C     TT(I,J)=T(I,J,L)
      OMGALF(I,J,L)=OMGALF(I,J,L)-(DIV(I,J,L-1)+DIV(I,J,L))
     1                            *RTOP(I,J,L)*EF4T
      T(I,J,L)=T(I,J,L)-(DIV(I,J,L-1)+DIV(I,J,L))*RTOP(I,J,L)*EF4T
C     CPEV(I,J)=CP*(T(I,J,L)-TT(I,J))*DPDE(I,J)
  140 CONTINUE
  145                        CONTINUE
C-----------------------------------------------------------------------
! !$omp parallel do
      DO 150 J=MYJS2,MYJE2
      DO 150 I=MYIS,MYIE
C     DPDE(I,J)=DETA(LM)*PDSL(I,J)
C     TT(I,J)=T(I,J,LM)
      OMGALF(I,J,LM)=OMGALF(I,J,LM)+(PRET(I,J)-DIV(I,J,LM1))
     1                              *RTOP(I,J,LM)*EF4T
      T(I,J,LM)=T(I,J,LM)+(PRET(I,J)-DIV(I,J,LM1))*RTOP(I,J,LM)*EF4T
C     CPEV(I,J)=CP*(T(I,J,LM)-TT(I,J))*DPDE(I,J)
  150 CONTINUE
C
                             DO 160 L=LM,2,-1
! !$omp parallel do
      DO 160 J=MYJS,MYJE2
      DO 160 I=MYIS,MYIE
      DIV(I,J,L)=DIV(I,J,L)-DIV(I,J,L-1)
  160 CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
