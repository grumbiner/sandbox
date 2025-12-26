C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE DDAMP
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    DDAMP       DIVERGENCE DAMPING
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08       
C     
C ABSTRACT:
C     DDAMP MODIFIES THE WIND COMPONENTS SO AS TO REDUCE THE
C     HORIZONTAL DIVERGENCE.  A SWITCH PROVIDES THE OPTION OF
C     ALSO MODIFYING THE TEMPERATURE FROM AN ENERGY VIEWPOINT.
C     
C PROGRAM HISTORY LOG:
C   87-08-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-03-28  BLACK      - ADDED EXTERNAL EDGE
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL DDAMP FROM MAIN PROGRAM EBU
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
C                  MASKS
C                  DYNAM
C                  VRBLS
C                  CONTIN
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
                             P A R A M E T E R
     & (RFCP=.25/1004.6)
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10),LP1=LM+1)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,HEAT
C----------------------------------------------------------------------
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
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & RDPDX (IDIM1:IDIM2,JDIM1:JDIM2),RDPDY (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UT    (IDIM1:IDIM2,JDIM1:JDIM2),VT    (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & CKE   (IDIM1:IDIM2,JDIM1:JDIM2),DPDE  (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
      HEAT=.FALSE.
!$omp parallel do 
      DO 100 J=MYJS1_P1,MYJE1_P1
      DO 100 I=MYIS_P1,MYIE_P1
      CKE(I,J)=0.
  100 CONTINUE
C-----------------------------------------------------------------------
!$omp parallel do private(cke,dpde,rdpdx,rdpdy,ut,vt)
                             DO 150 L=1,LM
C-----------------------------------------------------------------------
      CALL ZERO2(CKE)
      CALL ZERO2(DPDE)
C
      DO 110 J=MYJS_P2,MYJE_P2
      DO 110 I=MYIS_P1,MYIE_P1
      DPDE(I,J)=DETA(L)*PDSL(I,J)
      DIV(I,J,L)=DIV(I,J,L)*HBM2(I,J)
  110 CONTINUE
C
      DO 120 J=MYJS2,MYJE2
      DO 120 I=MYIS_P1,MYIE_P1
      RDPDX(I,J)=VTM(I,J,L)/(DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J))
      RDPDY(I,J)=VTM(I,J,L)/(DPDE(I,J-1)+DPDE(I,J+1))
  120 CONTINUE
C
      DO 130 J=MYJS2,MYJE2
      DO 130 I=MYIS1_P1,MYIE1_P1
      UT(I,J)=U(I,J,L)
      VT(I,J)=V(I,J,L)
      U(I,J,L)=U(I,J,L)+(DIV(I+IVE(J),J,L)-DIV(I+IVW(J),J,L))
     1                  *RDPDX(I,J)*DDMPU(I,J)
      V(I,J,L)=V(I,J,L)
     1         +(DIV(I,J+1,L)-DIV(I,J-1,L))*RDPDY(I,J)*DDMPV(I,J)
      CKE(I,J)=0.5*(U(I,J,L)*U(I,J,L)-UT(I,J)*UT(I,J)
     1             +V(I,J,L)*V(I,J,L)-VT(I,J)*VT(I,J))
  130 CONTINUE
C
      IF(HEAT)THEN
        DO 140 J=MYJS2,MYJE2
        DO 140 I=MYIS_P1,MYIE_P1
        T(I,J,L)=T(I,J,L)-RFCP*(CKE(I+IHE(J),J)+CKE(I,J+1)
     1                         +CKE(I+IHW(J),J)+CKE(I,J-1))*HBM2(I,J)
  140   CONTINUE
      ENDIF
  150                        CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
