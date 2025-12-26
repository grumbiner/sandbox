                             SUBROUTINE PGCOR
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    PGCOR       PRESSURE GRADIENT/CORIOLIS CALC
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-10-28       
C     
C ABSTRACT:
C     PGCOR CALCULATES THE PRESSURE GRADIENT FORCE, UPDATES THE
C     VELOCITY COMPONENTS DUE TO THE EFFECT OF THE PRESSURE GRADIENT
C     AND CORIOLIS FORCES.
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-29  BLACK      - ADDED EXTERNAL EDGE
C   97-03-17  MESINGER   - SPLIT FROM PFDHT
C   98-10-28  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL PFDHT FROM MAIN PROGRAM EBU
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
C***********************************************************************
      INCLUDE "PARMETA.comm"                                            
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R                          
!    & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10),LP1=LM+1)
C-----------------------------------------------------------------------
                             L O G I C A L                              
     & RUN,FIRST,RESTRT,SIGMA                                           
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
     & FILO  (IDIM1:IDIM2,JDIM1:JDIM2),RDPD  (IDIM1:IDIM2,JDIM1:JDIM2)  
     &,ADPDX (IDIM1:IDIM2,JDIM1:JDIM2),RDPDX (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADPDY (IDIM1:IDIM2,JDIM1:JDIM2),RDPDY (IDIM1:IDIM2,JDIM1:JDIM2)  
     &,FIUP  (IDIM1:IDIM2,JDIM1:JDIM2),ADPDNE(IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADPDSE(IDIM1:IDIM2,JDIM1:JDIM2)                                  
     &,PEW   (IDIM1:IDIM2,JDIM1:JDIM2),PNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PCEW  (IDIM1:IDIM2,JDIM1:JDIM2),PCNS  (IDIM1:IDIM2,JDIM1:JDIM2)  
     &,DPFEW (IDIM1:IDIM2,JDIM1:JDIM2),DPFNS (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNS   (IDIM1:IDIM2,JDIM1:JDIM2),TNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DPNE  (IDIM1:IDIM2,JDIM1:JDIM2),DPSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DCNE  (IDIM1:IDIM2,JDIM1:JDIM2),DCSE  (IDIM1:IDIM2,JDIM1:JDIM2)  
     &,DPFNE (IDIM1:IDIM2,JDIM1:JDIM2),DPFSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,VM    (IDIM1:IDIM2,JDIM1:JDIM2),F0    (IDIM1:IDIM2,JDIM1:JDIM2)                  
     &,UP    (IDIM1:IDIM2,JDIM1:JDIM2),VP    (IDIM1:IDIM2,JDIM1:JDIM2)                  
     &,PVNE  (IDIM1:IDIM2,JDIM1:JDIM2),PVSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,HM    (IDIM1:IDIM2,JDIM1:JDIM2)                  
C
                             D I M E N S I O N                          
     & DPDE(IDIM1:IDIM2,JDIM1:JDIM2),FIM (IDIM1:IDIM2,JDIM1:JDIM2)
     &,APEL(IDIM1:IDIM2,JDIM1:JDIM2),PCXC(IDIM1:IDIM2,JDIM1:JDIM2)
     &,UDY (IDIM1:IDIM2,JDIM1:JDIM2),VDX (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TEW (IDIM1:IDIM2,JDIM1:JDIM2),FEW (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TNE (IDIM1:IDIM2,JDIM1:JDIM2),TSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNE (IDIM1:IDIM2,JDIM1:JDIM2),FSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PNE (IDIM1:IDIM2,JDIM1:JDIM2),PSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,CNE (IDIM1:IDIM2,JDIM1:JDIM2),CSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PPNE(IDIM1:IDIM2,JDIM1:JDIM2),PPSE(IDIM1:IDIM2,JDIM1:JDIM2)
     &,PCNE(IDIM1:IDIM2,JDIM1:JDIM2),PCSE(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL ZERO2(DPDE)
      CALL ZERO2(FIM)
      CALL ZERO2(APEL)
      CALL ZERO2(PCXC)
      CALL ZERO2(UDY)
      CALL ZERO2(VDX)
      CALL ZERO2(TEW)
      CALL ZERO2(FEW)
      CALL ZERO2(TNE)
      CALL ZERO2(TSE)
      CALL ZERO2(FNE)
      CALL ZERO2(FSE)
      CALL ZERO2(PNE)
      CALL ZERO2(CNE)
      CALL ZERO2(PSE)
      CALL ZERO2(CSE)
      CALL ZERO2(PPNE)
      CALL ZERO2(PCNE)
      CALL ZERO2(PPSE)
      CALL ZERO2(PCSE)
C-----------------------------------------------------------------------
C--------------PREPARATORY CALCULATIONS---------------------------------
C-----------------------------------------------------------------------
      IF(SIGMA)THEN
! !$omp parallel do 
        DO 50 J=MYJS_P4,MYJE_P4
        DO 50 I=MYIS_P4,MYIE_P4
        FILO(I,J)=FIS(I,J)
        PDSL(I,J)=PD(I,J)
   50   CONTINUE
      ELSE
! !$omp parallel do 
        DO 100 J=MYJS_P4,MYJE_P4
        DO 100 I=MYIS_P4,MYIE_P4
        FILO(I,J)=0.
        PDSL(I,J)=RES(I,J)*PD(I,J)
  100   CONTINUE
      ENDIF
C
! !$omp parallel do 
      DO J=MYJS_P4,MYJE_P4
      DO I=MYIS_P4,MYIE_P4
        ADPDX(I,J)=0.
        ADPDY(I,J)=0.
      ENDDO
      ENDDO
C--------------MAIN VERTICAL INTEGRATION LOOP---------------------------
                             DO 400 L=LM,1,-1
C-----------------------------------------------------------------------
! !$omp parallel do 
      DO 210 J=MYJS_P4,MYJE_P4
      DO 210 I=MYIS_P4,MYIE_P4
      DPDE(I,J)=DETA(L)*PDSL(I,J)
  210 CONTINUE
C
! !$omp parallel do 
      DO 215 J=MYJS,MYJE
      DO 215 I=MYIS,MYIE
      RDPD(I,J)=1./DPDE(I,J)
  215 CONTINUE
C
! !$omp parallel do 
      DO 220 J=MYJS1_P2,MYJE1_P2
      DO 220 I=MYIS_P2,MYIE1_P2
      ADPDX(I,J)=DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J)
      ADPDY(I,J)=DPDE(I,J-1)+DPDE(I,J+1)
      RDPDX(I,J)=1./ADPDX(I,J)
      RDPDY(I,J)=1./ADPDY(I,J)
  220 CONTINUE
C-----------------------------------------------------------------------
! !$omp parallel do 
      DO 230 J=MYJS_P4,MYJE_P4
      DO 230 I=MYIS_P4,MYIE_P4
      APEL(I,J)=PT+AETA(L)*PDSL(I,J)
      RTOP(I,J,L)=R*T(I,J,L)*(1.+0.608*Q(I,J,L))/APEL(I,J)
      FIUPK=FILO(I,J)+RTOP(I,J,L)*DPDE(I,J)
      FIM(I,J)=FILO(I,J)+FIUPK
      FILO(I,J)=DFL(L)+HTM(I,J,L)*(FIUPK-DFL(L))
  230 CONTINUE
C--------------DIAGONAL CONTRIBUTIONS TO PRESSURE GRADIENT FORCE--------
! !$omp parallel do 
      DO 240 J=MYJS_P3,MYJE_P3
      DO 240 I=MYIS_P3,MYIE_P3
      ADPDNE(I,J)=DPDE(I+IHE(J),J+1)+DPDE(I,J)
      PNE(I,J)=2.*(FIM(I+IHE(J),J+1)-FIM(I,J))
      PPNE(I,J)=PNE(I,J)*ADPDNE(I,J)
      CNE(I,J)=2.*(RTOP(I+IHE(J),J+1,L)+RTOP(I,J,L))
     1           *(APEL(I+IHE(J),J+1)-APEL(I,J))
      PCNE(I,J)=CNE(I,J)*ADPDNE(I,J)
  240 CONTINUE
C
! !$omp parallel do 
      DO 250 J=MYJS1_P3,MYJE_P3
      DO 250 I=MYIS_P3,MYIE1_P3
      ADPDSE(I,J)=DPDE(I+IHE(J),J-1)+DPDE(I,J)
      PSE(I,J)=2.*(FIM(I+IHE(J),J-1)-FIM(I,J))
      PPSE(I,J)=PSE(I,J)*ADPDSE(I,J)
      CSE(I,J)=2.*(RTOP(I+IHE(J),J-1,L)+RTOP(I,J,L))
     1           *(APEL(I+IHE(J),J-1)-APEL(I,J))
      PCSE(I,J)=CSE(I,J)*ADPDSE(I,J)
  250 CONTINUE
C--------------LAT & LONG PRESSURE FORCE COMPONENTS---------------------
! !$omp parallel do private(dcnek,dcsek,dpnek,dpsek)
      DO 280 J=MYJS1_P2,MYJE1_P2
      DO 280 I=MYIS_P2,MYIE_P2
      DPNEK=PNE(I+IVW(J),J)+PNE(I,J-1)
      DPSEK=PSE(I+IVW(J),J)+PSE(I,J+1)
      PEW(I,J)=DPNEK+DPSEK
      PNS(I,J)=DPNEK-DPSEK
      DCNEK=CNE(I+IVW(J),J)+CNE(I,J-1)
      DCSEK=CSE(I+IVW(J),J)+CSE(I,J+1)
      PCEW(I,J)=(DCNEK+DCSEK)*ADPDX(I,J)
      PCNS(I,J)=(DCNEK-DCSEK)*ADPDY(I,J)
  280 CONTINUE
C--------------UPDATE U AND V (CORIOLIS & PGF)--------------------------
! !$omp parallel do private(dpfnek,dpfsek)
      DO 290 J=MYJS2_P2,MYJE2_P2
      DO 290 I=MYIS_P2,MYIE1_P2
      DPFNEK=((PPNE(I+IVW(J),J)+PPNE(I,J-1))
     1       +(PCNE(I+IVW(J),J)+PCNE(I,J-1)))*2.
      DPFSEK=((PPSE(I+IVW(J),J)+PPSE(I,J+1))
     1       +(PCSE(I+IVW(J),J)+PCSE(I,J+1)))*2.
      DPFEW(I,J)=DPFNEK+DPFSEK
      DPFNS(I,J)=DPFNEK-DPFSEK
  290 CONTINUE
C
! !$omp parallel do private(f0k,upk,utk,vpk,vtk)
      DO 300 J=MYJS2_P2,MYJE2_P2
      DO 300 I=MYIS_P2,MYIE1_P2
      F0K=U(I,J,L)*CURV(I,J)+F(I,J)
      VM(I,J)=VTM(I,J,L)*VBM2(I,J)
      UPK=((DPFEW(I,J)+PCEW(I,J))*RDPDX(I,J)
     1       +PEW(I,J))*CPGFU(I,J)+F0K*V(I,J,L)+U(I,J,L)
      VPK=((DPFNS(I,J)+PCNS(I,J))*RDPDY(I,J)
     1       +PNS(I,J))*CPGFV-F0K*U(I,J,L)+V(I,J,L)
      UTK=U(I,J,L)
      VTK=V(I,J,L)
      U(I,J,L)=((F0K*VPK+UPK)/(F0K*F0K+1.)-U(I,J,L))
     1         *VM(I,J)+U(I,J,L)
      V(I,J,L)=(VPK-F0K*U(I,J,L)-V(I,J,L))
     1         *VM(I,J)+V(I,J,L)
  300 CONTINUE
C-----------------------------------------------------------------------
  400                        CONTINUE                                   
C-----------------------------------------------------------------------
                             RETURN                                     
                             END                                        
