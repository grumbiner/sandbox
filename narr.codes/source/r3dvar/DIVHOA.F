                             SUBROUTINE DIVHOA(div0_eta)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    DIVHOA      DIVERGENCE/HORIZONTAL OMEGA-ALPHA
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-10-28       
C     
C ABSTRACT:
C     DIVHOA COMPUTES THE DIVERGENCE INCLUDING THE
C     MODIFICATION PREVENTING GRAVITY WAVE GRID SEPARATION, AND
C     CALCULATES THE HORIZONTAL PART OF THE OMEGA-ALPHA TERM
C     (THE PART PROPORTIONAL TO THE ADVECTION OF MASS ALONG
C     ETA SURFACES).
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-29  BLACK      - ADDED EXTERNAL EDGE
C   97-03-17  MESINGER   - SPLIT FROM PFDHT
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL DIVHOA FROM MAIN PROGRAM EBU
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
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
                             P A R A M E T E R                          
     & (CP=1004.6)                                                    
C-----------------------------------------------------------------------
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

      real(4) div0_eta(idim1:idim2,jdim1:jdim2,lm)

                             D I M E N S I O N                          
     & FILO  (IDIM1:IDIM2,JDIM1:JDIM2),RDPD  (IDIM1:IDIM2,JDIM1:JDIM2)  
     &,ADPDX (IDIM1:IDIM2,JDIM1:JDIM2),ADPDY (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FIUP  (IDIM1:IDIM2,JDIM1:JDIM2),F0    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADPDNE(IDIM1:IDIM2,JDIM1:JDIM2),ADPDSE(IDIM1:IDIM2,JDIM1:JDIM2)
     &,PEW   (IDIM1:IDIM2,JDIM1:JDIM2),PNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PCEW  (IDIM1:IDIM2,JDIM1:JDIM2),PCNS  (IDIM1:IDIM2,JDIM1:JDIM2)  
     &,DPFEW (IDIM1:IDIM2,JDIM1:JDIM2),DPFNS (IDIM1:IDIM2,JDIM1:JDIM2)                                  
     &,FNS   (IDIM1:IDIM2,JDIM1:JDIM2),TNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DPNE  (IDIM1:IDIM2,JDIM1:JDIM2),DPSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DCNE  (IDIM1:IDIM2,JDIM1:JDIM2),DCSE  (IDIM1:IDIM2,JDIM1:JDIM2)  
     &,DPFNE (IDIM1:IDIM2,JDIM1:JDIM2),DPFSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UP    (IDIM1:IDIM2,JDIM1:JDIM2),VP    (IDIM1:IDIM2,JDIM1:JDIM2)                  
     &,PVNE  (IDIM1:IDIM2,JDIM1:JDIM2),PVSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,VM    (IDIM1:IDIM2,JDIM1:JDIM2),HM    (IDIM1:IDIM2,JDIM1:JDIM2)                  
C
                             D I M E N S I O N                          
     & DPDE  (IDIM1:IDIM2,JDIM1:JDIM2),FIM   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,APEL  (IDIM1:IDIM2,JDIM1:JDIM2),PCXC  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UDY   (IDIM1:IDIM2,JDIM1:JDIM2),VDX   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TEW   (IDIM1:IDIM2,JDIM1:JDIM2),FEW   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TNE   (IDIM1:IDIM2,JDIM1:JDIM2),TSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNE   (IDIM1:IDIM2,JDIM1:JDIM2),FSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PNE   (IDIM1:IDIM2,JDIM1:JDIM2),PSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,CNE   (IDIM1:IDIM2,JDIM1:JDIM2),CSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PPNE  (IDIM1:IDIM2,JDIM1:JDIM2),PPSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PCNE  (IDIM1:IDIM2,JDIM1:JDIM2),PCSE  (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL ZERO2(DPDE)       
      CALL ZERO2(PNE)        
      CALL ZERO2(CNE)        
      CALL ZERO2(PSE)        
      CALL ZERO2(CSE)        
      CALL ZERO2(ADPDX)      
      CALL ZERO2(ADPDY)      
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
      DO 110 L=1,LM
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        DIV(I,J,L)=0.
        OMGALF(I,J,L)=0.
      ENDDO
      ENDDO
  110 CONTINUE
C
! !$omp parallel do 
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
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
      RDPD(I,J)=1./DPDE(I,J)
  210 CONTINUE
C
! !$omp parallel do 
      DO 220 J=MYJS1_P2,MYJE1_P2
      DO 220 I=MYIS_P2,MYIE1_P2
      ADPDX(I,J)=DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J)
      ADPDY(I,J)=DPDE(I,J-1)+DPDE(I,J+1)
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
      DO 240 J=MYJS_P3,MYJE1_P3
      DO 240 I=MYIS_P3,MYIE1_P3
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
C--------------CONTINUITY EQUATION MODIFICATION-------------------------
! !$omp parallel do 
      DO 260 J=MYJS1_P1,MYJE1_P1
      DO 260 I=MYIS_P1,MYIE_P1
      PCXC(I,J)=VBM3(I,J)*VTM(I,J,L)*(PNE(I+IVW(J),J)
     1         +CNE(I+IVW(J),J)+PSE(I+IVW(J),J)+CSE(I+IVW(J),J)
     2         -PNE(I,J-1)-CNE(I,J-1)-PSE(I,J+1)-CSE(I,J+1))
  260 CONTINUE
C-----------------------------------------------------------------------
! !$omp parallel do 
      DO 270 J=MYJS2,MYJE2
      DO 270 I=MYIS1,MYIE1
      DIV(I,J,L)=DETA(L)*WPDAR(I,J)
     1         *(PCXC(I+IHE(J),J)-PCXC(I,J+1)
     2          +PCXC(I+IHW(J),J)-PCXC(I,J-1))
  270 CONTINUE
C--------------LAT & LONG PRESSURE FORCE COMPONENTS---------------------
! !$omp parallel do 
      DO 280 J=MYJS1_P2,MYJE1_P2
      DO 280 I=MYIS_P2,MYIE_P2
      DCNEK=CNE(I+IVW(J),J)+CNE(I,J-1)
      DCSEK=CSE(I+IVW(J),J)+CSE(I,J+1)
      PCEW(I,J)=(DCNEK+DCSEK)*ADPDX(I,J)
      PCNS(I,J)=(DCNEK-DCSEK)*ADPDY(I,J)
  280 CONTINUE
C--------------LAT & LON FLUXES & OMEGA-ALPHA COMPONENTS----------------
! !$omp parallel do 
      DO 310 J=MYJS1_P2,MYJE1_P2
      DO 310 I=MYIS_P2,MYIE_P2
      UDY(I,J)=DY*U(I,J,L)
      FEW(I,J)=UDY(I,J)*ADPDX(I,J)
      TEW(I,J)=UDY(I,J)*PCEW(I,J)
      VDX(I,J)=DX(I,J)*V(I,J,L)
      FNS(I,J)=VDX(I,J)*ADPDY(I,J)
      TNS(I,J)=VDX(I,J)*PCNS(I,J)
  310 CONTINUE
C--------------DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND-------------
! !$omp parallel do 
      DO 320 J=MYJS1_P1,MYJE2_P1
      DO 320 I=MYIS_P1,MYIE1_P1
      PVNEK=(UDY(I+IHE(J),J)+VDX(I+IHE(J),J))+(UDY(I,J+1)+VDX(I,J+1))
      FNE(I,J)=PVNEK*ADPDNE(I,J)
      TNE(I,J)=PVNEK*PCNE(I,J)*2.
  320 CONTINUE
C
! !$omp parallel do 
      DO 330 J=MYJS2_P1,MYJE1_P1
      DO 330 I=MYIS_P1,MYIE1_P1
      PVSEK=(UDY(I+IHE(J),J)-VDX(I+IHE(J),J))+(UDY(I,J-1)-VDX(I,J-1))
      FSE(I,J)=PVSEK*ADPDSE(I,J)
      TSE(I,J)=PVSEK*PCSE(I,J)*2.
  330 CONTINUE
C--------------HORIZONTAL PART OF OMEGA-ALPHA & DIVERGENCE--------------
! !$omp parallel do 
      DO 340 J=MYJS2,MYJE2
      DO 340 I=MYIS1,MYIE1
      HM(I,J)=HTM(I,J,L)*HBM2(I,J)
      TTK=T(I,J,L)
      OMGALF(I,J,L)=(TEW(I+IHE(J),J)+TEW(I+IHW(J),J)+TNS(I,J+1)
     1              +TNS(I,J-1)+TNE(I,J)+TNE(I+IHW(J),J-1)+TSE(I,J)
     2              +TSE(I+IHW(J),J+1))*RDPD(I,J)*FCP(I,J)*HM(I,J)
      T(I,J,L)=OMGALF(I,J,L)+T(I,J,L)
      DIV(I,J,L)=(((FEW(I+IHE(J),J)+FNS(I,J+1)+FNE(I,J)+FSE(I,J))
     1    -(FEW(I+IHW(J),J)+FNS(I,J-1)+FNE(I+IHW(J),J-1)
     2     +FSE(I+IHW(J),J+1)))*FDIV(I,J)+DIV(I,J,L))*HM(I,J)
  340 CONTINUE
C-----------------------------------------------------------------------
  400                        CONTINUE                                   
C-----------------------------------------------------------------------
      do l=1,lm
       do j=myjs,myje
        do i=myis,myie
         div0_eta(i,j,l)=div(i,j,l)
        end do
       end do
      end do

                             RETURN                                     
                             END                                        
