      SUBROUTINE CALHEL(UST,VST,HELI)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALHEL       COMPUTES STORM RELATIVE HELICITY
C   PRGRMMR: BALDWIN         ORG: W/NP2      DATE: 94-08-22       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES ESTIMATED STORM MOTION AND
C     STORM-RELATIVE ENVIRONMENTAL HELICITY.  
C     (DAVIES-JONES ET AL 1990) THE ALGORITHM PROCEEDS AS 
C     FOLLOWS.
C     
C     THE STORM MOTION COMPUTATION NO LONGER EMPLOYS THE DAVIES AND
C   JOHNS (1993) METHOD WHICH DEFINED STORM MOTION AS 30 DEGREES TO
C   THE RIGHT OF THE 0-6 KM MEAN WIND AT 75% OF THE SPEED FOR MEAN
C   SPEEDS LESS THAN 15 M/S AND 20 DEGREES TO THE RIGHT FOR SPEEDS
C   GREATER THAN 15 M/S.   INSTEAD, WE NOW USE THE DYNAMIC METHOD
C   (BUNKERS ET AL. 1998) WHICH HAS BEEN FOUND TO DO BETTER IN
C   CASES WITH 'NON-CLASSIC' HODOGRAPHS (SUCH AS NORTHWEST-FLOW
C   EVENTS) AND DO AS WELL OR BETTER THAN THE OLD METHOD IN MORE
C   CLASSIC SITUATIONS. 
C     
C PROGRAM HISTORY LOG:
C   94-08-22  MICHAEL BALDWIN
C   97-03-27  MICHAEL BALDWIN - SPEED UP CODE
C   98-06-15  T BLACK         - CONVERSION FROM 1-D TO 2-D
C   00-01-10  G MANIKIN - CHANGED CODE TO USE BUNKERS METHOD
C                           AS DESCRIBED ABOVE 
C     
C USAGE:    CALHEL(UST,VST,HELI)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     UST      - ESTIMATED U COMPONENT (M/S) OF STORM MOTION.
C     VST      - ESTIMATED V COMPONENT (M/S) OF STORM MOTION.
C     HELI     - STORM-RELATIVE HELICITY (M**2/S**2)
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C
C     LIBRARY:
C       COMMON   - VRBLS
C                  LOOPS
C                  PHYS 
C                  EXTRA
C                  MASKS
C                  OPTIONS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE PARAMETERS.
      INCLUDE "parmeta"
      INCLUDE "params"
      INCLUDE "parm.tbl"
      PARAMETER (PI=3.141592654)
      PARAMETER (P150=15000.0,P300=30000.0,S15=15.0)
      PARAMETER (D3000=3000.0,PI6=0.5235987756,PI9=0.34906585)
      PARAMETER (D5500=5500.0,D6000=6000.0,D7000=7000.0)
      PARAMETER (D500=500.0)
C     
C     DECLARE VARIABLES
C     
      REAL UST(IM,JM),VST(IM,JM),HELI(IM,JM),HTSFC(IM,JM)
      REAL UST6(IM,JM),VST6(IM,JM),ETOT6(IM,JM)
      REAL UST5(IM,JM),VST5(IM,JM),ETOT5(IM,JM)
      REAL UST1(IM,JM),VST1(IM,JM),ETOT1(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "VRBLS.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C     
C****************************************************************
C     START CALHEL HERE
C     
C     INITIALIZE ARRAYS.
C     
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
         UST(I,J)    = 0.0
         VST(I,J)    = 0.0
         HELI(I,J)   = 0.0
         UST1(I,J)   = 0.0
         VST1(I,J)   = 0.0
         ETOT1(I,J)  = 0.0
         UST5(I,J)   = 0.0
         VST5(I,J)   = 0.0
         ETOT5(I,J)  = 0.0
         UST6(I,J)   = 0.0
         VST6(I,J)   = 0.0
         ETOT6(I,J)  = 0.0

      ENDDO
      ENDDO
C
C     LOOP OVER HORIZONTAL GRID.
C
      CALL EXCH(RES)
      CALL EXCH(PD)
      DO L = 1,LP1
        CALL EXCH(ZINT(1,1,L))
      END DO
C 
!$omp  parallel do
!$omp& private(htsfc,ie,iw,pdslvk,pkl,psfck)
      DO L = 1,LM
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          IE=I+IVE(J)
          IW=I+IVW(J)
          PDSLVK=(PD(IW,J)*RES(IW,J)+PD(IE,J)*RES(IE,J)+
     1           PD(I,J+1)*RES(I,J+1)+PD(I,J-1)*RES(I,J-1))*0.25
          PSFCK=AETA(LMV(I,J))*PDSLVK+PT
          HTSFC(I,J)=0.25*(ZINT(IW,J,LMV(I,J)+1)+ZINT(IE,J,LMV(I,J)+1)+
     1                ZINT(I,J+1,LMV(I,J)+1)+ZINT(I,J-1,LMV(I,J)+1))
C     
C     COMPUTE MASS WEIGHTED MEAN WIND IN THE 0-6 KM LAYER, THE
C  0-0.5 KM LAYER, AND THE 5.5-6 KM LAYER 
C
          Z2=0.125*(ZINT(IW,J,L)+ZINT(IW,J,L+1)+
     1            ZINT(IE,J,L)+ZINT(IE,J,L+1)+
     1            ZINT(I,J+1,L)+ZINT(I,J+1,L+1)+
     1            ZINT(I,J-1,L)+ZINT(I,J-1,L+1))
          DZABV=Z2-HTSFC(I,J)
  
          IF (DZABV.LE.D6000 .AND. L.LE.LMV(I,J)) THEN
               UST6(I,J) = UST6(I,J) + U(I,J,L) * DETA(L)
               VST6(I,J) = VST6(I,J) + V(I,J,L) * DETA(L)
               ETOT6(I,J) = ETOT6(I,J) + DETA(L)
          ENDIF

          IF (DZABV.LT.D6000 .AND. DZABV.GE.D5500 .AND.
     &       L.LE.LMV(I,J)) THEN
               UST5(I,J) = UST5(I,J) + U(I,J,L) * DETA(L)
               VST5(I,J) = VST5(I,J) + V(I,J,L) * DETA(L)
               ETOT5(I,J) = ETOT5(I,J) + DETA(L)
          ENDIF         

          IF (DZABV.LT.D500 .AND. L.LE.LMV(I,J)) THEN
               UST1(I,J) = UST1(I,J) + U(I,J,L) * DETA(L)
               VST1(I,J) = VST1(I,J) + V(I,J,L) * DETA(L)
               ETOT1(I,J) = ETOT1(I,J) + DETA(L)
          ENDIF

        ENDDO
        ENDDO
      ENDDO

C CASE WHERE THERE IS NO LEVEL WITH HEIGHT BETWEEN 5500 AND 6000
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        IF (ETOT5(I,J) .EQ. 0) THEN
         DO L=LM,1,-1
          IE=I+IVE(J)
          IW=I+IVW(J)
          Z2=0.125*(ZINT(IW,J,L)+ZINT(IW,J,L+1)+
     1            ZINT(IE,J,L)+ZINT(IE,J,L+1)+
     1            ZINT(I,J+1,L)+ZINT(I,J+1,L+1)+
     1            ZINT(I,J-1,L)+ZINT(I,J-1,L+1))
          DZABV=Z2-HTSFC(I,J)
          IF (DZABV.LT.D7000 .AND. DZABV.GE.D6000) THEN 
               UST5(I,J) = U(I,J,L) * DETA(L)
               VST5(I,J) = V(I,J,L) * DETA(L)
               ETOT5(I,J) = DETA(L)
               GOTO 30
          ENDIF
         ENDDO
        ENDIF
 30   CONTINUE
      ENDDO
      ENDDO


!$omp  parallel do
!$omp& private(umean6,vmean6,umean5,vmean5,umean1,vmean1,ushr,vshr)
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
         IF (ETOT6(I,J).GT.0.0 .AND. ETOT1(I,J) .GT. 0.0
     1      .AND. ETOT5(I,J) .GT. 0.0) THEN
           UMEAN6 = UST6(I,J) / ETOT6(I,J)
           VMEAN6 = VST6(I,J) / ETOT6(I,J)
           UMEAN5 = UST5(I,J) / ETOT5(I,J)
           VMEAN5 = VST5(I,J) / ETOT5(I,J)
           UMEAN1 = UST1(I,J) / ETOT1(I,J)
           VMEAN1 = VST1(I,J) / ETOT1(I,J)

C
C      COMPUTE STORM MOTION VECTOR
C      IT IS DEFINED AS 7.5 M/S TO THE RIGHT OF THE 0-6 KM MEAN
C      WIND CONSTRAINED ALONG A LINE WHICH IS BOTH PERPENDICULAR
C      TO THE 0-6 KM MEAN VERTICAL WIND SHEAR VECTOR AND PASSES
C      THROUGH THE 0-6 KM MEAN WIND.  THE WIND SHEAR VECTOR IS
C      SET AS THE DIFFERENCE BETWEEN THE 5.5-6 KM WIND (THE HEAD
C      OF THE SHEAR VECTOR) AND THE 0-0.5 KM WIND (THE TAIL).
C      THIS IS FOR THE RIGHT-MOVING CASE;  WE IGNORE THE LEFT MOVER.

           USHR = UMEAN5 - UMEAN1
           VSHR = VMEAN5 - VMEAN1

           UST(I,J) = UMEAN6 + (7.5*VSHR/SQRT(USHR*USHR+VSHR*VSHR))
           VST(I,J) = VMEAN6 - (7.5*USHR/SQRT(USHR*USHR+VSHR*VSHR))
         ELSE
           UST(I,J) = 0.0
           VST(I,J) = 0.0
        ENDIF
      ENDDO
      ENDDO
C
C
C       COMPUTE STORM-RELATIVE HELICITY
C
!$omp  parallel do
!$omp& private(du1,du2,dv1,dv2,dz,dz1,dz2,dzabv,ie,iw,z1,z2,z3)
      DO L = 2,LM-1
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          IW=I+IVW(J)
          IE=I+IVE(J)
          Z2=0.125*(ZINT(IW,J,L)+ZINT(IW,J,L+1)+
     &              ZINT(IE,J,L)+ZINT(IE,J,L+1)+
     &              ZINT(I,J+1,L)+ZINT(I,J+1,L+1)+
     &              ZINT(I,J-1,L)+ZINT(I,J-1,L+1))
          DZABV=Z2-HTSFC(I,J)
C
          IF(DZABV.LT.D3000.AND.L.LE.LMV(I,J))THEN
            Z1=0.125*(ZINT(IW,J,L+1)+ZINT(IW,J,L+2)+
     &                ZINT(IE,J,L+1)+ZINT(IE,J,L+2)+
     &                ZINT(I,J+1,L+1)+ZINT(I,J+1,L+2)+
     &                ZINT(I,J-1,L+1)+ZINT(I,J-1,L+2))
            Z3=0.125*(ZINT(IW,J,L-1)+ZINT(IW,J,L)+
     &                ZINT(IE,J,L-1)+ZINT(IE,J,L)+
     &                ZINT(I,J+1,L-1)+ZINT(I,J+1,L)+
     &                ZINT(I,J-1,L-1)+ZINT(I,J-1,L))
            DZ=0.25*((ZINT(IW,J,L)+ZINT(IE,J,L)+
     &                ZINT(I,J-1,L)+ZINT(I,J+1,L))-
     &               (ZINT(IW,J,L+1)+ZINT(IE,J,L+1)+
     &                ZINT(I,J-1,L+1)+ZINT(I,J+1,L+1)))
            DZ1=Z1-Z2
            DZ2=Z2-Z3
            DU1=U(I,J,L+1)-U(I,J,L)
            DU2=U(I,J,L)-U(I,J,L-1)
            DV1=V(I,J,L+1)-V(I,J,L)
            DV2=V(I,J,L)-V(I,J,L-1)
            HELI(I,J)=((V(I,J,L)-VST(I,J))*
     1                (DZ2*(DU1/DZ1)+DZ1*(DU2/DZ2))
     2                -(U(I,J,L)-UST(I,J))*
     3                (DZ2*(DV1/DZ1)+DZ1*(DV2/DZ2)))
     4                *DZ/(DZ1+DZ2)+HELI(I,J) 
           ENDIF
        ENDDO
        ENDDO
      ENDDO
C
C     END OF ROUTINE.
C
      RETURN
      END
