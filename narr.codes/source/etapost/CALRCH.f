      SUBROUTINE CALRCH(EL,RICHNO)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALRCH      COMPUTES GRD RCH NUMBER
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-10-11
C     
C ABSTRACT:  
C   THIS ROUTINE COMPUTES THE GRADIENT RICHARDSON NUMBER
C   AS CODED IN ETA MODEL SUBROUTINE PROFQ2.F.
C   FIX TO AVOID UNREASONABLY SMALL ANEMOMETER LEVEL WINDS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-10-11  RUSS TREADON
C   98-06-17  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL CALRCH(EL,RICHNO)
C   INPUT ARGUMENT LIST:
C     EL      - MIXING LENGTH SCALE.
C
C   OUTPUT ARGUMENT LIST: 
C     RICHNO  - GRADIENT RICHARDSON NUMBER.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - EXTRA
C                  LOOPS
C                  MASKS
C                  PHYS
C                  VRBLS
C                  PVRBLS
C                  OPTIONS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     INCLUDE,DERIVE,SET PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "params"
      INCLUDE "parm.tbl"
C     
C     DECLARE VARIABLES.
C     
      REAL EGRID1(IM,JM),EGRID2(IM,JM),HGT(IM,JM)
      REAL EL(IM,JM,LM),RICHNO(IM,JM,LM),THV(IM,JM,LM)
C     
C     INCLUDE COMMON BLOCKS.
C     
      INCLUDE "EXTRA.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C
C     
C*************************************************************************
C     START CALRCH HERE.
C     
C     INITIALIZE ARRAYS.
C     
!$omp  parallel do
      DO L = 1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          RICHNO(I,J,L)=SPVAL
        ENDDO
        ENDDO
      ENDDO
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        HGT(I,J)=RG*FIS(I,J)
      ENDDO
      ENDDO
C     
C     COMPUTE AND SMOOTH VIRTUAL POTENTIAL TEMPERATURE.
C
!$omp  parallel do
!$omp& private(ape,egrid1,egrid2,ie,iw)
      DO L=LM,1,-1
        DO J=JSTA,JEND
        DO I=1,IM
          APE=(H10E5/(PDSL(I,J)*AETA(L)+PT))**CAPA
          THV(I,J,L)=(Q(I,J,L)*D608+H1)*T(I,J,L)*APE
        ENDDO
        ENDDO
C     
        CALL EXCH(THV(1,1,L))
C
C        SMOOTH VIRTUAL POTENTIAL TEMPERATURE.
C     
        DO J=JSTA,JEND_M
        DO I=1,IM-1
          IE=I+IHE(J)
          EGRID1(I,J)=(THV(IE,J+1,L)-THV(I,J,L))
     1                *HTM(I,J,L)*HTM(IE,J+1,L)
        ENDDO
        ENDDO
C
        DO J=JSTA_M,JEND
        DO I=1,IM-1
          IE=I+IHE(J)
          EGRID2(I,J)=(THV(IE,J-1,L)-THV(I,J,L))
     1                *HTM(IE,J-1,L)*HTM(I,J,L)
        ENDDO
        ENDDO
C
        CALL EXCH(EGRID1)
        CALL EXCH(EGRID2)
C
        DO J=JSTA_M2,JEND_M2
        DO I=2,IM-1
          IW=I+IHW(J)
          THV(I,J,L)=THV(I,J,L) + D125 * 
     1              (EGRID1(I,J)-EGRID1(IW,J-1)+
     2               EGRID2(I,J)-EGRID2(IW,J+1))*HBM2(I,J)
        ENDDO
        ENDDO
C
      ENDDO
C
C     COMPUTE GRADIENT RICHARDSON NUMBER AS CODED IN ETA MODEL
C     SUBROUTINE PROFQ2.F.  OUTER LOOP OVER THE VERTICAL. 
C     INTTER LOOP OVER THE HORIZONTAL.
C
!$omp  parallel do
!$omp& private(cs,ct,dthvkl,dukl,dvkl,dzkl,elkl,elklsq,
!$omp&         ie,iw,q2kl,qroot,rdzkl,ri,uhkl,ulkl,vhkl,vlkl,
!$omp&         wndsl,wndslp)
      DO L = 1,LM1
C
C     ASSUME THAT VTM, U, and V HAVE UPDATED HALOS
C
        DO J=JSTA_M2,JEND_M2
        DO I=2,IM-1
          IE=I+IHE(J)
          IW=I+IHW(J)
C     
C         WE NEED (U,V) WINDS AT A MASS POINT.  FOUR POINT
C         AVERAGE (U,V) WINDS TO MASS POINT.  NORMALIZE FOUR
C         POINT AVERAGE BY THE ACTUAL NUMBER OF (U,V) WINDS
C         USED IN THE AVERAGING.  VTM=1 IF WIND POINT IS 
C         ABOVE GROUND.  VTM=0 IF BELOW GROUND.
C
          WNDSL=VTM(I,J-1,L)+VTM(IW,J,L)+VTM(IE,J,L)+VTM(I,J+1,L)
          WNDSLP=VTM(I,J-1,L+1)+VTM(IW,J,L+1)+
     1           VTM(IE,J,L+1)+VTM(I,J+1,L+1)
          IF(WNDSL.EQ.0..OR.WNDSLP.EQ.0.)GO TO 10
          UHKL=(U(I,J-1,L)+U(IW,J,L)+U(IE,J,L)+U(I,J+1,L))/WNDSL
          ULKL=(U(I,J-1,L+1)+U(IW,J,L+1)+U(IE,J,L+1)+
     1          U(I,J+1,L+1))/WNDSLP
          VHKL=(V(I,J-1,L)+V(IW,J,L)+V(IE,J,L)+V(I,J+1,L))/WNDSL
          VLKL=(V(I,J-1,L+1)+V(IW,J,L+1)+V(IE,J,L+1)+
     1           V(I,J+1,L+1))/WNDSLP
          DZKL=D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
          RDZKL=1./DZKL
          Q2KL=AMAX1(Q2(I,J,L),0.00001)
          QROOT=SQRT(Q2KL)
          ELKL=EL(I,J,L)
          ELKL=AMAX1(ELKL,EPSQ2)
          ELKLSQ=ELKL*ELKL
          DTHVKL=THV(I,J,L)-THV(I,J,L+1)
          DUKL=(UHKL-ULKL)
          DVKL=(VHKL-VLKL)
          CS=(DUKL*RDZKL)**2+(DVKL*RDZKL)**2
C     
C         COMPUTE GRADIENT RICHARDSON NUMBER.
C     
          IF(CS.LE.1.E-8)THEN
C
C         WIND SHEAR IS VANISHINGLY SO SET RICHARDSON
C         NUMBER TO POST PROCESSOR SPECIAL VALUE.
C
            RICHNO(I,J,L)=SPVAL
C
          ELSE
C
C         WIND SHEAR LARGE ENOUGH TO USE RICHARDSON NUMBER.
C
            CT=-1.*G*BETA*DTHVKL*RDZKL
            RI=-CT/CS
            RICHNO(I,J,L)=RI
          ENDIF
C
 10       CONTINUE
        ENDDO
        ENDDO
      ENDDO
C     
C     END OF ROUTINE.
C     
      RETURN
      END

