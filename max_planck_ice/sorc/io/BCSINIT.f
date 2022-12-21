      SUBROUTINE BCSINIT(VM, HM, OM, FLM)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                        AUG  87
C     Robert Grumbine       NMC, Camp Springs                   June 94
C  PURPOSE:
C     -READS DOMAIN MASKS
C     -IDENTIFIES OUTFLOW CELLS
C     -Modified to work with reading in variable grid sizes. 
C         BG 23 June 1994.
C  LAST MODIFIED: 23 June 1994
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/OUTFLOW/NOUT,IOUT(LDO), JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
      INTEGER K1(L,M), K2(0:L,0:M)
C=======================================================================
      INTEGER I, J
      CHARACTER*9 form, formp
C-----------------------------------------------------------------------
C  INITIALIZE THE MASKS
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
          WRITE (form, 9001) L
          WRITE (formp,9001) LP
        READ(10,form) ((VM(I,J), I=1,L), J=1,M)
        READ(10,formp) ((HM(I,J), I=0,L), J=0,M)
        READ(10,formp) ((OM(I,J), I=0,L), J=0,M)
CD        READ(10,801) ((VM(I,J), I=1,L), J=1,M)
CD        READ(10,802) ((HM(I,J), I=0,L), J=0,M)
CD        READ(10,802) ((OM(I,J), I=0,L), J=0,M)
       ELSE IF (PTYPE .EQ. 3) THEN
          WRITE (form, 9001) L
          WRITE (formp,9001) LP
          READ(10,form ) ((VM(I,J), I=1,L), J=1,M)
          READ(10,formp) ((HM(I,J), I=0,L), J=0,M)
          READ(10,formp) ((OM(I,J), I=0,L), J=0,M)
CD          WRITE(*,form) ((INT(VM(I,J)), I=1,L), J=1,M)
CD          WRITE(*,formp) ((INT(HM(I,J)), I=0,L), J=0,M)
        DO 1000 J = 0, M
        DO 1000 I = 0, L
            K2(I,J) = INT(HM(I,J))
 1000   CONTINUE
        DO 1100 J = 1, M
        DO 1100 I = 1, L
            K1(I,J) = INT(VM(I,J))
 1100   CONTINUE

       ELSE
        STOP 'PTYPE OUT OF RANGE IN BCSINIT'
      ENDIF
C-----------------------------------------------------------------------
C  DETEMINE NUMBER OF OUTFLOW CELLS
C-----------------------------------------------------------------------
      NOUT=0
      DO 10 J=0,M
      DO 10 I=0,L
       IF ((OM(I,J).EQ. 0.0).AND.(HM(I,J).NE.0.0)) THEN
        NOUT=NOUT+1
        IOUT(NOUT)=I
        JOUT(NOUT)=J
        IF (NOUT.GT.LDO) THEN
         PRINT 811,LDO
         STOP
        END IF
       END IF
   10 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE MASKS FOR FLUX BC'S
C-----------------------------------------------------------------------
      DO 20 J=1,M
      DO 20 I=1,L
       FLM(I,J,1)=0.5*(OM(I,J)+OM(I-1,J))*(1.-ABS(OM(I,J)-OM(I-1,J)))
       FLM(I,J,2)=0.5*(OM(I,J)+OM(I,J-1))*(1.-ABS(OM(I,J)-OM(I,J-1)))
   20 CONTINUE
      RETURN

CD  801 FORMAT (127G1.0)
CD  802 FORMAT (128G1.0)

 9001 FORMAT ('(',I3,'G1.0)')

  811 FORMAT ('NUMBER OF OUTFLOW GRID POINTS EXCEEDS THE DIMENSION ',I5)
      END
