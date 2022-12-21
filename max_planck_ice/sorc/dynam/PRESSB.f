      SUBROUTINE PRESSB(LOLD, P, H, A)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE PLAST IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     R. W. Grumbine           NMC, Camp Springs, MD                1992
C  PURPOSE:
C     -CALCULATION OF ICE STRENGTH (EQ.17 IN HIBLER (79))
C  METHOD:
C     -USES HIBLER (1979) EQUATION OF STATE
C  INTERFACE:
C     -LOLD: RUNNING INDEX FOR OLD TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      REAL  H(0:L,0:M,2), A(0:L,0:M,2)
      REAL  P(L,M)
      INTEGER LOLD, I, J
C=======================================================================
CD      DO 200 J=1,MM
CBG   Try filling while P grid

      IF (LOLD .GT. 2 .OR. LOLD .LT. 1) THEN
        PRINT *,'Lold in Pressb = ', LOLD
        PRINT *,'Cannot continue in routine, setting values to zero'
        DO 100 J = 1, M
        DO 100 I = 1, L
          P(I,J) = 0.0
  100   CONTINUE
        
       ELSE
        DO 200 J=1,M
        DO 200 I=1,L
          P(I,J)=PSTAR*H(I,J,LOLD)*EXP(-CSTAR*(1.0-A(I,J,LOLD)))
  200   CONTINUE
      ENDIF

      RETURN
      END
