      PROGRAM mapim
C     Create a raster plot image from the ice concentration map.
C     Or create a real array from a raster map.
C     Robert Grumbine 16 June 1994.
C     Robert Grumbine 17 October 1994.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL conc(LP,MP)
      CHARACTER*1 cout(LP,MP)
      INTEGER mask(LP,MP)
      INTEGER conver

      PRINT *,'Are you converting from a real grid to a character (1),'
      PRINT *,' or from a character grid to a real (2)?'
      READ (*,9001) conver
 9001 FORMAT (I1)

      IF (conver .EQ. 1) THEN
        CALL rtochar(cout, conc, mask, LP, MP)
       ELSE IF (conver .EQ. 2) THEN
        CALL chartor(cout, conc, mask, LP, MP)
       ELSE
        PRINT *,'not a valid conversion'
      ENDIF

      STOP
      END
 
      SUBROUTINE rtochar(cout, conc, mask, lp, mp)

      INTEGER lp, mp
      REAL conc(LP,MP)
      CHARACTER*1 cout(LP,MP)
      INTEGER mask(LP,MP)

      REAL xmin, xmax
      INTEGER i, j, k, nframe
      CHARACTER*5 formp

      PRINT *,'How many frames are there?'
      READ (*, 9003) nframe

 9001 FORMAT (E13.6)
 9002 FORMAT ('(',I3,'I1)')
 9003 FORMAT (I3)
      WRITE (formp,9002) LP
        DO 3000 j = 1, MP
          READ (12, formp) (mask(i,j),i=1,LP)
 3000   CONTINUE

      DO 2000 k = 1, nframe
        READ(10) conc
 1000   PRINT *,'What is the minimum value?'
        READ (*,9001) xmin
        PRINT *,'What is the maximum value?'
        READ (*,9001) xmax
        IF (xmax .LT. xmin) GO TO 1000
        CALL imprep(conc, cout, LP, MP, xmin, xmax)

        DO 2100 j = 1, MP
          DO 2110 i = 1, LP
            IF (mask(i,j) .EQ. 0) THEN
              cout(i,j) = CHAR(157)
          ENDIF
 2110     CONTINUE
 2100   CONTINUE

        WRITE (11) cout
        CLOSE (11) 
 2000 CONTINUE

      RETURN
      END
      SUBROUTINE chartor(cout, conc, mask, lp, mp)

      REAL conc(LP,MP)
      CHARACTER*1 cout(LP,MP)
      INTEGER mask(LP,MP)

      REAL xmin, xmax
      INTEGER i, j, k, nframe
      CHARACTER*5 formp
      INTEGER conver

      PRINT *,'How many frames are there?'
      READ (*, 9003) nframe
 9003 FORMAT (I3)

      DO 2000 k = 1, nframe
        READ(10) cout
C 1000   PRINT *,'What is the minimum value?'
C        READ (*,9001) xmin
C        PRINT *,'What is the maximum value?'
C        READ (*,9001) xmax
C        IF (xmax .LT. xmin) GO TO 1000

        DO 2100 j = 1, MP
          DO 2110 i = 1, LP
            conc(i,j) = FLOAT( ICHAR(cout(i,j)) )
 2110     CONTINUE
 2100   CONTINUE

        WRITE (11) conc
        CLOSE (11) 
 2000 CONTINUE

      RETURN
      END
