      PROGRAM mapim
C     Create a raster plot image from the model's output.
C     Bob Grumbine 16 August 1994.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL conc(LP,MP)
      CHARACTER*1 cout(LP,MP)
      INTEGER mask(LP,MP)

      REAL xmin, xmax
      INTEGER i, j, k, nframe
      CHARACTER*5 formp
      CHARACTER*60 fname

      PRINT *,'What is the name of the land mask file?'
      READ (*,9004) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the model output file?'
      READ (*,9004) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
 

      PRINT *,'How many frames are there?'
      READ (*, 9003) nframe

 9001 FORMAT (E13.6)
 9002 FORMAT ('(',I3,'G1.0)')
 9003 FORMAT (I3)
 9004 FORMAT (A60)
 9009 FORMAT (77I1)

      WRITE (formp,9002) LP
      DO 3000 j = 1, MP
CD        READ (12, formp) (mask(i,j),i=1,LP)
        READ (12, 9009) (mask(i,j),i=1,LP)
 3000 CONTINUE

      DO 2000 k = 1, nframe
        PRINT *,'What name do you want for the rasterized output?'
        READ (*,9004) fname
        OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
        READ (10) conc
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

        WRITE (11) ((cout(i,j), i=1, LP),j=1,MP)

        CLOSE (11) 
 2000 CONTINUE

      STOP
      END
