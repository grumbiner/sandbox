      SUBROUTINE layers(dlevel, fname, deep, shal, unit)
C     Extract specified layers from the levitus field maps.
C     Specify a set of layers (all at once to avoid reading in
C      the 8.5 Mb file multiple times) and write them out as
C      global arrays.
C     Robert Grumbine 1 August 1995

      IMPLICIT none

      INTEGER imax, jmax, kmax
      PARAMETER (imax = 360)
      PARAMETER (jmax = 180)
      PARAMETER (kmax =  33)
C     User is presumed to know what depth corresponds to which k.

CD      REAL giant(imax, jmax, kmax)
      REAL xsec(jmax, kmax), deep(imax, jmax), shal(imax, jmax)
      INTEGER merid

      INTEGER i, j, k
      INTEGER unit, dlevel, kout(2)
      CHARACTER*60 fname

C===============================================================
      OPEN (unit, FILE=fname,
     1          FORM='UNFORMATTED', STATUS='OLD')

      kout(1) = 1
      kout(2) = dlevel

C     Read in meridional sections:
      DO 2000 i = 1, imax
        READ (unit) merid, xsec
        DO 2100 j = 1, jmax
          DO 2200 k = 1, kmax
CD            giant(i,j,k) = xsec(j,k)
            shal(i,j) = xsec(j,kout(1))
            deep(i,j) = xsec(j,kout(2))
 2200     CONTINUE
 2100   CONTINUE
 2000 CONTINUE

C     Write out levels to separate files:
CD        DO 3100 i = 1, imax
CD          DO 3200 j = 1, jmax
CD            shal(i,j) = giant(i,j,kout(1))
CD            deep(i,j) = giant(i,j,kout(2))
CD 3200     CONTINUE
CD 3100   CONTINUE
    
      CLOSE (unit)
      RETURN
      END 
