      PROGRAM bathy
C     Create bathymetry for the ice model
C     Work from a regular lat-long grid as constructed by Hendrik Tolman's
C       etopo5 management program. 
C     Robert Grumbine 13 July 1995
C     Last Modified 21 December 1995

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL bathym(0:L,0:M)
      REAL global(360, 180) !Note that this is a half-off specification for
C                            the regular grid.  Also that we're hard-coding
C                            the resolution

      INTEGER pole, tlat, tlong, terr
      INTEGER i, j
      CHARACTER*60 fname
      CHARACTER*12 form, formp

C     Select the data type
      OPEN(10, FILE='bathy.bin', FORM='UNFORMATTED', STATUS='OLD')
      OPEN(12, FILE='bathy.out', FORM='UNFORMATTED', STATUS='NEW')

      READ (10) global
CD      PRINT *,'read in the global bathymetry'
      DO 1000 j = 0, M
        DO 1100 i = 0, L
          bathym(i,j) = 0.0
 1100   CONTINUE
 1000 CONTINUE 

      IF (LATMIN .GT. 0) THEN
        pole = 1
       ELSE 
        pole = 2
      ENDIF 

C     Interpolate
CD      PRINT *,'About to call terph'
      CALL terph(global, bathym, pole, .FALSE.)
CD      PRINT *,'back from terph'
      DO 2000 j = 0, M
        DO 2100 i = 0, L
          bathym(i,j) = AMAX1(1., bathym(i,j) )
 2100   CONTINUE
 2000 CONTINUE

      WRITE (12) bathym
 9001 FORMAT (11F7.1)
CD      WRITE (*,9001) ((bathym(i,j),i=0,L),j=0,M)

      CLOSE (10, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')

      STOP
      END
