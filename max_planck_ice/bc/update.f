      PROGRAM update
C     Read in the update output (from the restart program of ic) and 
C       count the number of times any given point was updated and for 
C       which reason.
C     Northern Hemisphere 127 km version
C     Robert Grumbine
C     Last Modified 9 November 1995

      INTEGER nx, ny
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)
      REAL tml, tref, sml, sref
      REAL bathy(nx, ny)
      INTEGER tflag(nx, ny), sflag(nx, ny)
      INTEGER i, j
      INTEGER cbase

      OPEN (10, FILE='bathy.north', STATUS='OLD', FORM='UNFORMATTED')
      READ (10) bathy
      tflag = 0
      sflag = 0
      count = 0
 1000 CONTINUE
        READ (*,*,END = 2000) i, j, tml, tref, sml, sref
        count = count + 1
        IF (ABS(tml - tref) .GT. 10.0) tflag(i,j) = tflag(i,j) + 1
        IF (ABS(sml - sref) .GT.  5.0) sflag(i,j) = sflag(i,j) + 1
        GO TO 1000

 2000 CONTINUE

      cbase = ICHAR('a')
      PRINT *,count,' point*steps were adjusted '
      PRINT *,' '
      WRITE (*,9001) (( CHAR( cbase + tflag(i,j)),i=1,nx),j=1,ny)
      PRINT *,' '
      WRITE (*,9001) (( CHAR( cbase + sflag(i,j)),i=1,nx),j=1,ny)
 9001 FORMAT (77A1)

      PRINT *,' '
      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          IF (tflag(i,j) .GT. 0  .OR. sflag(i,j) .GT. 0) THEN
            WRITE (*,9002) i,j , tflag(i,j), sflag(i,j), bathy(i,j)
          ENDIF
 3100   CONTINUE
 3000 CONTINUE

 9002 FORMAT (2I5, 2I5, F7.0)

      STOP
      END
