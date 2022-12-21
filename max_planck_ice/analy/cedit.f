      PROGRAM cedit
C     Edit the concentration map
C     Robert Grumbine 7 November 1994

      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)
      REAL conc(nx, ny)
      INTEGER mask(nx, ny)
      CHARACTER map(nx, ny)

      INTEGER i, j

      OPEN (10, FILE='conc.north.18', FORM='UNFORMATTED', STATUS='OLD')
      OPEN ( 9, FILE='imask', FORM='FORMATTED', STATUS='OLD')
      READ (10) conc
      READ (10) conc
      READ (10) conc
      READ (10) conc
      READ (10) conc
      READ (10) conc
      READ (10) conc

      DO 100 j = 1, ny
        READ (9, 9002) (mask(i,j),i=1,nx) 
CD        WRITE (*, 9002) (mask(i,j),i=1,nx) 
 100  CONTINUE
 9002 FORMAT (77I1)

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          IF (conc(i,j) .GT. 0.0) THEN
            map(i,j) = 'I'
           ELSE
            map(i,j) = '0'
          ENDIF
          IF (mask(i,j) .EQ. 0) map(i,j) = '.'
 1100   CONTINUE
 1000 CONTINUE

      OPEN (11, FILE='icemap', FORM='FORMATTED', STATUS='NEW')
      DO 2000 j = 1, ny
        WRITE (11, 9001) (map(i,j),i=1,77)
 2000 CONTINUE
 9001 FORMAT (77A1)

      STOP
      END
