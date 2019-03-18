      PROGRAM icconc
C     Go from the character map to a concentration map.
      INTEGER nx, ny
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)

      CHARACTER map(nx, ny)
      REAL conc(nx, ny)

      INTEGER i, j

      OPEN (10, FILE='newice', FORM='FORMATTED', STATUS='OLD')
      OPEN (11, FILE='modelin', FORM='UNFORMATTED', STATUS='NEW')

 9001 FORMAT (77A1)
      DO 1000 j = 1, ny
        READ (10, 9001) (map(i,j),i=1,77)
 1000 CONTINUE

      DO 2000 j = 1, ny
        DO 2100 i = 1, nx
          IF (map(i,j) .EQ. 'I') THEN
            conc(i,j) = 100.0
           ELSE
            conc(i,j) = 0.0
          ENDIF
 2100   CONTINUE
 2000 CONTINUE

      WRITE (11) conc

      STOP
      END
