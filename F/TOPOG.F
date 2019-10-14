      PROGRAM topog
C     generate a topographic field for the barotropic flow soln.

      INTEGER nx, ny
      PARAMETER (nx = 20)
      PARAMETER (ny = 20)
      REAL h(nx, ny)

      INTEGER i, j
      REAL dx, dy
      REAL hnot, a, b, c, d
      INTEGER type
      CHARACTER*60 fname

      PRINT *,'What type of topography would you like?'
      PRINT *,'     1 = hnot*linear(y)*linear(x) '
      PRINT *,'     2 = hnot*exponential(y)*exp(x)'
      PRINT *,'     3 = hnot*[exp(y) + gauss(x)] (ridge)'
      PRINT *,'     4 = hnot*[exp(y) - gauss(x)] (trench)'
      READ (*,9001) type

      PRINT *,'What is delta x?'
      READ (*,9002) dx
      PRINT *,'What is delta y?'
      READ (*,9002) dy
      PRINT *,'What is the reference depth?'
      READ (*,9002) hnot

      IF (type .EQ. 1) THEN
        PRINT *,'What is the slope in x?'
        READ (*,9002) a
        PRINT *,'What is the slope in y?'
        READ (*,9002) b
        DO 1000 j = 1, ny
          DO 1010 i = 1, nx
            h(i,j) = hnot*(1+a*i*dx)*(1+b*j*dy)
 1010     CONTINUE
 1000   CONTINUE

       ELSE IF (type .EQ. 2) THEN
        PRINT *,'What is the e-folding length in x?'
        READ (*,9002) a
        PRINT *,'What is the e-folding length in y?'
        READ (*,9002) b
        DO 2000 j = 1, ny
          DO 2010 i = 1, nx
            h(i,j) = hnot*exp(a*i*dx+b*j*dy)
 2010     CONTINUE
 2000   CONTINUE

       ELSE IF (type .EQ. 3) THEN
        PRINT *,'What is the e-folding length in y?'
        READ (*,9002) a
        PRINT *,'Where is the center of the ridge?'
        READ (*,9002) b
        PRINT *,'What is the s. dev. of the ridge?'
        READ (*,9002) c
        PRINT *,'What is the relative amplitude of the ridge?'
        READ (*,9002) d
        DO 3000 j = 1, ny
          DO 3010 i = 1, nx
            h(i,j) = hnot*(exp(a*j*dy)-d*exp(-(ABS((b-i*dx))/c)**2 ) )
 3010     CONTINUE
 3000   CONTINUE

       ELSE IF (type .EQ. 4) THEN
        PRINT *,'What is the e-folding length in y?'
        READ (*,9002) a
        PRINT *,'Where is the center of the trench?'
        READ (*,9002) b
        PRINT *,'What is the s. dev. of the trench?'
        READ (*,9002) c
        PRINT *,'What is the relative amplitude of the trench?'
        READ (*,9002) d
        DO 4000 j = 1, ny
          DO 4010 i = 1, nx
            h(i,j) = hnot*(exp(a*j*dy)+d*exp(-(ABS((b-i*dx))/c)**2 ) )
 4010     CONTINUE
 4000   CONTINUE

        ELSE
         PRINT *,'not implemented'
 
      ENDIF

      PRINT *,'What would you like to call the topography file?'
      READ (*,9003) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) h
      CLOSE (10, STATUS='KEEP')

 9001 FORMAT (I1)

 9002 FORMAT (E13.7)

 9003 FORMAT (A60)

      END
