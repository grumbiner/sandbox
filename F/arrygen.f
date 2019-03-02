      PROGRAM arrygen
C     generate an array with a given format
C     Robert Grumbine 2 May 1995

      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL h(nx, ny)

      INTEGER i, j
      REAL dx, dy
      REAL hnot, a, b, c, d, slop
      INTEGER type, jbot, jbreak
      CHARACTER*60 fname
      DOUBLE PRECISION drand48

      PRINT *,'What type of array would you like?'
      PRINT *,'     1 = hnot*linear(y)*linear(x) '
      PRINT *,'     2 = hnot*exponential(y)*exp(x)'
      PRINT *,'     3 = hnot*[exp(y) + gauss(x)] (trench)'
      PRINT *,'     4 = hnot*[exp(y) - gauss(x)] (ridge)'
      PRINT *,'     5 = hnot + cshelf(y) '
      PRINT *,'     6 = hnot '
      PRINT *,'     7 = hnot*sin(kx)*sin(ly)'
      PRINT *,'     8 = hnot*sin(ly)'
      PRINT *,'     9 = uniform random'
      READ (*,9001) type

      PRINT *,'What is delta x?'
      READ (*,9002) dx
      PRINT *,'What is delta y?'
      READ (*,9002) dy
      PRINT *,'What is the reference value?'
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

        ELSE IF (type .EQ. 5) THEN
         PRINT *,'Where is the top of the shelf break?'
         READ (*,9002) b
         PRINT *,'Where is the bottom of the shelf break?'
         READ (*,9002) c
         PRINT *,'What is the depth of the deep sea?'
         READ (*,9002) d
         slop = (d-hnot)/(c-b+1.)
         jbreak = MIN0(INT(b/dy), ny)
         jbot   = MIN0(INT(c/dy), ny)
         PRINT *,'shelf parms', jbreak, jbot, slop, nx, ny
         DO 5000 j = 1, jbreak-1
           DO 5010 i = 1, nx
             h(i,j) = hnot 
 5010      CONTINUE
 5000    CONTINUE
         DO 5020 j = jbreak, jbot
           DO 5030 i = 1, nx
             h(i,j) = hnot + slop*dy*(1+j-jbreak)
 5030      CONTINUE
 5020    CONTINUE
         DO 5040 j = jbot+1, ny
           DO 5050 i = 1, nx
             h(i,j) = d
 5050      CONTINUE
 5040    CONTINUE

       ELSE IF (type .EQ. 6) THEN
        DO 6000 j = 1, ny
          DO 6010 i = 1, nx
            h(i,j) = hnot
 6010     CONTINUE
 6000   CONTINUE

       ELSE IF (type .EQ. 7) THEN
        PRINT *,'What is the wave length in x?'
        READ (*,9002) a
        PRINT *,'What is the wave length in y?'
        READ (*,9002) b
        DO 7000 j = 0, ny-1
          DO 7010 i = 0, nx-1
            h(i+1,j+1) = hnot*sin(2.*pi*i*dx/a)*sin(2.*pi*j*dy/b)
 7010     CONTINUE
 7000   CONTINUE

       ELSE IF (type .EQ. 8) THEN
        PRINT *,'What is the wave length in y?'
        READ (*,9002) b
        DO 8000 j = 0, ny-1
          DO 8010 i = 1, nx
            h(i,j+1) = hnot*sin(2.*pi*j*dy/b)
 8010     CONTINUE
 8000   CONTINUE
   
       ELSE IF (type .EQ. 9) THEN
        PRINT *,'What amplitude would you like?'
        READ (*,9002) b
        DO 9100 j = 1, ny
          DO 9110 i = 1, nx
            h(i,j) = SNGL(drand48())*b*2. - b
 9110     CONTINUE
 9100   CONTINUE
     
        ELSE
         PRINT *,'not implemented'
 
      ENDIF

      PRINT *,'What would you like to call the array?'
      READ (*,9003) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) h
      CLOSE (10, STATUS='KEEP')

 9001 FORMAT (I1)

 9002 FORMAT (E13.7)

 9003 FORMAT (A60)

      END
