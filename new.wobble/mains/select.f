      PROGRAM select
      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 144)
      PARAMETER (ny =  73)
      REAL press(nx, ny)
      INTEGER i, j, k, nday
      INTEGER year, month
      CHARACTER*80 fname
      INTEGER loci, locj

      OPEN (10, FILE="selected", FORM="FORMATTED", STATUS="UNKNOWN")
  
      loci = 72
      locj = 24
      DO year = 1948, 1989
        IF (MOD(year, 4) .EQ. 0) THEN
          nday = 366
        ELSE
          nday = 365
        ENDIF
 
        WRITE(fname, 9001) year
        OPEN (11, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")

        DO k = 1, 4*nday
          READ(11, ERR=8000) press
          WRITE (10,*) press(loci, locj)
CD          WRITE (* ,*) press(loci, locj)
        ENDDO
 8000   CONTINUE 
        CLOSE (11)

      ENDDO
 9001 FORMAT ("bin.",I4)

      STOP
      END
