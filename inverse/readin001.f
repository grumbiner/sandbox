C********************************************
      SUBROUTINE readin(a0, nx, ny)
      INTEGER nx, ny, i, j
      CHARACTER*60 fname
      REAL a0(nx, ny)
      
      fname = "beta"
      OPEN (10, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      DO 1000 j = 1, ny
      DO 1001 i = 1, nx
        READ (10,*) d1, d2, a0(d1,d2)
 1001 CONTINUE
 1000 CONTINUE

      RETURN
      END 
