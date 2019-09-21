C********************************************
      SUBROUTINE aread(a0, nx, ny, fname)
      INTEGER nx, ny, i, j
      CHARACTER*60 fname
      REAL a0(nx, ny)
      
      OPEN (10, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      DO 1000 j = 1, ny
      DO 1001 i = 1, nx
        READ (10,*) d1, d2, a0(d1+1,d2+1)
 1001 CONTINUE
 1000 CONTINUE

      RETURN
      END 
