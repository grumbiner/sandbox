C********************************************
      SUBROUTINE readin(a0, a, nx, ny, nt)
      INTEGER nx, ny, nt, i, j, k
      CHARACTER*60 fname
      REAL a0(nx, ny, nt)
      REAL a(nx, ny, nt)
      
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      DO 1002 k = 1, nt
      DO 1000 j = 2, ny-1
      DO 1001 i = 2, nx-1
        READ (10,*) d1, d2, a0(d1,d2,k), a(d1, d2, k)
 1001 CONTINUE
 1000 CONTINUE
 1002 CONTINUE

      RETURN
      END 
