C********************************************
      SUBROUTINE readin(a0, nx, ny)
      IMPLICIT none
      INTEGER d1, d2
      INTEGER nx, ny, i, j
      CHARACTER*60 fname
      REAL a0(nx, ny)
      
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      DO 1000 j = 1, ny
      DO 1001 i = 1, nx
        READ (10,*) d1, d2, a0(d1,d2)
 1001 CONTINUE
 1000 CONTINUE

      RETURN
      END 
