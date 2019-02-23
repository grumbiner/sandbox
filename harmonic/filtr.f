      SUBROUTINE filtr
C     Subroutine to perform a simple low-pass filtering of a data record.
C     Robert Grumbine 7-29-86.

      INTEGER nrec
      REAL x(9000), xpart(9000), avg(9000) 
      INTEGER filter(3)

      INTEGER i, j, k 
      REAL dummy, avtmp, sdtmp
      
      filter(1) = 25
      filter(2) = 24
      filter(3) = 24
 
      PRINT *,'How many points are in the data array?'
      READ (*,9001) nrec

      CALL readin(x, nrec)

      DO 2000 k = 1, 3

        DO 1000 j = 1, nrec-filter(k) 

          DO 1010 i = 1, filter(k)
            xpart(i) = x(i + (j-1) )
 1010     CONTINUE

          CALL rmsval(xpart, filter(k), dummy, avtmp, sdtmp )
          avg(j) = avtmp

 1000   CONTINUE
        DO 1100 i = 1, nrec-filter(k)
          x(i) = avg(i)
 1100   CONTINUE

 2000 CONTINUE

      CALL ritout (avg, nrec-filter(1)-filter(2)-filter(3), 10)

 9001 FORMAT (I5)
      
      RETURN
      END
