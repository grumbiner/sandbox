      PROGRAM icevary
C     Restart on computing variational improvements to A
C     9 December 1996

      IMPLICIT none

      INTEGER nx, ny, nday
      PARAMETER (nx = 345 / 5)
      PARAMETER (ny = 355 / 5)
      PARAMETER (nday = 30)

      REAL ain(nx, ny, nday)
      REAL aout(nx, ny, nday)
      REAL atmp(nx, ny)
      REAL avec(nday)
      REAL beta, epsi

      INTEGER i, j, k

      CHARACTER*80 fname
        

      DO 1000 k = 1, 30
        IF (k .LT. 10) THEN
          WRITE (fname, 9001) k
        ELSE
          WRITE (fname, 9002) k
        ENDIF

        CALL cread(atmp, nx, ny, nday, fname)
        CALL acopin(ain, atmp, nx, ny, nday, k)
 1000 CONTINUE
 9001 FORMAT ("b3sred.96110",I1)
 9002 FORMAT ("b3sred.9611",I2)

      DO j = 1, ny
        DO i = 1, nx
          CALL getvec(ain, avec, i, j, nx, ny, nday) 
          CALL vary(avec, nday, epsi, beta)
          CALL putvec(aout, avec, i, j, nx, ny, nday)
        ENDDO
      ENDDO

      DO 3000 k = 1, 30
        IF (k .LT. 10) THEN
          WRITE (fname, 9003) k
        ELSE
          WRITE (fname, 9004) k
        ENDIF

        CALL acopout(ain, atmp, nx, ny, nday, k)
        CALL cwrite(atmp, nx, ny, nday, fname)
 3000 CONTINUE
 9003 FORMAT ("b3sout.96110",I1)
 9004 FORMAT ("b3sout.9611",I2)
     
      STOP
      END
