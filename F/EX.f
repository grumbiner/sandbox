      SUBROUTINE EX (A, B, X, N, BUFFER, ERR)
      REAL a(n,n), b(n,n), x(n,n), buffer, err
      INTEGER n

      CALL invhil(a,n)
      buffer = 1.E-12
      err    = 0.0

      CALL iparpiv(a, b, n, buffer, err)
      IF (err .NE. 0.0) THEN
        WRITE (6,111)
 111    FORMAT ('1 Error occurred, cannot proceed further. ')
        STOP
      ENDIF

      CALL ibaksol(a, b, x, n, buffer, err)
      IF (err .NE. 0.0) THEN
        WRITE (6,111)
        STOP
      ENDIF

      CALL hilbrt(a, n)

      WRITE (6, 110) n
 110  FORMAT ('1 Compare relative error for T inverse and H for n=',I3)
      DO 100 i = 1, n
        WRITE (6, 101) (x(i, j),j=1, n)
 101    FORMAT (' ',6e20.12)
 100  CONTINUE
      DO 200 i = 1, n
        WRITE (6,101) (( a(i,j)-x(i,j))/abs(a(i,j)), j=1, n)
 200  CONTINUE

      RETURN
      END      
