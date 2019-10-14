      PROGRAM mrfcomp
C     Intercompare all fields from an mrf output file
      INTEGER nf, ny, nx
      PARAMETER (nf = 49)
      PARAMETER (ny = 192)
      PARAMETER (nx =  94)
 
      REAL f1(nx, ny), f2(nx, ny)
      REAL all1(nx, ny, nf), all2(nx, ny, nf)

      INTEGER i, j, k

      REAL iagree, ia, r2, sig2x, sig2y, xbar, ybar

      OPEN (10, FILE="dump.ref", FORM="UNFORMATTED", STATUS="OLD")
      OPEN (11, FILE="dump.thick", FORM="UNFORMATTED", STATUS="OLD")
      READ (10) all1
      READ (11) all2

      DO 1000 k = 1, nf
        CALL pull(all1, f1, k, nx, ny, nf)
        CALL pull(all2, f2, k, nx, ny, nf)
        ia = iagree(f1, f2, nx*ny)
        CALL correl(f1, f2, nx*ny, r2, xbar, ybar, sig2x, sig2y)
        WRITE (*,9001) k, ia, r2, xbar, ybar, SQRT(sig2x), SQRT(sig2y)
 1000 CONTINUE
 9001 FORMAT (I2, 2x, 2F6.3, 2E13.6, 2x, 2E13.6)


      STOP
      END
      SUBROUTINE pull(all, x, k, nx, ny, nf)
      INTEGER nx, ny, nf, k
      REAL x(nx, ny), all(nx, ny, nf)
      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          x(i,j) = all(i,j,k)
 1100   CONTINUE
 1000 CONTINUE
 
      RETURN
      END 
