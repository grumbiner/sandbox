      PROGRAM af
C     Compute mean and sd of data for bar-conc vs conc-bar
      REAL sum, sumx2, x(-9: 9)
      INTEGER i, n
      x(9) = 1
      x(8) = 0
      x(7) = 2
      x(6) = 3
      x(5) = 5
      x(4) = 1
      x(3) = 4
      x(2) = 21
      x(1) = 2506
      x(0) = 14743
      x(-1) = 6967
      x(-2) = 236
      x(-3) = 4
      x(-4) = 2
      x(-5) = 0
      x(-6) = 1
      x(-7) = 2
      x(-8) = 3

      sum = 0
      sumx2 = 0
      n     = 0

      DO 1000 i = -8, 9
        sum = sum + i*x(i)
        n   = n + x(i)
        sumx2 = sumx2 + i**2 * x(i)
 1000 CONTINUE
    
      PRINT *,n, sum/n, (n*sumx2-sum**2)/FLOAT(n-1)/FLOAT(n)
      PRINT *, SQRT( (n*sumx2-sum**2)/FLOAT(n-1)/FLOAT(n) )
 
      STOP
      END
