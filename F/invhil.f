      SUBROUTINE invhil(t, n)
C     Generate the inverse of the holbert matrix of degree n.
C     Robert Grumbine 27 Sep 1995

      REAL t(n, n)
C     Note, T is actually an array of integers, but when its inverse
C       is computed, it will generate reals.  To avoid type mismatch,
C       declare t to be real here.
      INTEGER p, r, i, j
      
      p = n 
      DO 10 i = 1, n
        IF (i .NE. 1) p = ((n-i+1)*p*(n+i-1))/(i-1)**2
        r = p**2
        t(i,i) = float(r/(2*i-1))
        DO 20 j = i+1, n
          r = -((n-j+1)*r*(n+j-1))/(j-1)**2
          t(i, j) = float(r/(i+j-1))
          t(j, i) = t(i, j)
  20    CONTINUE
  10  CONTINUE

      RETURN
      END      
