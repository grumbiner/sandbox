      REAL FUNCTION p(m, r, g)
C     Function to compute the significance of the Fisher statistic for a given
C       m (number of frequencies) and r (order magnitude of the given frequ
C       ency.) according to the formula given in Shimshoni 1971.
C     Robert Grumbine 1987

      INTEGER m, r
      REAL g

      INTEGER j, l, s
      DOUBLE PRECISION pi, ptemp, fact

      pi    = 3.141592654D0
      ptemp = 0.D0
      s     = 1
      l     = INT(1./g)

      IF (r .EQ. 1) THEN
C       Use the first term of the series.  This is sufficient according to
C         Fisher and Shimshoni.
        p = FLOAT(m)*(1.-g)**(m-1)
       ELSE
C       Must use full formula.
C       The expression looks different from the one in Shimshoni's paper
C         bacause the factorial of some very large numbers are involved,
C         along with a multiplication by a very small number.  To avoid
C         overflow and underflow problems, I have used Stirling's 
C         approximation to the factorial, and rewrote the small term
C         (1-jg)**(m-1) as exp( (m-1)*Ln(1-jg) ).  Then I combined 
C         everything into one very large mess for evaluation.
C       The j=r term is evaluated separately because (j-r)! appears in 
C         the expression, and Stirling's approximation fails there.
        j = r
        ptemp = ptemp + DBLE(s) *( DEXP(-1.D0)/(2.D0*pi)**.5 )* 
     1      DSQRT( DBLE((m-j)*(r-1) )/DBLE(m) )*
     2      DEXP(DBLE(m)*DLOG(DBLE(m)) - DBLE(m-j)*DLOG(DBLE(m-j))
     3        -DBLE(r-1)*DLOG(DBLE(r-1)) 
     4        +DBLE(m-1)*DLOG(1.D0-DBLE(j)*DBLE(g))     )
     5         / DBLE(j)
        s = -s

C       The upper limit is MIN(m-1, l) because it is common for 1/g to 
C         be greater than the length of the series.  When this occurs, 
C         p will be evaluated as improbable.
        DO 1000 j = r+1, MIN0(m-1,l)
          ptemp = ptemp + DBLE(s) *( DEXP(-1.D0)/(2.D0*pi) )* 
     1        DSQRT( DBLE((m-j)*(j-r)*(r-1) )/DBLE(m) )*
     2        DEXP(DBLE(m)*DLOG(DBLE(m)) - DBLE(m-j)*DLOG(DBLE(m-j))
     3          -DBLE(j-r)*DLOG(DBLE(j-r))-DBLE(r-1)*DLOG(DBLE(r-1)) 
     4          +DBLE(m-1)*DLOG(1.D0-DBLE(j)*DBLE(g))     )
     5           / DBLE(j)
C         PRINT *,'j = ',j,'ptemp = ',ptemp
          s = -1 * s
 1000   CONTINUE

C       PRINT *,ptemp, s
        IF (ptemp .LT. 0.D0) THEN
          PRINT *,'negative p, corrected in subroutine p.'
          ptemp = 1.D0 
        ENDIF

        p = ptemp 

      ENDIF

      RETURN
      END
