      PROGRAM score
       
      INTEGER i, n
      PARAMETER (n = 81)
      REAL alpha_sw(n), alpha_lw(n), alpha_sh(n), alpha_lh(n),
     1         r2(n), xbar(n), ybar(n), sig2x(n), sig2y(n)
      REAL s1(n), s2(n)

      DO i = 1, 81
        READ (*,*) alpha_sw(i), alpha_lw(i), alpha_sh(i), alpha_lh(i),
     1         r2(i), s1(i), xbar(i), s2(i), sig2x(i)
      ENDDO

C     We're assuming that the score file has already been sorted on
C       the correlation coefficient, so a non-dominated solution must
C       have better s1 or s2 than the first line
      i = 1
      WRITE (*,9001)  alpha_sw(i), alpha_lw(i), alpha_sh(i), 
     1              alpha_lh(i),
     1              r2(i), s1(i), s2(i)
      DO i = 2, n
        IF (s1(i) .GT. s1(1) .OR. s2(i) .GT. s2(1) ) THEN
          WRITE (*,9001)  alpha_sw(i), alpha_lw(i), alpha_sh(i), 
     1              alpha_lh(i),
     1              r2(i), s1(i), s2(i)
        ENDIF
      ENDDO
 9001 FORMAT (4F7.3,F7.3,F7.3,F7.3)

      END
