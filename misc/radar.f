      PROGRAM radar
C     Pragram to compute the maximum distance a given radar can detect
C       objects of a specified cross section.
C     Robert Grumbine 1-27-87

      IMPLICIT none

      REAL r(10000)
      INTEGER i, n
      REAL alpha, beta, delta, sigend, sigma

      PRINT *,' What is the radar constant?'
      READ (*,9001) beta
      PRINT *,'What is the exponent of the argument?'
      READ (*,9001) alpha
      PRINT *,'What do you want to start sigma at?'
      READ (*,9001) sigma
      PRINT *,'What do you want to iterate to?'
      READ (*,9001) sigend
      PRINT *,'How many steps do you want to take?'
      READ (*,9002) n

      delta = (sigend - sigma)/FLOAT(n)  

      DO 1000 i = 1, n+1
        sigma = sigma + delta 
        r(i)  = beta* (10.**sigma)**alpha
 1000 CONTINUE

      PRINT *,'What do you want to call the output file?'
      CALL ritout(r, n, 12)

 9001 FORMAT (E13.6)

 9002 FORMAT (I5)

      END
