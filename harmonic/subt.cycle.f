      SUBROUTINE demod (a, b, freq, n, x, m)

C     Subroutine to subtract harmonically analyzed components from the data.
C     Robert Grumbine.  Re-entered after accidental deletion 8-28-86.

      INTEGER n, m
      REAL a(n), b(n), freq(n)
      REAL x(m)

      INTEGER i, j
      REAL t

      DO 1000 i = 1, m
C       t = i-1 due to manner of harmonic analysis.
C       Use dcos, dsin because the argument may become larger than 
C         allowed on the HP 9000. 
        t = FLOAT(i) - 1.
        DO 1010 j = 1, n
          x(i) = x(i) -a(j)*dcos(DBLE(t*freq(j)))
     1                -b(j)*dsin(DBLE(t*freq(j)))
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
