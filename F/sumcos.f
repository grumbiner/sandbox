      FUNCTION sumsin(freq, time, n)
C     Sin, cos summation by 27 September 1995 Robert Grumbine
C     Minor F90-ish updating -- implicit none, do loops, no ENTRY 7 April 2014
      IMPLICIT none

      REAL sumsin, sumcos
      DOUBLE PRECISION tsin, tcos

      INTEGER i, n
      REAL time(n), freq

      tsin = 0.D0
      DO i = 1, n
       tsin = tsin + dsin(DBLE( freq*time(i) ))
      ENDDO

      sumsin = SNGL(tsin)

      RETURN
      END
      REAL FUNCTION sumcos(freq, time, n)
      IMPLICIT none
      DOUBLE PRECISION tcos
      INTEGER i, n
      REAL time(n), freq

      tcos = 0.D0
      DO i = 1, n
        tcos = tcos + dcos(DBLE( freq*time(i) ))
      ENDDO

      sumcos = SNGL(tcos)

      RETURN
      END
