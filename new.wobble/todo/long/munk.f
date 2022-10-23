      PROGRAM munk
      IMPLICIT none

      INTEGER n
      PARAMETER (n = 6136)

      REAL radius(n), press(n)

      REAL period, dt
      PARAMETER (period = 7*435) !days
      PARAMETER (dt     = 2.5) 

      INTEGER nsums, nrealizations
      PARAMETER (nsums = 2*period / dt)
      PARAMETER (nrealizations = n / nsums)

      COMPLEX g(nsums/4, nrealizations ), h(nsums/4, nrealizations )
      COMPLEX z(nsums/4), cohere(nsums/4)

      INTEGER i

      PRINT *,'nsums = ',nsums
      PRINT *,'nrealize = ', nrealizations
      OPEN (10, FILE="radius", FORM="FORMATTED", STATUS="OLD")
      OPEN (11, FILE="press", FORM="FORMATTED", STATUS="OLD")

      DO i = 1, n
        READ (10,*) radius(i)
        READ (11,*) press(i)
      ENDDO

      CALL estim(radius, n, dt, period, g, nsums, nrealizations)
      CALL estim(press , n, dt, period, h, nsums, nrealizations)

      CALL ensemble(g, h, nsums/4, nrealizations, z, cohere, period)


      STOP
      END
