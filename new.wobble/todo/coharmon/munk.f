      PROGRAM munk
      IMPLICIT none

      INTEGER n
      PARAMETER (n = 6136)

      REAL radius(n), press(n)

      REAL period, dt
      PARAMETER (period = 8*365) !days
      PARAMETER (dt     = 2.5) 

      INTEGER nsums, nrealizations, over
      PARAMETER (nsums = 2*period / dt)
      PARAMETER (nrealizations = n / nsums)
      PARAMETER (over = 16)

      COMPLEX g(nsums/4*over, nrealizations )
      COMPLEX h(nsums/4*over, nrealizations )
      COMPLEX z(nsums/4*over), cohere(nsums/4*over)

      INTEGER i

      PRINT *,'nsums = ',nsums
      PRINT *,'nrealize = ', nrealizations
      OPEN (10, FILE="radius", FORM="FORMATTED", STATUS="OLD")
      OPEN (11, FILE="press", FORM="FORMATTED", STATUS="OLD")

      DO i = 1, n
        READ (10,*) radius(i)
        READ (11,*) press(i)
      ENDDO

      CALL estim(radius, n, dt, period, g, nsums, over, nrealizations)
      CALL estim(press , n, dt, period, h, nsums, over, nrealizations)

      CALL ensemble(g, h, nsums/4*over, nrealizations, z, cohere, 
     1                    period, over)


      STOP
      END
