      PROGRAM dummy
C     use to generate data for figure 2, on aliasing. (paper 1)
C     Robert Grumbine 2-17-87

      REAL x(10000), y(10000)
      INTEGER i, j, n, l
      REAL pi, dt
      PARAMETER (pi = 3.141592654)

      PRINT *,'Which harmonic is the major?'
      READ (*,9001) i
      PRINT *,'which alias do you want?'
      READ (*,9001) l
      PRINT *,'How many points do you want?'
      READ (*,9001) n

 9001 FORMAT (I5)

      dt = 1./FLOAT(n)

      DO 1000 j = 1, n
        x(j) = cos(2.*pi*(i/(n*dt))*j*dt*.1)
        y(j) = SNGL(dcos(DBLE(2.*pi*(i/(n*dt) + l/dt) * j*dt*.1)))
 1000 CONTINUE

      PRINT *,'What do you want to call the main freq. file?'
      CALL ritout(x, n, 11)

      PRINT *,'What do you want to call the secondary freq file?'
      CALL ritout(y, n, 12)

      END
