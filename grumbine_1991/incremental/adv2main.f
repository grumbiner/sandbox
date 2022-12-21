      PROGRAM full
C     Test program for advection in 2-d
C     Robert Grumbine 2 June 1994

      IMPLICIT none
      INCLUDE "grid.inc"

      REAL u(nx, ny), v(nx, ny), q(nx, ny)
      REAL g(nx, ny), gold(nx, ny), gnew(nx, ny)
      REAL we(nx, ny), h(nx, ny), qsd(nx, ny)
      REAL qsfmax, qsfref, qsm
      PARAMETER (qsfmax = 3.47E-5/500.)
      PARAMETER (qsfref = 1.37E-6/500.)
      PARAMETER (qsm    = -2.28E-6/500.)

      REAL dx, dy, dt
      PARAMETER (dx = 2.0E4)
      PARAMETER (dy = 2.0E4)
      PARAMETER (dt = 1.31485E5)

      REAL totg, totg2, total, diffu, q0, period, time
      INTEGER i, j, nstep, outfrq, forfrq
      REAL totu2, totv2, totq2


      OPEN (10, FILE='advout', FORM='UNFORMATTED', STATUS='NEW')

      DO 1 j = 1, ny
        DO 2 i = 1, nx
          we(i,j) = 2.E-7
          h(i,j) = 500.0
   2    CONTINUE
   1  CONTINUE

      CALL init(u, v, gnew, g, gold, h, nx, ny, nstep, outfrq,
     1           forfrq, diffu, q0, period, dx, dy)
      CALL integ2(g, nx, ny, dx, dy, total)
      WRITE (10) g

      CALL source(g, q, qsd, we, h, nx, ny, 0, qsfmax, qsfref, qsm)
      CALL adv2d1(u, v, q, g, gold, nx, ny, diffu, dx, dy, dt)
      CALL integ2(g, nx, ny, dx, dy, totg)
      CALL integ22(g, nx, ny, dx, dy, totg2)
      CALL integ22(u, nx, ny, dx, dy, totu2)
      CALL integ22(v, nx, ny, dx, dy, totv2)
      CALL integ22(q, nx, ny, dx, dy, totq2)
      PRINT *,'u2, v2, q2',totu2, totv2, totq2
      WRITE (*,9001) 0, totg/dx/dy/nx/ny, totg2/dx/dy/nx/ny

      DO 1000 i = 1, nstep
        time = dt * FLOAT(i)
        CALL source(g, q, qsd, we, h, nx, ny, i, qsfmax, qsfref, qsm)
        IF (MOD(i,forfrq) .EQ. 0) THEN
         CALL adv2d1(u, v, q, g, gold, nx, ny, diffu, dx, dy, dt)
         ELSE
        CALL adv2d2(u, v, q, gnew, g, gold, nx, ny, diffu, dx, dy, dt)
        ENDIF
        CALL sbc(g, nx, ny)
        IF (MOD(i,outfrq) .EQ. 0) THEN
          CALL integ2(g, nx, ny, dx, dy, totg)
          CALL integ22(g, nx, ny, dx, dy, totg2)
          WRITE (*,9001) i, totg/dx/dy/nx/ny, totg2/dx/dy/nx/ny
          WRITE (10) g
        ENDIF
 1000 CONTINUE

 9001 FORMAT (I7, 2F14.6)

      STOP
      END
