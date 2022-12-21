      PROGRAM hsmain
C     Test program for advection in 2-d
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL uold(nx, ny), vold(nx, ny)
      REAL u(nx, ny), v(nx, ny)
      REAL unew(nx, ny), vnew(nx, ny)
      REAL qi(nx, ny), qj(nx, ny)
      REAL utav(nx, ny), vtav(nx, ny)
      REAL uc(nx, ny), vc(nx, ny)
      REAL ucav(nx, ny), vcav(nx, ny)

      REAL g(nx, ny), gold(nx, ny), gnew(nx, ny)
      REAL gd(nx, ny), gdold(nx, ny), gdnew(nx, ny)
      REAL w0, we(nx, ny), h(nx, ny)
      REAL q(nx, ny), qd(nx, ny), fs(nx, ny), fd(nx, ny)
      REAL s(nx, ny), p(nx, ny)

      REAL qsfmax, qsfref, qsm
      INTEGER strwin, strfll, strsum, strspr, loy

      REAL dx, dy, delt, dt

      REAL rhonot, grav, f, beta, ahm, asv, scrit
      INTEGER nint
      PARAMETER (nint = (nx-2)*(ny-2))
      REAL flm(ny), fls(ny), flmt(ny), flst(ny)

      REAL totg, totg2, diffu, q0, period, time
      INTEGER i, j, nstep, outfrq, forfrq
      REAL totgd, totgd2, totu, totv, totu2, totv2
      REAL dtdelt
      PARAMETER (dtdelt = 10.0)
C********(*********(*********(*********(*********(*********(*********(--
      CALL init(uold, vold, gnew, g, gold, we, w0,
     1         nx, ny, nstep, outfrq, forfrq,
     1                 diffu, q0, period,
     2 qsfmax, qsfref, qsm, loy, strwin, strspr, strsum, strfll,
     3 dx, dy, dt,
     4 rhonot, grav, f, beta, ahm, asv, scrit  )
      u = uold
      v = vold
      WRITE (11) uold
      WRITE (12) vold
      CALL ubc(u, v)
      CALL speed(s, u, v)
      CALL integ2(s, nx, ny, dx, dy, totu)
      CALL integ22(s, nx, ny, dx, dy, totu2)
      WRITE (*,9001) 0, totu/dx/dy, totu2/dx/dy

      CALL arset(gd, nx, ny, -0.10)
      CALL arset(gdold, nx, ny, -0.10)
      CALL arset(utav, nx, ny, 0.0)
      CALL arset(vtav, nx, ny, 0.0)
      CALL arset(ucav, nx, ny, 0.0)
      CALL arset(vcav, nx, ny, 0.0)
      CALL arset(h, nx, ny, 500.0)
      CALL arset(p, nx, ny, 0.0)
      CALL arset(qi, nx, ny, 0.0)
      CALL arset(qj, nx, ny, 0.0)
      DO 3200 j = 1, ny
        flmt(j) = 0.0
        flst(j) = 0.0
 3200 CONTINUE

C********(*********(*********(*********(*********(*********(*********(--
      CALL source(g, gd, q, qd, we, h, nx, ny, 0, qsfmax, qsfref, qsm,
     1 loy, strwin, strspr, strsum, strfll, dx, dy, dt)
      CALL ucext(uc, vc, g, h(1,1), rhonot, grav, f, beta, ahm, dx, dy)
C     Take the first step:
      CALL source(g, gd, q, qd, we, h, nx, ny, 1, qsfmax, qsfref, qsm,
     1 loy, strwin, strspr, strsum, strfll, dx, dy, dt)
      CALL isrc(uc, vc, u, v, we, g, gd, fs, fd, h, dx, dy, asv)
      CALL vadd(q, fs)
      CALL vadd(qd, fd)
      CALL adv2d1(u, v, q, g, gold, nx, ny, diffu, dx, dy, dt)
      CALL adv2d1(u, v, qd, gd, gdold, nx, ny, diffu, dx, dy, dt)
      CALL bcond(g, nx, ny)
      CALL bcond(gd, nx, ny)
      CALL convec(gd, nx, ny, 1)
      CALL ucext(uc, vc, g, h(1,1), rhonot, grav, f, beta, ahm, dx, dy)
      CALL arset(fs, nx, ny, 0.0)
      CALL arset(fd, nx, ny, 0.0)
      delt =  dt/dtdelt
      unew = uold
      vnew = vold
      CALL msrc(qi, qj, h, f, beta, w0, dx, dy)
      DO 900 i = 1, INT(dtdelt)
        CALL mad2d1(u, v, uold, vold, p, rhonot, qi, qj, h,
     1    f, beta, diffu, dx, dy, delt)
        CALL ubc(u, v)
  900 CONTINUE
      uold = unew
      vold = vnew

      CALL speed(s, u, v)
      CALL integ2(s, nx, ny, dx, dy, totu)
      CALL integ22(s, nx, ny, dx, dy, totu2)
      WRITE (*,9001) 1, totu/dx/dy, totu2/dx/dy

      DO 1000 i = 2, nstep
        time = dt * FLOAT(i)
        CALL source(g, gd, q, qd, we, h, nx, ny, i,
     1                    qsfmax, qsfref, qsm,
     1   loy, strwin, strspr, strsum, strfll, dx, dy, dt)
        CALL isrc(uc, vc, u, v, we, g, gd, fs, fd, h,
     1                                 dx, dy, asv)
        CALL vadd(q, fs)
        CALL vadd(qd, fd)
        CALL arset(fs, nx, ny, 0.0)
        CALL arset(fd, nx, ny, 0.0)

        IF (MOD(i,forfrq) .EQ. 0) THEN
          CALL adv2d1(u, v, q, g, gold, nx, ny, diffu,
     1                                  dx, dy, dt)
          CALL adv2d1(u, v, qd, gd, gdold, nx, ny, diffu,
     1                                  dx, dy, dt)
          CALL mad2d1(u, v, uold, vold, p, rhonot, qi, qj, h,
     1                  f, beta, diffu, dx, dy, dt)
         ELSE
          CALL adv2d2(u, v, q, gnew, g, gold, nx, ny,
     1                                 diffu, dx, dy, dt)
          CALL adv2d2(u, v, qd, gdnew, gd, gdold, nx, ny,
     1                                     diffu, dx, dy, dt)
          CALL mad2d2(unew, vnew, u, v, uold, vold, p, rhonot,
     1         qi, qj, h, f, beta, diffu, dx, dy, dt)
        ENDIF
        CALL bcond(g, nx, ny)
        CALL bcond(gd, nx, ny)
        CALL ubc(u, v)
        CALL vadd(utav, u)
        CALL vadd(vtav, v)

        CALL ucext(uc, vc, g, h(1,1), rhonot, grav, f, beta,
     1                                         ahm, dx, dy   )
        CALL convec(gd, nx, ny, i)

        CALL speed(s, u, v)
        CALL integ2(s, nx, ny, dx, dy, totu)
        CALL integ22(s, nx, ny, dx, dy, totu2)
        WRITE (*,9001) i, totu/dx/dy, totu2/dx/dy
        IF (MOD(i,outfrq) .EQ. 0) THEN
          WRITE (11) uold
          WRITE (12) vold
        ENDIF
CD        IF (i .GT. (nstep-2*loy)) THEN
CD          CALL reflux(v, vc, g, gd, h, flm, fls, scrit, nx, ny, dx)
CD          DO 3000 j = 1, ny
CD            flmt(j) = flmt(j)+flm(j)
CD            flst(j) = flst(j)+fls(j)
CD 3000     CONTINUE
CD          ucav = ucav+uc
CD          vcav = vcav+vc
CD          WRITE (13,9002) (flm(j)/1.E6,j=3,(2*ny)/3-1)
CD          WRITE (14,9002) (fls(j)/1.E5,j=3,(2*ny)/3-1)
CD        ENDIF

 1000 CONTINUE

      DO 3100 j = 1, ny
        flmt(j) = flmt(j)/FLOAT(2*loy)
        flst(j) = flst(j)/FLOAT(2*loy)
 3100 CONTINUE
      DO 4000 j = 1, ny
        DO 4100 i = 1, nx
CD          utav(i,j) = u(i,j) + ucav(i,j)/FLOAT(2*loy)
CD          vtav(i,j) = v(i,j) + vcav(i,j)/FLOAT(2*loy)
          utav(i,j) = utav(i,j)/FLOAT(nstep)
          vtav(i,j) = vtav(i,j)/FLOAT(nstep)
 4100   CONTINUE
 4000 CONTINUE
CD      WRITE (13,9002) (flmt(j)/1.E6,j=3,(2*ny)/3-1)
CD      WRITE (14,9002) (flst(j)/1.E5,j=3,(2*ny)/3-1)
CD      WRITE ( *,9002) (flmt(j)/1.E6,j=3,(2*ny)/3-1)
CD      WRITE ( *,9002) (flst(j)/1.E5,j=3,(2*ny)/3-1)
      WRITE (11) utav
      WRITE (12) vtav

 9001 FORMAT (I7, 4F14.6)

 9002 FORMAT (11F7.3)

      STOP
      END
