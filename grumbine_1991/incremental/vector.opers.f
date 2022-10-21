C***********************************************************----------!!
      SUBROUTINE vadd(a, b, npts)
C     A = A + B
      REAL a(npts), b(npts)
      INTEGER i

      DO 1000 i = 1, npts
        a(i) = a(i)+b(i)
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE vadd2(a, b, npts)
C     A = A + B
      REAL a(npts), b(npts)
      INTEGER i

      DO 1000 i = 1, npts
        a(i) = a(i)+b(i)
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE vdiff3(x, y, ndat, z)
C     compute the difference between two vectors, x-y = z

      INTEGER ndat
      REAL x(ndat), y(ndat), z(ndat)
      INTEGER i

      DO 1000 i = 1, ndat
        z(i) = x(i)-y(i)
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE speed(s, u, v, nx, ny)
      IMPLICIT none
      INTEGER nx, ny
      REAL s(nx*ny), u(nx*ny), v(nx*ny)
      INTEGER k

      DO 1000 k = 1, nx*ny
        s(k) = u(k)*u(k)+v(k)*v(k)
 1000 CONTINUE
      DO 1100 k = 1, nx*ny
        s(k) = SQRT(s(k))
 1100 CONTINUE

      RETURN
      END
C***********************************************************----------!!
