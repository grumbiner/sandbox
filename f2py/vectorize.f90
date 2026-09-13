SUBROUTINE vectorize(u, v, r, theta, npts)
  IMPLICIT none
  INTEGER, intent(in) :: npts
  REAL, intent(in)    :: r(npts), theta(npts)
  REAL, intent(inout) :: u(npts), v(npts)
  REAL :: pi = 3.1416
  u = r*cos(theta)
  v = r*sin(theta)
  RETURN
END
  
