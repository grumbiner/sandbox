PROGRAM daylight
  IMPLICIT none
  REAL utc
  PARAMETER (utc = 64.184 / 8.64e4)
  REAL n

  REAL m, c, lambda
  REAL phi, pi, delta, omega

  INTEGER i
  REAL lol(366),ddt(366),ddtt(366)


  pi  = 3.1416
  phi = 39.0
  ddt = 0.
  ddtt = 0.

  DO i = 1, 366 
    n = i + utc

    m = AMOD((357.5291 + 0.98560028*n), 360.)
    c = 1.9148*sin(m*pi/180.)+2.e-2*sin(2.*m*pi/180.) + 3.e-4*sin(3.*m*pi/180.)

    lambda = AMOD((m + c + 180+102.9372),360.)
    delta = ASIN(sin(23.44*pi/180.)*sin(lambda*pi/180.))
!delta in rads
    omega = ACOS(-tan(phi*pi/180.)*tan(delta))

    lol(i) =  2.*omega*180/pi/360*24.

    !PRINT *,i, m, c,lambda,delta, omega*180/pi, 2.*omega*180/pi/360*24.
    !PRINT *,i, delta, omega*180/pi, 2.*omega*180/pi/360*24.

  ENDDO

  DO i = 2, 365
    ddt(i) = lol(i)-lol(i-1)
    ddtt(i) = lol(i+1)-2.*lol(i)+lol(i-1)
    PRINT *,i,lol(i), ddt(i)*3600, ddtt(i)*3600
  ENDDO
END
