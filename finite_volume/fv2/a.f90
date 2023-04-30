PROGRAM alpha
IMPLICIT none

  REAL(SELECTED_REAL_KIND(14,30)) :: pi1, pi2
  REAL(SELECTED_REAL_KIND(14,30)) :: dp1, dp2 

  REAL(SELECTED_REAL_KIND(14,30)) :: dlat, dlon, firstlat, firstlon
  PARAMETER (dlat = 1.0)
  PARAMETER (dlon = 1.0)
  PARAMETER (firstlat = -90.0 + dlat / 2.)
  PARAMETER (firstlon = dlon / 2.)

  REAL(SELECTED_REAL_KIND(7,30)) :: tlat

  INTEGER j
  pi1 = 3.1415926535898d0
  pi2 = DACOS(-1.d0)
  PRINT *,pi1
  PRINT *,pi2
  PRINT *,'delta pi = ',pi2-pi1
  dp1 = pi1/180.d0
  dp2 = pi2/180.d0

  DO j = 1, 10
    tlat = firstlat + (j-1)*dlat
    PRINT *, tlat-dlat/2.-(-91.+j), dcos((tlat-dlat/2.)*dp1), cos((tlat-dlat/2.)*dp2)
  ENDDO

END PROGRAM
