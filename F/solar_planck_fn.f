      PROGRAM pl
      IMPLICIT none

      INTEGER nlam, ntemp
      PARAMETER (nlam = 200000)
      PARAMETER (ntemp = 100)
      REAL(SELECTED_REAL_KIND(6,20)), external :: planck
      REAL(KIND=8), external :: planck2
      REAL lambda, t
      INTEGER i, j, k

      REAL dl, dt, luminance(nlam)

      LOGICAL isuv, isir, isvis, isnir, istir, isfir, isfarir

      REAL(SELECTED_REAL_KIND(6,20)) :: sum, sumuv, sumir, sumvis
      REAL(SELECTED_REAL_KIND(6,20)) :: sumnir, sumtir, sumfir
      REAL(SELECTED_REAL_KIND(6,20)) :: sumfarir

      REAL(SELECTED_REAL_KIND(6,20)) :: sigma, pi, sb
      PARAMETER (sigma = 5.670373e-8)

      pi = ABS(ACOS(-1.0))

      dl = (200e-6)/FLOAT(nlam)
      dt = (325-180)/FLOAT(ntemp)
      dt = (5800-5200)/FLOAT(ntemp)
      DO j = 0, ntemp
        t = 5200. + j*dt
        sum = planck2(luminance, dl, nlam, t)
        sumuv = 0.0
        sumvis = 0.0
        sumnir = 0.0
        sumtir = 0.0
        sumfir = 0.0
        sumfarir = 0.0
        DO k = 1, nlam
          lambda = k*dl
          IF (isuv(lambda)) THEN
            sumuv = sumuv + luminance(k)
          ELSE IF (isvis(lambda)) THEN
            sumvis = sumvis + luminance(k)
          ELSE IF (isnir(lambda)) THEN
            sumnir = sumnir + luminance(k)
          ELSE IF (istir(lambda)) THEN
            sumtir = sumtir + luminance(k)
          ELSE IF (isfir(lambda)) THEN
            sumfir = sumfir + luminance(k)
          ELSE
            PRINT *,'should not be here, lambda = ',lambda
          ENDIF
          IF (isfarir(lambda)) THEN
            sumfarir = sumfarir + luminance(k)
          ENDIF
        ENDDO 

        sb = sigma / pi * t
        sb = sb * t * t * t
        WRITE(*,9001) t,sum, 1e6*(1. - sum/sb), 
!     1     sumuv*dl/sum, sumvis*dl/sum, sumnir*dl/sum, 
!     2     sumtir*dl/sum, sumfir*dl/sum, sumfarir*dl/sum
     1     sumuv*dl, sumvis*dl, sumnir*dl, 
     2     sumtir*dl, sumfir*dl, sumfarir*dl
      ENDDO
 9001 FORMAT (F8.2,1x,E13.6,1x,F8.3,6E13.6)

      END

!      DOUBLE PRECISION
!      REAL(SELECTED_REAL_KIND(6,20)) FUNCTION planck2(luminance, dl, nlam, t)
      REAL(KIND=8) FUNCTION planck2(luminance, dl, nlam, t)
      IMPLICIT none
      REAL c1, c2
      PARAMETER (c1 = 1.191042e8 / 1.e24) ! convert to SI
      PARAMETER (c2 = 1.4387752e4 / 1.e6) ! convert to SI
      REAL(KIND=8), external :: integral_flat
      REAL(KIND=8), external :: integral_trapezoid
      
      INTEGER nlam
      REAL luminance(nlam), dl, t

      INTEGER i
      REAL(SELECTED_REAL_KIND(6,20)) :: sum
      REAL lambda
    
      sum = 0.0
      DO i = 1, nlam
        lambda = i * dl
        luminance(i) = c1 / lambda**5 / (exp(c2/t/lambda) - 1.)
!        sum = sum + luminance(i)
      ENDDO

!      planck2 = sum*dl
!      planck2 = integral_flat(luminance, nlam, dl)
      planck2 = integral_trapezoid(luminance, nlam, dl)
      RETURN
      END

      REAL(SELECTED_REAL_KIND(6,20)) FUNCTION planck(lambda, t)
      IMPLICIT none
      REAL lambda, t
      REAL(SELECTED_REAL_KIND(6,20)) :: c1, c2
      PARAMETER (c1 = 1.191042e8 / 1.e24) ! convert to SI
      PARAMETER (c2 = 1.4387752e4 / 1.e6) ! convert to SI

      planck = c1 / lambda**5 / (exp(c2/t/lambda) - 1.)

      RETURN
      END
      LOGICAL FUNCTION isuv(lambda)
      IMPLICIT none
      REAL lambda
      isuv = (lambda .LE. 4000e-10) ! 4000 Angstrom units
      RETURN 
      END
      LOGICAL FUNCTION isir(lambda)
      IMPLICIT none
      REAL lambda
      isir = (lambda .GE. 7000e-10) ! 4000 Angstrom units
      RETURN 
      END
      LOGICAL FUNCTION isvis(lambda)
      IMPLICIT none
      REAL lambda
      LOGICAL isuv, isir
      isvis = .NOT. isuv(lambda) .AND. .NOT. isir(lambda)
      RETURN 
      END
      LOGICAL FUNCTION isnir(lambda)
      IMPLICIT none
      REAL lambda
      isnir = (lambda .GT. 7000e-10) .AND. (lambda .LE. 4e-6) ! 7000 Angstrom units to 4 microns
      RETURN 
      END
      LOGICAL FUNCTION istir(lambda)
      IMPLICIT none
      REAL lambda
      istir = (lambda .GT. 4e-6) .AND. (lambda .LT. 20e-6) ! 4-20 microns Angstrom units
      RETURN 
      END
      LOGICAL FUNCTION isfir(lambda)
      IMPLICIT none
      REAL lambda
      isfir = (lambda .GE. 20e-6) ! microns
      RETURN 
      END
      LOGICAL FUNCTION isfarir(lambda)
      IMPLICIT none
      REAL lambda
      isfarir = (lambda .GE. 200e-6) ! microns
      RETURN 
      END

      REAL(KIND=8) FUNCTION integral_flat(x, nx, dx)
      IMPLICIT none
      REAL(KIND=8) sum
      INTEGER nx
      REAL dx, x(nx)
      INTEGER i
      sum = 0.0
      DO i = 1, nx
        sum = sum + x(i)
      ENDDO
      integral_flat = sum*dx
      RETURN
      END

      REAL(KIND=8) FUNCTION integral_trapezoid(x, nx, dx)
      IMPLICIT none
      REAL(KIND=8) sum
      INTEGER nx
      REAL dx, x(nx)
      INTEGER i
      sum = 0.0

      DO i = 2, nx-1
        sum = sum + x(i)
      ENDDO
      sum = sum + x(1)/2
      sum = sum + x(nx)/2
      integral_trapezoid = sum*dx
      RETURN
      END
