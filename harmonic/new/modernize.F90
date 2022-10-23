MODULE astronomy
  IMPLICIT none

! Declare Types of Interest ----------------------------------------------------------------
  TYPE doodson_number
    INTEGER d(6)
    !day, month, year, 8.86 lunar, 18.6 lunar, 20ky precession
  END TYPE doodson_number
  !function doodson_to (input 6 integers), _from (return 6 integers)

  ! give the 7 integers for wobble
  TYPE wobble_number
    INTEGER w(7)
  END TYPE wobble_number

! Declare Values of Interest ----------------------------------------------------------------
  REAL(8) :: PI, eccen
      PARAMETER (PI = 3.14159265358979323846 )
      PARAMETER (eccen = 1.6716738e-2)
      
  REAL(8) :: omega_chandler
      PARAMETER (omega_chandler = 2.*PI/433.0) ! Chandler period (cpd)
      
! Earth orbital:
  REAL(8) :: omega_day, omega_e_anomalistic, omega_e_sidereal, omega_e_tropical, omega_prec
      PARAMETER (omega_day           = 2.*PI/1.0       ) ! day (cpd)
      PARAMETER (omega_e_anomalistic = 2.*PI/365.259635) ! anomalistic year (cpd)
      PARAMETER (omega_e_sidereal    = 2.*PI/365.256363) ! sidereal year (cpd)
      PARAMETER (omega_e_tropical    = 2.*PI/365.24190 ) ! tropical year (cpd)
      PARAMETER (omega_prec = omega_e_sidereal / 20940.) ! Precession

!Moon:
    REAL(8) :: omega_moon, omega_perigee, omega_node
      PARAMETER (omega_moon    = 2.*PI/27.3217)          !sidereal month
      PARAMETER (omega_perigee = 2.*PI/ 8.85 /365.24190)
      PARAMETER (omega_node    = 2.*PI/18.613/365.256)

!Other Planets:
    REAL(8) :: omega_venus, omega_mars, omega_jupiter, omega_saturn
    REAL(8) :: omega_uranus, omega_neptune
      PARAMETER (omega_venus   = 2.*PI/  224.701) !cpd
      PARAMETER (omega_mars    = 2.*PI/  686.980)
      PARAMETER (omega_jupiter = 2.*PI/ 4332.589)
      PARAMETER (omega_saturn  = 2.*PI/10759.22 )
      PARAMETER (omega_uranus  = 2.*PI/30685.4  )
      PARAMETER (omega_neptune = 2.*PI/60189.   )

!CONTAINS

END MODULE astronomy 
!------------------------------------------------------------------

!-----------------------------------------------------------------------
MODULE harmonics 
  IMPLICIT none

!make a type
  TYPE harmonic_set
    REAL(8), allocatable :: freqs(:), acos(:), asin(:)
    INTEGER :: nfreqs
  END TYPE

!CONTAINS
END MODULE harmonics 
!------------------------------------------------------------------

!-------------------------------------------------------------------
MODULE time_series 
  IMPLICIT none
!RG should inherit from mvector

!make a type
  TYPE series
    INTEGER :: nx
    REAL    :: dt
    REAL, allocatable :: x(:)
  END TYPE
!filters (hanning, cosine bell, ...)
!25 filter for tides, 25,24,24 for tides
!extract trends -- mvector


!CONTAINS
END MODULE time_series 
!----------------------------------------------------------------

!----------------------------------------------------------
MODULE irregular_time_series 
  USE time_series
  !inherit for irregular time spacing

  IMPLICIT none

  TYPE irregular
    REAL, allocatable :: tau(:)
  END TYPE

!CONTAINS
END MODULE irregular_time_series 
!------------------------------------------------------


!=======================================================================================
!=======================================================================================
SUBROUTINE demod(x, harmonics)
  USE harmonic
  USE time_series
  IMPLICIT none
  TYPE(series)  :: x
  TYPE(harmonic_set) :: harmonics
  INTEGER i, j

  DO i = 1, x%nx
    DO j = 1, harmonics%nfreqs
      x%x(i) = x%x(i) - harmonics%acos(j)*cos(i*harmonics%freqs(j))     & 
                      - harmonics%asin(j)*sin(i*harmonics%freqs(j))
    ENDDO
  ENDDO

  RETURN
END SUBROUTINE demod
!=======================================================================================
