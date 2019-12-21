PROGRAM alpha

  IMPLICIT none
  INTEGER, PARAMETER :: r15 = selected_real_kind(15,30)

!    inertia tensor perturbations
  REAL(r15), PARAMETER :: I(3,3) = 0._r15
!    spherical radii
  REAL(r15), PARAMETER :: rearth_sphere_arclength   =  6.371e6_r15
  REAL(r15), PARAMETER :: rearth_sphere_surfacearea =  6.371e6_r15
  REAL(r15), PARAMETER :: rearth_sphere_volume      =  6.371e6_r15
!    spheroids
  REAL(r15), PARAMETER :: wgs84_r = 6378.137e3_r15 , wgs84_flattening = 1._r15/299._r15
!    rotation, principle
  REAL(r15), PARAMETER :: lod = 86164.098903691_r15     ! seconds, IERS
  REAL(r15), PARAMETER :: omega_earth = 7.292115e-5_r15 ! rad/sec

!
! unit indices
!cm, m, km, micron, inch, foot, yard, mile, 
!g, kg, lb, amu
!s, min, hr, day, month (lunars), year(multiple)
!K, C, F
!Pa, mb, Bar, PSI, in Hg, torr
!J, erg, eV
!W, Hp
!Hz, cm^-1
!rad, degree, mol

  TYPE physical_parameter
    REAL(r15) :: value
    INTEGER   :: units
    INTEGER   :: precision ! relative? absolute?
  END TYPE
  INTEGER p,r

  TYPE(physical_parameter) :: x 
  TYPE(physical_parameter),PARAMETER :: y = physical_parameter(7.292115e-5_r15, 5, 5)

  x = physical_parameter(7.292115e-5_r15, 5, 5)
  r = 30
!  DO p = 1, 40
!      PRINT *,' p = ',p,selected_real_kind(p,r)
!  ENDDO

  PRINT *,x%value
  PRINT *,x%units
  PRINT *,x%precision

  PRINT *,y%value
  PRINT *,y%units
  PRINT *,y%precision

  PRINT *,lod
!  PRINT *,I

END
