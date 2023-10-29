
       IMPLICIT none

       REAL(SELECTED_REAL_KIND(14,300)) :: PI

!Mathematical Parameters:
           PARAMETER (PI = 3.14159265358979323846 )

! Reanalysis 1 Gaussian Grid
           INTEGER nx, ny
           PARAMETER (nx = 144)
           PARAMETER (ny =  73)

! Astronomical Periods -- cycles per day
! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001

       REAL(SELECTED_REAL_KIND(14,300)) :: omega_day, omega_moon, omega_perigee, omega_node
       REAL(SELECTED_REAL_KIND(14,300)) :: omega_v, omega_mars, omega_j, omega_s, omega_u, omega_n
       REAL(SELECTED_REAL_KIND(14,300)) :: omega_e_tropical, omega_e_sid, omega_e_anom, omega_prec

       PARAMETER (omega_day     = 2.*PI/1.)
  
       PARAMETER (omega_moon    = 2.*PI/27.3217)   ! Sidereal month
  
       PARAMETER (omega_v = 2.*PI/224.701)         ! Venus
  
       PARAMETER (omega_e_tropical = 2.*PI/365.24190) ! tropical year
       PARAMETER (omega_e_sid  = 2.*PI/365.256363)    ! sidereal year
       PARAMETER (omega_e_anom = 2.*PI/365.259635)    ! anomalistic year
  
       PARAMETER (omega_mars = 2.*PI/ 686.980)   ! Mars
  
       PARAMETER (omega_j = 2.*PI/4332.589)      ! Jupiter
       PARAMETER (omega_s = 2.*PI/10759.22)      ! Saturn
       PARAMETER (omega_u = 2.*PI/30685.4)       ! Uranus
       PARAMETER (omega_n = 2.*PI/60189.)        ! Neptune
  
       PARAMETER (omega_perigee = omega_e_tropical/8.85) ! Lunar Perigee
       PARAMETER (omega_node    = omega_e_tropical/18.6) ! Lunar Node
  
       PARAMETER (omega_prec = omega_e_sid / 20940. )    ! Earth precession

