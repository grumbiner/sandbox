      PROGRAM getfreqs
      IMPLICIT none
C Extract a set of frequencies from a data set.  Specify by way of Doodson-like
C   numbers
C Variant for working with daily average, per IERS data and the spliced text files

C Math
      DOUBLE PRECISION pi

! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001

      DOUBLE PRECISION omega_day, omega_month
      DOUBLE PRECISION omega_e_sidereal, omega_e_anom, omega_e_tropical
      PARAMETER (omega_day = 1./1.0)
      PARAMETER (omega_e_sidereal  = 1./365.256363) ! sidereal year
      PARAMETER (omega_e_anom = 1./365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = 1./365.24190) !tropical year
      DOUBLE PRECISION  omega_prec
      PARAMETER (omega_prec = omega_e_sidereal / 20940. )

      DOUBLE PRECISION omega_j, omega_s, omega_v
      DOUBLE PRECISION omega_n, omega_u, omega_mars 
      PARAMETER (omega_v    = 1./224.701)
      PARAMETER (omega_mars = 1./ 686.980)  
      PARAMETER (omega_j    = 1./4332.589)
      PARAMETER (omega_s    = 1./10759.22)
      PARAMETER (omega_u    = 1./30685.4)
      PARAMETER (omega_n    = 1./60189.0)

      DOUBLE PRECISION  omega_moon, omega_perigee, omega_node
      PARAMETER (omega_moon    = 1./27.3217)
      PARAMETER (omega_perigee = 1./365.256363/8.85)
      PARAMETER (omega_node    = 1./365.256363/18.613)

C Time series
      !68668 = Jan 1 1962 to Dec 31 2008, 6 hourly
      REAL :: var(68668)
      REAL :: date(68668)
      DOUBLE PRECISION, ALLOCATABLE :: omega(:), a(:), b(:)
      DOUBLE PRECISION, ALLOCATABLE :: freq(:), amplitude(:), phase(:)
      INTEGER, ALLOCATABLE :: day(:), month(:)
      INTEGER, ALLOCATABLE :: anom(:), sidereal(:)
      INTEGER, ALLOCATABLE :: venus(:), mars(:), jupiter(:), saturn(:)

C Misc
      INTEGER i, todo, iindex, jindex, tmpj
      INTEGER nx, ny, npts
      DOUBLE PRECISION dt
      DOUBLE PRECISION intercept, slope, correl
      REAL mean
      CHARACTER*70 fname, fname2, fname3


C ------------- Start executable section ------------------------------
      dt = 0.25
      npts = 68668
      pi = DABS(DACOS(-1.D0))
      OPEN (10,FILE="doodson", FORM="FORMATTED")
      READ (10,*) todo
      ALLOCATE (day(todo), month(todo), anom(todo), sidereal(todo))
      ALLOCATE (venus(todo), jupiter(todo), saturn(todo) )
      ALLOCATE (omega(todo), a(todo), b(todo))
      ALLOCATE (freq(todo), amplitude(todo), phase(todo) )

      DO i = 1, todo
        READ (10,*) day(i), month(i), sidereal(i), anom(i), 
     1                        venus(i), jupiter(i), saturn(i)
        omega(i) = day(i)         * omega_day +
     1             month(i)       * omega_moon +
     2             sidereal(i)    * omega_e_sidereal +
     2             anom(i)        * omega_e_anom +
     3             venus(i)       * omega_v +
     4             jupiter(i)     * omega_j + 
     5             saturn(i)      * omega_s
        !PRINT *,'analysing ',omega(i), 1./omega(i)
      ENDDO
      CLOSE (10)
      freq = omega*dt*2.*pi

C -------------- Get data to analyze
      READ (*,*) fname
      OPEN (12, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      
      DO i = 1, npts
        READ(12, *) date(i), var(i)
      ENDDO
      CLOSE (12)

C----------------------------------------------------------------------------
      ! Extract the mean and linear trend
      !PRINT *,'extracting linear trend'
      CALL linear_extract(date, var, npts, "var")

      !PRINT *,'extracting specified harmonics'
      CALL harmonic_extract(var, npts, freq, a, b, todo, dt)

      !Now write back out:
      DO i = 1, npts
        WRITE (13,9001) date(i), var(i)
      ENDDO
 9001 FORMAT(F8.2,"	",E13.6)

      END
