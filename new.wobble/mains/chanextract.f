      PROGRAM chanextract
      IMPLICIT none
C Extract a set of frequencies from a data set.  Specify by way of Doodson-like
C   numbers
C Always analyze the Chandler frequency

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

      DOUBLE PRECISION omega_chandler
      PARAMETER (omega_chandler = 1./433.0)

C Time series
      DOUBLE PRECISION, ALLOCATABLE :: x(:)
      DOUBLE PRECISION, ALLOCATABLE :: omega(:), a(:), b(:)
      DOUBLE PRECISION, ALLOCATABLE :: freq(:), amplitude(:), phase(:)
      INTEGER, ALLOCATABLE :: day(:), month(:)
      INTEGER, ALLOCATABLE :: anom(:), sidereal(:)
      INTEGER, ALLOCATABLE :: venus(:), jupiter(:), saturn(:)
      INTEGER, ALLOCATABLE :: node(:), perigee(:)

C Misc
      INTEGER i, todo
      INTEGER npts
      DOUBLE PRECISION dt, sum, mean
      CHARACTER*70 fname
      DOUBLE PRECISION intercept, slope, correl
      DOUBLE PRECISION, ALLOCATABLE :: y(:)


C ------------- Start executable section ------------------------------
      !PRINT *,'pi difference ',PI - DABS(DACOS(-1.D0))
      pi = DABS(DACOS(-1.D0))
      OPEN (10,FILE="doodson", FORM="FORMATTED")
      READ (10,*) todo
!These are specified by doodson-like numbers:
      ALLOCATE (day(todo), month(todo), anom(todo), sidereal(todo))
      ALLOCATE (venus(todo), jupiter(todo), saturn(todo) )
      ALLOCATE (node(todo), perigee(todo))

!Add 1 for the chandler frequency:
      ALLOCATE (omega(todo+1), a(todo+1), b(todo+1))
      ALLOCATE (freq(todo+1), amplitude(todo+1), phase(todo+1) )

      DO i = 1, todo
        READ (10,*) day(i), month(i), sidereal(i), anom(i), 
     1                        venus(i), jupiter(i), saturn(i)
        omega(i) = day(i)     * omega_day +
     1             month(i)   * omega_moon +
     2             sidereal(i)    * omega_e_sidereal +
     3             anom(i)    * omega_e_anom +
     4             venus(i)   * omega_v +
     5             jupiter(i) * omega_j + 
     6             saturn(i) * omega_s
      ENDDO
      CLOSE (10)
      omega(todo+1) = omega_chandler

C -------------- Get data to analyze
      READ (*,*) fname
      OPEN (11, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      READ (11,*) dt
      READ (11,*) npts
      READ (11,*) fname
      ALLOCATE (x(npts))
      ALLOCATE (y(npts))
      CLOSE (11)

      !Read in data and ensure mean is zero:
      OPEN (12, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      sum = 0.D0
      DO i = 1, npts
        READ (12, *) x(i)
        sum = sum + x(i)
        y(i) = i
      ENDDO
      CLOSE (12)
      mean = sum / npts
      PRINT *,'mean was ',mean
      !x = x - mean
      ! Extract the linear trend
      CALL fit(y, x, npts, intercept, slope, correl)
      PRINT *,'regression line is a + bx ', intercept, slope, 
     1              ' correlation ', correl
      DO i = 1, npts
        x(i) = x(i) - intercept - slope*y(i)
      ENDDO
    

C----------------------------------------------------------------------------
      freq = omega*dt*2.*pi
      CALL harmrm(x, npts, freq, a, b, todo+1)
      DO i = 1, todo+1
        WRITE (*, *) freq(i)/2./pi/dt,",",
     1               SQRT(a(i)*a(i)+b(i)*b(i))*1d6, ",",
     2               ATAN2(b(i),a(i))*180./pi,",",  2.D0*pi/freq(i)*dt
        amplitude(i) = SQRT(a(i)*a(i)+b(i)*b(i))
        phase(i)     = ATAN2(b(i),a(i))*180./pi
      ENDDO

      CALL extract(x, npts, freq, a, b, todo+1, dt)
      DO i = 1, npts
        WRITE (*,*) x(i)
      ENDDO

      END
