      PROGRAM getfreqs
      IMPLICIT none
C Extract a set of frequencies from a data set.  Specify by way of Doodson-like
C   numbers

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
      REAL :: x(192, 32, 68668)
      REAL :: tmp(192, 94)
      REAL :: z(68668)
      REAL, ALLOCATABLE :: y(:)
      DOUBLE PRECISION, ALLOCATABLE :: omega(:), a(:), b(:)
      DOUBLE PRECISION, ALLOCATABLE :: freq(:), amplitude(:), phase(:)
      INTEGER, ALLOCATABLE :: day(:), month(:)
      INTEGER, ALLOCATABLE :: anom(:), sidereal(:)
      INTEGER, ALLOCATABLE :: venus(:), jupiter(:), saturn(:)

C Misc
      INTEGER i, todo, iindex, jindex, tmpj
      INTEGER nx, ny, npts
      DOUBLE PRECISION dt
      DOUBLE PRECISION intercept, slope, correl
      REAL mean
      CHARACTER*70 fname, fname2, fname3


C ------------- Start executable section ------------------------------
      !PRINT *,'pi difference ',PI - DABS(DACOS(-1.D0))
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
        !DEBUF  PRINT *,'analysing ',omega(i), 2.*pi/omega(i)
      ENDDO
      CLOSE (10)

C -------------- Get data to analyze
      READ (*,*) fname
      OPEN (11, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      READ (11,*) dt
      WRITE (*,*) dt
      READ (11,*) npts
      WRITE (*,*) npts
      READ (11,*) nx, ny
      WRITE (*,*) nx, ny
      READ (11,*) fname2
      WRITE (*,*) fname2
      CLOSE (11)
      ALLOCATE (y( npts))
      DO i = 1, npts
        y(i) = i
      ENDDO
      !DEBUG PRINT *,'npts = ',npts

      !Read in data 
      OPEN (12, FILE=fname2, FORM="UNFORMATTED", STATUS="OLD") 


C----------------------------------------------------------------------------
      ! Extract the linear trend
      ! Note that the splitting of 1-32, 33-64, 65-94 is a limit imposed 
      !      by compiler (2Gb max array size), not algorithmic
      DO jindex = 1, 94
        !DEBUG PRINT *,'jindex = ',jindex
        IF (jindex .EQ. 1) THEN
          DO i = 1, npts
            READ (12) tmp
            !DEBUG PRINT *,i,MAXVAL(tmp), MINVAL(tmp)
            x(:,:,i) = tmp(:,1:32)
          ENDDO
        ELSE IF (jindex .EQ. 33) THEN
          REWIND(12)
          DO i = 1, npts
            READ (12) tmp
            x(:,:,i) = tmp(:,33:64)
          ENDDO
        ELSE IF (jindex .EQ. 65) THEN
          REWIND(12)
          DO i = 1, npts
            READ (12) tmp
            x(:,1:30,i) = tmp(:,65:94)
          ENDDO
        ENDIF

        IF (jindex .LE. 32) THEN
          tmpj = jindex
        ELSE IF (jindex .LE. 64) THEN
          tmpj = jindex - 32
        ELSE
          tmpj = jindex - 64
        ENDIF
        !  End of annoying splitting of original data

        !DEBUG PRINT *,'tmpj = ',tmpj
      DO iindex = 1, 192
        !DEBUG PRINT *,'   iindex = ',iindex

      !Find and extract the best fit line:
        CALL demean(x(iindex, tmpj, :), mean, npts)
        CALL fit(y, x(iindex, tmpj, :) , npts, 
     1              intercept, slope, correl)
!        PRINT *,'regression line ',iindex, jindex, ' is a + bx ', 
!     1              intercept, slope, 
!     1              ' correlation ', correl
        WRITE (*,9002) iindex, jindex, mean, intercept, slope, correl
 9002   FORMAT ("i,j",I4,I3," mean ",F11.4," regress a+bx ",F7.3, E10.3, 
     1             "correlation ",F8.5)

        DO i = 1, npts
          x(iindex, tmpj, i) = x(iindex, tmpj, i) - intercept - 
     1            slope*y(i)
        ENDDO
    
        freq = omega*dt*2.*pi
        CALL harmrm(x(iindex, tmpj, :) , npts, freq, a, b, todo)
        DO i = 1, todo
          amplitude(i) = SQRT(a(i)*a(i)+b(i)*b(i))
          phase(i)     = ATAN2(b(i),a(i))*180./pi
          WRITE (*, *) iindex, jindex, freq(i)/2./pi/dt,",",
     1                 amplitude(i),",",
     2                 phase(i),",",  2.D0*pi/freq(i)*dt
        ENDDO

        CALL extract(x(iindex, tmpj, :) , npts, freq, a, b, todo, dt)
        !DEBUG PRINT *,'making file name'
        WRITE (fname3,9001) iindex, jindex
        !DEBUG PRINT *,'fname = ',fname3
 9001   FORMAT("extracted",I3,"_",I2)
        OPEN (90,FILE=fname3, FORM="UNFORMATTED")
          WRITE (90) x(iindex, tmpj, :)
        CLOSE(90)
        !DEBUG PRINT *,'done writing out ',iindex, jindex

      ENDDO
      ENDDO !jindex

      CLOSE (12)

      END
