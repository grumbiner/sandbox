      PROGRAM testing
      IMPLICIT none
      DOUBLE PRECISION PI
!      PARAMETER (PI = 3.141592654)
      PARAMETER (PI = 3.14159265358979323846 )
      INTEGER i, j, n, m, nfreq
      PARAMETER (n = 75972) !Reanalysis to dec 31 1999
      PARAMETER (nfreq = 2000)
      DOUBLE PRECISION omega_e_sid, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_v, omega_j, omega_s, omega_u, omega_n
      DOUBLE PRECISION omega_mars
! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001
      REAL stepsize
      PARAMETER (stepsize = 0.25)
      PARAMETER (omega_e_sid  = stepsize*2.*PI/365.256363) ! sidereal year
      PARAMETER (omega_e_anom = stepsize*2.*PI/365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = stepsize*2.*PI/365.24190) !tropical year
      PARAMETER (omega_mars = stepsize*2.*PI/ 686.980)
      PARAMETER (omega_v = stepsize*2.*PI/224.701)
      PARAMETER (omega_j = stepsize*2.*PI/4332.589)
      PARAMETER (omega_s = stepsize*2.*PI/10759.22)
      PARAMETER (omega_u = stepsize*2.*PI/30685.4)
      PARAMETER (omega_n = stepsize*2.*PI/60189.)
      REAL omega_prec
      PARAMETER (omega_prec = omega_e_sid / 20940. )
      REAL omega_moon, omega_perigee, omega_node
      PARAMETER (omega_moon    = stepsize*2.*PI/27.32)
      PARAMETER (omega_perigee = stepsize*2.*PI/365.256/8.85)
      PARAMETER (omega_node    = stepsize*2.*PI/365.256/18.6)
      REAL omega_day
      PARAMETER (omega_day     = stepsize*2.*PI/1)
      DOUBLE PRECISION jd(n), x(n), y(n), utc(n), lod(n), r(n), rdot(n)
      DOUBLE PRECISION omega(0:nfreq-1), a(0:nfreq-1), b(0:nfreq-1)
      INTEGER todo
      INTEGER day(nfreq), month(nfreq), year(nfreq), venus(nfreq)
      INTEGER jupiter(nfreq)

!Want to go to double precision linpack call
      OPEN (10,FILE="doodson", FORM="FORMATTED")
      READ (10,*) todo
      DO i = 1, todo
        READ (10,*) day(i), month(i), year(i), venus(i), jupiter(i)
        !  PRINT *, i, day(i), month(i), year(i), venus(i), jupiter(i)
        !  PRINT *, omega_day, omega_moon, omega_e_anom, omega_v, omega_j
        omega(i) = day(i)     * omega_day +
     1             month(i)   * omega_moon +
     2             year(i)    * omega_e_anom +
     3             venus(i)   * omega_v +
     4             jupiter(i) * omega_j
        !IF (omega(i) .NE. 0.0) THEN
        !  PRINT *,i,2.*PI*stepsize/omega(i), omega(i)
        !ELSE
        !  PRINT *,i,"mean"
        !ENDIF
       
      ENDDO
      CLOSE(10)

      m = todo
C now get data and beat tar out of it
      OPEN (11, FILE="dist", FORM="FORMATTED")
      DO i = 1, n
        READ (11,*) r(i)
        !IF (i .LT. 100) PRINT *, r(i)
      ENDDO
      !CALL extract(r, n, omega, a, b, m, stepsize)

      !DO i = 1, n
      !  r(i) = 1/r(i)/r(i)
      !ENDDO
      CALL extract(r, n, omega, a, b, m, stepsize)
      DO i = 1, n
        PRINT *,r(i)
      ENDDO

      
      STOP
      END
