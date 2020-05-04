      PROGRAM abledo_check
      implicit none

C     Serve as a front end to the 3 layer thermodynamic code of
C     Winton, 2000.

C     From the meteorological model:
      INTEGER nx, ny
      PARAMETER (nx = 216)
      PARAMETER (ny =  97)
      REAL swdown(nx, ny), swup(nx, ny)
      REAL lwdown(nx, ny), lwup(nx, ny)
      REAL prate(nx, ny)  ! kg/m^2/s
      REAL shtfl(nx, ny), lhtfl(nx, ny)  !net sensible and latent heat fluxes
C     Meteorology a local flux computation would want:
C       Sensible heat:
      REAL ugrd(nx, ny), vgrd(nx, ny), t2m(nx, ny)
C       Latent heat:
      REAL spfh(nx, ny), press(nx, ny)
C       Radiation
      REAL albedo(nx, ny)
C       Boundary Layer depth
      REAL hpbl(nx, ny)
C       Land mask, 1 = land
      REAL land(nx, ny)


C     For the ice code:
C     Ice variables
      REAL hs(nx, ny), hi(nx, ny), ts(nx, ny), t1(nx, ny), t2(nx, ny)
      REAL tmelt, bmelt
C     Forcing variables
      REAL hf, sol, hfd, fb, snow
C     Generic
      REAL dt
C     physics
      REAL thinice, rhoi, lf
      PARAMETER (rhoi = 905) !winton value
      PARAMETER (lf   = 3.34e5) !
      PARAMETER (thinice = 0.01)

C     Required external (to ice code) function:
      REAL dfluxdtemp

C     Set up some parameters
      PARAMETER (dt = 3600.*6) !6 hourly info from flux file 
      PARAMETER (fb = 2)       !canonical arctic ocean-ice flux
      
      INTEGER yearmax, steps(12),mon(12)
      INTEGER i, j, tau, month, years
      REAL sum_rad(nx, ny), sum_sen(nx, ny)
      CHARACTER*80 fname
      DATA steps/124,120,121,124,116,124,120,124,120,124,122,120/
      DATA mon  /10,  11, 12, 01, 02, 03, 04, 05, 06, 07, 08, 09/
      PARAMETER (yearmax = 1)

      INTEGER count(nx, ny)
      REAL imax(nx, ny), smax(nx, ny)

      INTEGER first(nx, ny), last(nx, ny), days(nx, ny)
      INTEGER dayno

C     No prior ice information, set up cold thin ice start:
      hs =  0.0
      hi =  0.0
      t1 = -1.8
      t2 = -1.8
      ts =  0.0

      OPEN(10, FILE="land", FORM="unformatted")
      READ(10) land
      CLOSE(10)
      OPEN(10, FILE="first", FORM="unformatted")
      READ(10) first
      CLOSE(10)
      OPEN(10, FILE="last", FORM="unformatted")
      READ(10) last
      CLOSE(10)
      OPEN(10, FILE="days", FORM="unformatted")
      READ(10) days
      CLOSE(10)

      DO years = 1, yearmax
        dayno = 0
      DO month = 1, 9  ! run through end of June
C     Get meteorological input
      IF (mon(month) .LT. 10) THEN
        WRITE(fname,9001) mon(month)
      ELSE
        WRITE(fname,9002) mon(month)
      ENDIF
 9001 FORMAT("metinput.0",I1)
 9002 FORMAT("metinput.",I2)
      OPEN(10, FILE=fname, FORM="unformatted")

      DO tau = 1, steps(month)
        IF ( mod(tau-1, 4) .EQ. 0) dayno = dayno + 1

CD        PRINT *,'month, tau = ',mon(month), tau
        READ (10) shtfl
        READ (10) lhtfl
        READ (10) t2m
        READ (10) lwdown
        READ (10) lwup
        READ (10) swup
        READ (10) swdown
        READ (10) prate
        READ (10) ugrd
        READ (10) vgrd
        READ (10) spfh
        READ (10) press
        READ (10) hpbl
        READ (10) albedo
        DO j = 1, ny
        DO i = 1, nx
C         Skip land points and not-yet iced points:
          IF (land(i,j) .GT. 128 ) CYCLE
          IF (dayno .GE. first(i,j) .AND. dayno .LE. last(i,j) 
     1          .AND. albedo(i,j) .NE. 0.0 ) THEN
            WRITE(*,9000) i,j,dayno,albedo(i,j),
     1              100.*swup(i,j)/swdown(i,j),swup(i,j),swdown(i,j)
          ENDIF
        ENDDO !i
        ENDDO !j

      ENDDO !tau
        CLOSE(10)
      ENDDO !months
      ENDDO !years
 9000 FORMAT (3I4,1x,F5.2,1x,F5.2,2x,2F7.1)

      STOP
      END
