C***********************************************************----------!!
      SUBROUTINE init(ub, vb, ut, vt, ss, sd, we, h, nx, ny,
     1                ahm, avm, ahs, avs,
     2                sdref, ssref, rhoref, g, f,
     3                delx, dely, delt, nout, nflout, ntot,
     4                xmin, xmax, ymin, ymax, qfmax, qfref, qm,
     5                strspr, strsum, strfll, strwin, loy, beta    )
C     Initialize the data arrays and constants

      REAL secpyr
      PARAMETER (secpyr = 3.1556908E7)

      INTEGER nx, ny
      REAL ub(nx, ny), vb(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny)
      REAL we(nx, ny), h(nx, ny)

      REAL ahm, avm, ahs, avs
      REAL sref, sdref, ssref, rhoref
      REAL g, f
      REAL delx, dely, delt
      INTEGER nout, nflout, ntot

C     Parameters for buoyancy forcing
      INTEGER xmin, xmax, ymin, ymax
      REAL qfmax, qfref, qm
      INTEGER strspr, strsum, strfll, strwin, loy
C     Parms for barotropic flow
      REAL beta

      REAL rho, value
      CHARACTER*60 fname
      INTEGER i, j, dummy
      LOGICAL yes

C***********************************************************----------**
C     Get array data:

      PRINT *,'Would you like to use output from an old run?'
      IF (yes(.FALSE.)) THEN
        PRINT *,'What is the file name for ub?'
        READ (*,9001) fname
        OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for vb?'
        READ (*,9001) fname
        OPEN (13, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'ut   '
        READ (*,9001) fname
        OPEN (14, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'vt   '
        READ (*,9001) fname
        OPEN (15, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for ss?'
        READ (*,9001) fname
        OPEN (16, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for sd?'
        READ (*,9001) fname
        OPEN (17, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')

        PRINT *,'At what time step do you want the data?'
        READ (*,9003) dummy
        DO 1000 i = 1, dummy
          READ (12) ub
          READ (13) vb
          READ (14) ut
          READ (15) vt
          READ (16) ss
          READ (17) sd
 1000   CONTINUE

       ELSE
        WRITE (*,9011) 'ub   '
        READ (*,9002) value
        CALL arset(ub, nx, ny, value)
        WRITE (*,9011) 'vb   '
        READ (*,9002) value
        CALL arset(vb, nx, ny, value)
        WRITE (*,9011) 'ut   '
        READ (*,9002) value
        CALL arset(ut, nx, ny, value)
        WRITE (*,9011) 'vt   '
        READ (*,9002) value
        CALL arset(vt, nx, ny, value)
        WRITE (*,9011) 'ss   '
        READ (*,9002) value
        CALL arset(ss, nx, ny, value)
        WRITE (*,9011) 'sd   '
        READ (*,9002) value
        CALL arset(sd, nx, ny, value)
      ENDIF

      PRINT *,'What is the name of the ekman pumping file?'
        READ (*,9001) fname
        CALL read2(we, nx, ny, 2, fname, .TRUE., .TRUE.)
CU    WRITE (*,9011) 'we   '
CU    READ (*,9002) value
CU    CALL arset(we, nx, ny, value)

      WRITE (*,9011) 'h    '
      READ (*,9002) value
      CALL arset(h, nx, ny, value)

 9011 FORMAT (' What is the value of ',A5,'?')

C***********************************************************----------**
      PRINT *,'What is the name of the scalar parameter file?'
      READ (*,9001) fname
      WRITE (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
C        Forcing
        READ (11,9003) xmin
        READ (11,9003) xmax
        READ (11,9003) ymin
        READ (11,9003) ymax
        READ (11,9002) qfmax
        READ (11,9002) qfref
        READ (11,9002) qm
        READ (11,9003) strspr
        READ (11,9003) strsum
        READ (11,9003) strfll
        READ (11,9003) strwin
C        output
        READ (11,9003) nout
        READ (11,9003) nflout
        READ (11,9003) ntot
C        grid
        READ (11,9002) delx
        READ (11,9002) dely
        READ (11,9002) delt
C        boundary condition info.
        READ (11,9002) sref
        READ (11,9002) sdref
        READ (11,9002) ssref
C        diffusion
        READ (11,9002) ahm
        READ (11,9002) avm
        READ (11,9002) ahs
        READ (11,9002) avs
C        physical constants
        READ (11,9002) f
        READ (11,9002) g
C        terms for barotropic solution.
        READ (11,9002) beta
      CLOSE (11, STATUS='KEEP')

C     Computed parameters:
      rhoref = rho(sref, 0.0, 0.)
      loy    = INT(secpyr/delt)

C     Rescale input values of qfmax (m/day), qfref,qm (m/season) to
C       kg/m**2/s.  Must divide by h in uvext.  8-10-88.
        qfmax = qfmax / 2.88E3
        qfref = qfref * (30./FLOAT(strspr-strwin)/delt)
        qm    = qm    * (30./FLOAT(strfll-strsum)/delt)
C        Forcing
        WRITE (*,9003) xmin
        WRITE (*,9003) xmax
        WRITE (*,9003) ymin
        WRITE (*,9003) ymax
        WRITE (*,9002) qfmax
        WRITE (*,9002) qfref
        WRITE (*,9002) qm
        WRITE (*,9003) strspr
        WRITE (*,9003) strsum
        WRITE (*,9003) strfll
        WRITE (*,9003) strwin
C        output
        WRITE (*,9003) nout
        WRITE (*,9003) nflout
        WRITE (*,9003) ntot
C        grid
        WRITE (*,9002) delx
        WRITE (*,9002) dely
        WRITE (*,9002) delt
C        boundary condition info.
        WRITE (*,9002) sref
        WRITE (*,9002) sdref
        WRITE (*,9002) ssref
C        diffusion
        WRITE (*,9002) ahm
        WRITE (*,9002) avm
        WRITE (*,9002) ahs
        WRITE (*,9002) avs
C        physical constants
        WRITE (*,9002) f
        WRITE (*,9002) g
C        terms for barotropic solution.
        WRITE (*,9002) beta
C        computed constants
        WRITE (*,9003) loy

 9001 FORMAT (A60)

 9002 FORMAT (BN, E13.6)

 9003 FORMAT (BN, I8)

      RETURN
      END
