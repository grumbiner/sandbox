C***********************************************************----------!!
      SUBROUTINE INIT(UC, VC, UT, VT, SS, SD, WE, H, nx, ny,
     1                AHM, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                DELX, DELY, DELT,
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,
     4                XMIN, XMAX, YMIN, YMAX, QFMAX, QFREF, QM,
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )
C     INITIALIZE THE DATA ARRAYS AND CONSTANTS

      IMPLICIT none
      REAL SECPYR
      PARAMETER (SECPYR = 3.1556908E7)
      REAL PI
      PARAMETER (PI = 3.141592654)

      INTEGER nx, ny
      REAL UC(nx, ny), VC(nx, ny), UT(nx, ny), VT(nx, ny)
      REAL SS(nx, ny), SD(nx, ny)
      REAL WE(nx, ny), H(nx, ny)

      REAL AHM, AVM, AHS, AVS
      REAL SREF, SDREF, SSREF, RHOREF
      REAL G, F
      REAL DELX, DELY, DELT
      INTEGER NOUT, NFLOUT, NTOT, TAV
      REAL SCRIT

C     PARAMETERS FOR BUOYANCY FORCING
      INTEGER XMIN, XMAX, YMIN, YMAX
      REAL QFMAX, QFREF, QM
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY

C     PARMS FOR BAROTROPIC FLOW
      REAL BETA

      REAL RHO, VALUE, A, B
      CHARACTER*60 FNAME
      INTEGER I, J, DUMMY
      LOGICAL YES

C***********************************************************----------**
      OPEN (11, FILE='MODEL9IN', FORM='FORMATTED', STATUS='OLD')
C***********************************************************----------**
C     GET ARRAY DATA:

      PRINT *,'WOULD YOU LIKE TO USE OUTPUT FROM AN OLD RUN?'
      IF (YES(.FALSE.)) THEN
        PRINT *,'WHAT IS THE FILE NAME FOR UC?'
        READ (11,9001) FNAME
        OPEN (12, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR VC?'
        READ (11,9001) FNAME
        OPEN (13, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'UT   '
        READ (11,9001) FNAME
        OPEN (14, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'VT   '
        READ (11,9001) FNAME
        OPEN (15, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR SS?'
        READ (11,9001) FNAME
        OPEN (16, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR SD?'
        READ (11,9001) FNAME
        OPEN (17, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')

        PRINT *,'AT WHAT TIME STEP DO YOU WANT THE DATA?'
        READ (11,9003) DUMMY
        DO 1000 I = 1, DUMMY
          READ (12) UC
          READ (13) VC
          READ (14) UT
          READ (15) VT
          READ (16) SS
          READ (17) SD
 1000   CONTINUE

       ELSE
        WRITE (*,9011) 'UC   '
        READ (11,9002) VALUE
        CALL ARSET(UC, nx, ny, VALUE)
        WRITE (*,9011) 'VC   '
        READ (11,9002) VALUE
        CALL ARSET(VC, nx, ny, VALUE)
        WRITE (*,9011) 'UT   '
        READ (11,9002) VALUE
        CALL ARSET(UT, nx, ny, VALUE)
        WRITE (*,9011) 'VT   '
        READ (11,9002) VALUE
        CALL ARSET(VT, nx, ny, VALUE)
        WRITE (*,9011) 'SS   '
        READ (11,9002) VALUE
        CALL ARSET(SS, nx, ny, VALUE)
        WRITE (*,9011) 'SD   '
        READ (11,9002) VALUE
        CALL ARSET(SD, nx, ny, VALUE)
      ENDIF

      WRITE (*,9011) 'WE   '
      READ (11,9002) VALUE
      PRINT *,'WHAT IS THE WAVE LENGTH IN X?'
      READ (11,9002) A
      PRINT *,'WHAT IS THE WAVE LENGTH IN Y?'
      READ (11,9002) B
      DO 7000 J = 0, ny-1
        DO 7010 I = 0, nx-1
          WE(I+1,J+1) = VALUE*SIN(2.*PI*J/B)
 7010   CONTINUE
 7000 CONTINUE

      WRITE (*,9011) 'H    '
      READ (11,9002) VALUE
      CALL ARSET(H, nx, ny, VALUE)

 9011 FORMAT (' WHAT IS THE VALUE OF ',A5,'?')

C***********************************************************----------**
C        FORCING
        READ (11,9003) XMIN
        READ (11,9003) XMAX
        READ (11,9003) YMIN
        READ (11,9003) YMAX
        READ (11,9002) QFMAX
        READ (11,9002) QFREF
        READ (11,9002) QM
        READ (11,9003) STRSPR
        READ (11,9003) STRSUM
        READ (11,9003) STRFLL
        READ (11,9003) STRWIN
C        OUTPUT
        READ (11,9003) NOUT
        READ (11,9003) NFLOUT
        READ (11,9003) NTOT
        READ (11,9002) SCRIT
        READ (11,9003) TAV
C        GRID
        READ (11,9002) DELX
        READ (11,9002) DELY
        READ (11,9002) DELT
C        BOUNDARY CONDITION INFO.
        READ (11,9002) SREF
        READ (11,9002) SDREF
        READ (11,9002) SSREF
C        DIFFUSION
        READ (11,9002) AHM
        READ (11,9002) AVM
        READ (11,9002) AHS
        READ (11,9002) AVS
C        PHYSICAL CONSTANTS
        READ (11,9002) F
        READ (11,9002) G
C        TERMS FOR BAROTROPIC SOLUTION.
        READ (11,9002) BETA

C     COMPUTED PARAMETERS:
      RHOREF = RHO(SREF, 0.0, 0.)
      LOY    = INT(SECPYR/DELT)

C     RESCALE INPUT VALUES OF QFMAX (M/DAY), QFREF,QM (M/SEASON) TO
C       KG/M**2/S.  MUST DIVIDE BY H IN UVEXT.  8-10-88.
        QFMAX = QFMAX / 2.88E3
        QFREF = QFREF * (30./FLOAT(STRSPR-STRWIN)/DELT)
        QM    = QM    * (30./FLOAT(STRFLL-STRSUM)/DELT)
C        FORCING
        WRITE (*,9003) XMIN
        WRITE (*,9003) XMAX
        WRITE (*,9003) YMIN
        WRITE (*,9003) YMAX
        WRITE (*,9002) QFMAX
        WRITE (*,9002) QFREF
        WRITE (*,9002) QM
        WRITE (*,9003) STRSPR
        WRITE (*,9003) STRSUM
        WRITE (*,9003) STRFLL
        WRITE (*,9003) STRWIN
C        OUTPUT
        WRITE (*,9003) NOUT
        WRITE (*,9003) NFLOUT
        WRITE (*,9003) NTOT
        WRITE (*,9002) SCRIT
        WRITE (*,9003) TAV
C        GRID
        WRITE (*,9002) DELX
        WRITE (*,9002) DELY
        WRITE (*,9002) DELT
C        BOUNDARY CONDITION INFO.
        WRITE (*,9002) SREF
        WRITE (*,9002) SDREF
        WRITE (*,9002) SSREF
C        DIFFUSION
        WRITE (*,9002) AHM
        WRITE (*,9002) AVM
        WRITE (*,9002) AHS
        WRITE (*,9002) AVS
C        PHYSICAL CONSTANTS
        WRITE (*,9002) F
        WRITE (*,9002) G
C        TERMS FOR BAROTROPIC SOLUTION.
        WRITE (*,9002) BETA
C        COMPUTED CONSTANTS
        WRITE (*,9003) LOY

 9001 FORMAT (A60)

 9002 FORMAT (BN, E13.6)

 9003 FORMAT (BN, I8)

      RETURN
      END
