      PROGRAM CSHELF
C     MODEL BOUYANCY (AND PERHAPS WIND) FORCED CONTINENTAL SHELF
C       MOTIONS AND CONDITIONS FOR THE POLAR REGIONS.

      INTEGER NX, NY
      PARAMETER (NX = 20, NY = 20)

C     DATA ARRAYS
      REAL UB(NX, NY), VB(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), TS(NX, NY), TD(NX, NY)
      REAL CS(NX, NY), CD(NX, NY)
      REAL QSS(NX, NY), QSD(NX, NY)
      REAL QTS(NX, NY), QTD(NX, NY)
      REAL QCS(NX, NY), QCD(NX, NY)
      REAL UWIND(NX, NY), VWIND(NX, NY), WE(NX, NY)
      REAL H(NX, NY), LNH(NX, NY)

C     PHYSICAL PARAMETERS
      REAL AHM, AVM, AHT, AHS, AVT, AVS
      REAL SDREF, SSREF, SREF, TDREF, TSREF, TREF, RHOREF
      REAL G, F, BETA
      INTEGER XMIN, XMAX, YMIN, YMAX, STRSPR, STRSUM, STRFLL, STRWIN
      REAL QSFMAX, QSFREF, QSM
      REAL DCADT, CSREF, CDREF, LAMBDA

C     NUMERICAL PARAMETERS
      REAL DELX, DELY, DELT, GAMMA
      REAL CONTOL
      INTEGER ITMAX, LNORM
      INTEGER NOUT, NFLOUT, NTOT, LOY

C     LOCAL VARIABLES
      INTEGER I
      CHARACTER*6 FNAME
CT    FOR TIMING:
CT    INTEGER BEFORE, AFTER, CLOCK
CT    INTEGER UVTIME, STTIME, QTIME

C     BEGIN EXECUTION

CT    INITIALIZE THE TIMING VARIABLES:
CT    UVTIME = 0
CT    STTIME = 0
CT    QTIME  = 0

      I = 0
C     INITIALIZE THE VARIABLES
      CALL INIT (UB, VB, UT, VT, SS, SD, TS, TD, CS, CD,
     1           UWIND, VWIND, WE, H, LNH, NX, NY,
     2           AHM, AVM, AHT, AVT, AHS, AVS,
     3           SREF, SDREF, SSREF, TREF, TDREF, TSREF, RHOREF,
     4           G, F,
     5           DELX, DELY, DELT, GAMMA, NOUT, NFLOUT, NTOT,
     6           XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     7           STRSPR, STRSUM, STRFLL, STRWIN, LOY,
     8           BETA, CONTOL, ITMAX, LNORM,
     9           CSREF, CDREF, LAMBDA, DCADT                      )

C     OPEN THE OUTPUT FILES
      CALL OUTSTR(UB, VB, UT, VT, SS, SD, TS, TD, CS, CD, NX, NY,
     1                      RHOREF, SREF, TREF, I)
CD    CALL OUTDAT(UB, VB, UT, VT, SS, SD, TS, TD, CS, CD, NX, NY,
CD   1                      RHOREF, SREF, TREF, I)

C     EXTRAPOLATION LOOP
      DO 1000 I = 1, NTOT
CD      PRINT *,'STEP ',I
CT      BEFORE = CLOCK(0)
        CALL UVEXT (UB, VB, SS, SD, TS, TD,
     1              UWIND, VWIND, H, LNH, NX, NY,
     2              SREF, TREF, RHOREF, G, F,
     3              AHM, AVM, DELX, DELY, DELT          )
CT      AFTER = CLOCK(0)
CT      UVTIME = UVTIME + AFTER - BEFORE

CT      BEFORE = CLOCK(0)
        CALL QSEXT (QSS, QSD, H, NX, NY, I, LOY,
     1              XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     2              STRSPR, STRSUM, STRFLL, STRWIN              )
        CALL QTEXT
CT      AFTER = CLOCK(0)
CT      QTIME = QTIME + AFTER - BEFORE

CD      PRINT *,'CALLING STEXT'
CT      BEFORE = CLOCK(0)
        CALL STEXT(UB, VB, UT, VT, WE, SS, SD, QSS, QSD, H, LNH,
     1             NX, NY, DELX, DELY, DELT, SDREF, SSREF, SREF,
     2             GAMMA, AHS, AVS, I)
CD      PRINT *,'FINISHED SEXT'
C       NOTE THAT THE EXTRAPOLATION FOR T IS NOT ACTUALLY CARRIED OUT,
C         IT IS CURRENTLY UNFORCED (3-29-88)
C       CALL STEXT(UB, VB, TS, TD, QTS, QTD, H, LNH, NX, NY,
C    1             DELX, DELY, DELT, TDREF, TSREF, TREF, GAMMA,
C    2             AHT, AVT, I)
CD      PRINT *,'FINISHED TEXT'
CT      AFTER = CLOCK(0)
CT      STTIME = STTIME + AFTER - BEFORE

        CALL QCEXT (QCS, QCD, H, NX, NY, I, LOY,
     1              XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     2              STRSPR, STRSUM, STRFLL, STRWIN, DCADT, DELT  )
        CALL CHEXT(UB, VB, UT, VT, WE, CS, CD, QCS, QCD, H, LNH,
     1             NX, NY, DELX, DELY, DELT, CDREF, CSREF, LAMBDA,
     2             GAMMA, AHS, AVS, I)

        CALL UWEXT
        CALL VWEXT
        CALL UTROP (DELX, DELY, AHM, F, BETA, G, RHOREF,
     1              H, WE, UT, VT, UB, VB, SS, SD, TS, TD,
     2              NX, NY, CONTOL, ITMAX, LNORM     )

CD      PRINT *,'CALLING CONVEC'
        CALL CONVEC(SS, SD, TS, TD, CD, NX, NY, SREF, TREF, I)
CD      PRINT *,'CHECKING FOR OUTPUT'
        IF (MOD(I,NOUT) .EQ. 0) THEN
           CALL OUTDAT (UB, VB, UT, VT, SS, SD, TS, TD, CS, CD,
     1                  NX, NY, RHOREF, SREF, TREF, I)
C          PRINT *,'TSTEP=',I
CT         PRINT *,'TSTEP=',I,' TIMING =',UVTIME/1E6, STTIME/1E6
        ENDIF
        IF (MOD(I,NFLOUT) .EQ. 0) CALL OUTFL (UB, VB, UT, VT,
     1            SS, SD, TS, TD, CS, CD, NX, NY,
     2            RHOREF, SREF, TREF, I )
 1000 CONTINUE

CT    PRINT *,' UV TIME=  ',UVTIME/1E6,' ST TIME= ', STTIME/1E6
      CALL OUTEND (UB, VB, UT, VT, SS, SD, TS, TD, CS, CD,
     1             NX, NY, RHOREF, SREF, TREF, I)

      END
      SUBROUTINE INIT(UB, VB, UT, VT, SS, SD, TS, TD, CS, CD,
     1                UWIND, VWIND, WE, H, LNH, NX, NY,
     2                AHM, AVM, AHT, AVT, AHS, AVS,
     3                SREF, SDREF, SSREF, TREF, TDREF, TSREF, RHOREF,
     4                G, F,
     5                DELX, DELY, DELT, GAMMA, NOUT, NFLOUT, NTOT,
     6                XMIN, XMAX, YMIN, YMAX, QFMAX, QFREF, QM,
     7                STRSPR, STRSUM, STRFLL, STRWIN, LOY,
     8                BETA, CONTOL, ITMAX, LNORM,
     9                CSREF, CDREF, LAMBDA, DCADT                    )
C     INITIALIZE THE DATA ARRAYS AND CONSTANTS

      INTEGER NFIELD
      PARAMETER (NFIELD = 14)
      REAL SECPYR
      PARAMETER (SECPYR = 3.1556908E7)

      INTEGER NX, NY
      REAL UB(NX, NY), VB(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), TS(NX, NY), TD(NX, NY)
      REAL CS(NX, NY), CD(NX, NY)
      REAL UWIND(NX, NY), VWIND(NX, NY), WE(NX, NY)
      REAL H(NX, NY), LNH(NX, NY)

      REAL AHM, AVM, AHT, AVT, AHS, AVS
      REAL SREF, SDREF, SSREF, TREF, TDREF, TSREF, RHOREF
      REAL G, F
      REAL DELX, DELY, DELT, GAMMA
      INTEGER NOUT, NFLOUT, NTOT

C     PARAMETERS FOR BUOYANCY FORCING
      INTEGER XMIN, XMAX, YMIN, YMAX
      REAL QFMAX, QFREF, QM
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY
C     PARAMETERS FOR CHEMISTRY
      REAL CSREF, CDREF, LAMBDA, DCADT
C     PARMS FOR BAROTROPIC FLOW
      REAL BETA, CONTOL
      INTEGER ITMAX, LNORM

      REAL RHO
      CHARACTER*6 FNAME
      INTEGER I, J, LOC
      LOGICAL YES
      INTEGER DUMMY
      REAL VALUE

C***********************************************************----------**
C     GET ARRAY DATA:

      PRINT *,'WOULD YOU LIKE TO USE OUTPUT FROM AN OLD RUN?'
      IF (YES(.FALSE.)) THEN
        PRINT *,'WHAT IS THE FILE NAME FOR UB?'
        READ (*,9001) FNAME
        OPEN (12, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR VB?'
        READ (*,9001) FNAME
        OPEN (13, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'UT   '
        READ (*,9001) FNAME
        OPEN (14, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'VT   '
        READ (*,9001) FNAME
        OPEN (15, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR SS?'
        READ (*,9001) FNAME
        OPEN (16, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR SD?'
        READ (*,9001) FNAME
        OPEN (17, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR TS?'
        READ (*,9001) FNAME
        OPEN (18, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR TD?'
        READ (*,9001) FNAME
        OPEN (19, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR CS?'
        READ (*,9001) FNAME
        OPEN (20, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR CD?'
        READ (*,9001) FNAME
        OPEN (21, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')

        PRINT *,'AT WHAT TIME STEP DO YOU WANT THE DATA?'
        READ (*,9014) DUMMY
        DO 1000 I = 1, DUMMY
          READ (12) UB
          READ (13) VB
          READ (14) UT
          READ (15) VT
          READ (16) SS
          READ (17) SD
          READ (18) TS
          READ (19) TD
          READ (20) CS
          READ (21) CD
 1000   CONTINUE
        CLOSE (12)
        CLOSE (13)
        CLOSE (14)
        CLOSE (15)
        CLOSE (16)
        CLOSE (17)
        CLOSE (18)
        CLOSE (19)
        CLOSE (20)
        CLOSE (21)

       ELSE
        WRITE (*,9011) 'UB   '
        READ (*,9012) VALUE
        CALL ARSET(UB, NX, NY, VALUE)
        WRITE (*,9011) 'VB   '
        READ (*,9012) VALUE
        CALL ARSET(VB, NX, NY, VALUE)
        WRITE (*,9011) 'UT   '
        READ (*,9012) VALUE
        CALL ARSET(UT, NX, NY, VALUE)
        WRITE (*,9011) 'VT   '
        READ (*,9012) VALUE
        CALL ARSET(VT, NX, NY, VALUE)
        WRITE (*,9011) 'SS   '
        READ (*,9012) VALUE
        CALL ARSET(SS, NX, NY, VALUE)
        WRITE (*,9011) 'SD   '
        READ (*,9012) VALUE
        CALL ARSET(SD, NX, NY, VALUE)
        WRITE (*,9011) 'TS   '
        READ (*,9012) VALUE
        CALL ARSET(TS, NX, NY, VALUE)
        WRITE (*,9011) 'TD   '
        READ (*,9012) VALUE
        CALL ARSET(TD, NX, NY, VALUE)
        WRITE (*,9011) 'CS   '
        READ (*,9012) VALUE
        CALL ARSET(CS, NX, NY, VALUE)
        WRITE (*,9011) 'CD   '
        READ (*,9012) VALUE
        CALL ARSET(CD, NX, NY, VALUE)
      ENDIF

      WRITE (*,9011) 'UWIND'
      READ (*,9012) VALUE
      CALL ARSET(UWIND, NX, NY, VALUE)
      WRITE (*,9011) 'VWIND'
      READ (*,9012) VALUE
      CALL ARSET(VWIND, NX, NY, VALUE)
CO    PRINT *,'WHAT IS THE NAME OF THE EKMAN PUMPING FILE?'
CO      READ (*,9001) FNAME
CO      CALL READ2(WE, NX, NY, 2, FNAME, .TRUE., .TRUE.)
      WRITE (*,9011) 'WE   '
      READ (*,9012) VALUE
      CALL ARSET(WE, NX, NY, VALUE)

CO    PRINT *,'WHAT IS THE NAME OF THE TOPOGRAPHY FILE?'
CO      READ (*,9001) FNAME
CO      CALL READ2(H, NX, NY, 1, FNAME, .TRUE., .TRUE.)
      WRITE (*,9011) 'H    '
      READ (*,9012) VALUE
      CALL ARSET(H, NX, NY, VALUE)

 9011 FORMAT (' WHAT IS THE VALUE OF ',A5,'?')

 9012 FORMAT (E13.6)

 9014 FORMAT (I5)

C***********************************************************----------**
      PRINT *,'WHAT IS THE NAME OF THE SCALAR PARAMETER FILE?'
      READ (*,9001) FNAME
      WRITE (*,9001) FNAME
      OPEN (11, FILE=FNAME, FORM='FORMATTED', STATUS='OLD')
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
C        GRID
        READ (11,9002) DELX
        READ (11,9002) DELY
        READ (11,9002) DELT
C        BOUNDARY CONDITION INFO.
        READ (11,9002) SREF
        READ (11,9002) SDREF
        READ (11,9002) SSREF
        READ (11,9002) TREF
        READ (11,9002) TDREF
        READ (11,9002) TSREF
        READ (11,9002) GAMMA
C        DIFFUSION
        READ (11,9002) AHM
        READ (11,9002) AVM
        READ (11,9002) AHT
        READ (11,9002) AVT
        READ (11,9002) AHS
        READ (11,9002) AVS
C        PHYSICAL CONSTANTS
        READ (11,9002) F
        READ (11,9002) G
C        TERMS FOR BAROTROPIC SOLUTION.
        READ (11,9002) BETA
        READ (11,9002) CONTOL
        READ (11,9003) ITMAX
        READ (11,9003) LNORM
C        TERMS FOR CHEMISTRY
        READ (11,9002) CSREF
        READ (11,9002) CDREF
        READ (11,9002) LAMBDA
        READ (11,9002) DCADT
      CLOSE (11, STATUS='KEEP')

C     COMPUTED PARAMETERS:
      RHOREF = RHO(SREF, TREF, 0.)
      LOY    = INT(SECPYR/DELT)
      DO 2000 J = 1, NY
        DO 2010 I = 1, NX
           LNH(I, J) = ALOG( H(I,J)  )
 2010   CONTINUE
 2000 CONTINUE
C     RESCALE INPUT VALUES OF QFMAX (M/DAY), QFREF,QM (M/SEASON) TO
C       KG/M**2/S.  MUST DIFIVE BY H IN UVEXT.  8-10-88.
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
C        GRID
        WRITE (*,9002) DELX
        WRITE (*,9002) DELY
        WRITE (*,9002) DELT
C        BOUNDARY CONDITION INFO.
        WRITE (*,9002) SREF
        WRITE (*,9002) SDREF
        WRITE (*,9002) SSREF
        WRITE (*,9002) TREF
        WRITE (*,9002) TDREF
        WRITE (*,9002) TSREF
        WRITE (*,9002) GAMMA
C        DIFFUSION
        WRITE (*,9002) AHM
        WRITE (*,9002) AVM
        WRITE (*,9002) AHT
        WRITE (*,9002) AVT
        WRITE (*,9002) AHS
        WRITE (*,9002) AVS
C        PHYSICAL CONSTANTS
        WRITE (*,9002) F
        WRITE (*,9002) G
C        TERMS FOR BAROTROPIC SOLUTION.
        WRITE (*,9002) BETA
        WRITE (*,9002) CONTOL
        WRITE (*,9003) ITMAX
        WRITE (*,9003) LNORM
C        TERMS FOR CHEMISTRY
        WRITE (*,9002) CSREF
        WRITE (*,9002) CDREF
        WRITE (*,9002) LAMBDA
        WRITE (*,9002) DCADT
C        COMPUTED CONSTANTS
        WRITE (*,9003) LOY

 9001 FORMAT (A6)

 9002 FORMAT (BN, E14.6)

 9003 FORMAT (BN, I8)

      RETURN
      END
      SUBROUTINE OUTDAT (UC, VC, UT, VT, SS, SD, TS, TD, CS, CD,
     1                   NX, NY, RHOREF, SREF, TREF, TIME        )

C     WRITE OUT THE VELOCITY, SALINITY, AND TEMPERATURE.
      INTEGER NFIELD
      PARAMETER (NFIELD = 10)

      INTEGER TIME, NX, NY
      REAL UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), TS(NX, NY), TD(NX, NY)
      REAL CS(NX, NY), CD(NX, NY)
      REAL SREF, TREF, RHOREF

      INTEGER I, J
      REAL UCTEMP, VCTEMP
      REAL UTTEMP, VTTEMP
      REAL MSUM, SALSUM, HETSUM, AABWFL
      INTEGER NFL
      PARAMETER (NFL = 20)
      REAL FLM(NFL), FLS(NFL), FLC(NFL)

      CHARACTER*6 FNAME(NFIELD)

      UCTEMP  = UC(1,1)
      VCTEMP  = VC(1,1)
      UTTEMP  = UT(1,1)
      VTTEMP  = VT(1,1)
      UC(1,1) = 0.025
      VC(1,1) = 0.025
CU    UT(1,1) = 0.025
CU    VT(1,1) = 0.025

      WRITE (29+1) UC
      WRITE (29+2) VC
      WRITE (29+3) UT
      WRITE (29+4) VT
      WRITE (29+5) SS
      WRITE (29+6) SD
      WRITE (29+7) TS
      WRITE (29+8) TD
      WRITE (38)   CS
      WRITE (39)   CD

      UC(1,1) = UCTEMP
      VC(1,1) = VCTEMP
CU    UT(1,1) = UTTEMP
CU    VT(1,1) = VTTEMP

      RETURN

      ENTRY OUTFL(UC, VC, UT, VT, SS, SD, TS, TD, CS, CD, NX, NY,
     1            RHOREF, SREF, TREF, TIME)
C     COMPUTE OUTPUT FLUXES AT THE I=3 BNDY, FOR THE LOWER LAYER.
      MSUM   = 0.0
      SALSUM = 0.0
      HETSUM = 0.0
      AABWFL = 0.0

C     I = 3
C     DO 1000 J = 1, NY
C       THE MINUS SIGN IS DUE TO THE FACT THAT THIS IS COMPUTED FOR THE
C          LAYER.
C       MSUM   = MSUM   - U(I,J)
C       SALSUM = SALSUM - U(I,J)*.5*(SS(I,J)-SD(I,J))
C       HETSUM = HETSUM - U(I,J)*.5*(TS(I,J)-TD(I,J))
C       IF ( SS(I,J)-SD(I,J) .GT. 0.2)
C    1      AABWFL = AABWFL - U(I,J)
C1000 CONTINUE

      DO 1000 I = 1, NX
        FLM(I) = 0.0
        FLS(I) = 0.0
        FLC(I) = 0.0
        DO 1010 J = 1, NY
          FLM(I) = FLM(I) - (VT(I,J) - VC(I,J))
          FLS(I) = FLS(I) - (VT(I,J) - VC(I,J))*(SS(I,J)-SD(I,J))
          FLC(I) = FLC(I) - (VT(I,J) - VC(I,J))*(CS(I,J)-CD(I,J))
 1010   CONTINUE
 1000 CONTINUE

C     WRITE (40) TIME, MSUM, SALSUM, HETSUM, AABWFL
      WRITE (40) FLM
      WRITE (41) FLS
      WRITE (42) FLC

      RETURN

      ENTRY OUTSTR(UC, VC, UT, VT, SS, SD, TS, TD, CS, CD, NX, NY,
     1            RHOREF, SREF, TREF, TIME)

C     OPEN THE NECESSARY OUTPUT FILES
      DO 2000 I = 1, NFIELD
        PRINT *,'WHAT IS THE NAME OF OUTPUT FILE # ',I
        READ  (*,9002) FNAME(I)
        WRITE (*,9002) FNAME(I)
CD      PRINT *,FNAME
        OPEN (29+I, FILE=FNAME(I), FORM='UNFORMATTED', STATUS='NEW')
 2000 CONTINUE
      PRINT *,'NAME FOR THE MASS FLUX FILE '
      READ  (*, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
      OPEN (40, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'NAME FOR THE SALT FLUX FILE '
      READ  (*, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
      OPEN (41, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'NAME FOR THE CHEMICAL FLUX FILE '
      READ  (*, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
      OPEN (42, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')

 9002 FORMAT (A6)

      RETURN

      ENTRY OUTEND(UC, VC, UT, VT, SS, SD, TS, TD, CS, CD, NX, NY,
     1                                 RHOREF, SREF, TREF, TIME)

C     CLOSE THE DATA FILES:
      CLOSE (30, STATUS='KEEP')
      CLOSE (31, STATUS='KEEP')
      CLOSE (32, STATUS='KEEP')
      CLOSE (33, STATUS='KEEP')
      CLOSE (34, STATUS='KEEP')
      CLOSE (35, STATUS='KEEP')
      CLOSE (36, STATUS='KEEP')
      CLOSE (37, STATUS='KEEP')
      CLOSE (38, STATUS='KEEP')
      CLOSE (39, STATUS='KEEP')
      CLOSE (40, STATUS='KEEP')
      CLOSE (41, STATUS='KEEP')
      CLOSE (42, STATUS='KEEP')

      RETURN
      END
      SUBROUTINE UTROP(DX, DY, AM, F, BETA, G, RHOREF,
     1                 H, WE, UT, VT, UC, VC, SS, SD, TS, TD,
     2                 NX, NY, CONTOL, ITMAX, L               )
C     COMPUTE THE STEADY BAROTROPIC VELOCITY FIELD.
C     VERSION WITHOUT BOUNDARY LAYERS STARTED 12-20-88.
C     BOUNDARY LAYERS BEGUN 1-11-89.

      INTEGER NX, NY
      REAL DX, DY, AM, F, BETA, G, RHOREF
      REAL H(NX, NY), WE(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL UC(NX, NY), VC(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), TS(NX, NY), TD(NX, NY)
      REAL CONTOL, RHOS1P
      INTEGER ITMAX, L

C     LOCAL VARIABLES - INTERNAL VELOCITY FIELD
      INTEGER NNX, NNY
      PARAMETER (NNX = 20)
      PARAMETER (NNY = 20)
      REAL FORCE(NNX, NNY), UN(NNX, NNY), UNP1(NNX, NNY)
      REAL DHDX(NNX, NNY), DHDY(NNX, NNY), TOPBET(NNX, NNY)
      REAL WPCLIN(NNX, NNY), DEN(NNX, NNY), BUOYVR(NNX, NNY)
      REAL FDIV(NNX, NNY), FVOR(NNX, NNY), Q(NNX, NNY)
      INTEGER I, J, ITERS
      REAL NORM, DBGSCA, DBGARY(NNX, NNY)
      LOGICAL FLATX
C     LOCAL VARIABLES FOR BOUNDARY LAYER SOLN:
      INTEGER K, NB
      REAL LB, BETTMN, UB(NNX, NNY), VB(NNX, NNY)
      REAL PI
      PARAMETER (PI = 3.141592654)
C       FOR LINPACK INVERSION OF BOUNDARY LAYERS
      INTEGER IPVT(NNY)
      REAL COEF(NNY, NNX), RHS(NNY), Z(NNY), RCOND

C***********************************************************----------!!

C     COMPUTE CONSTANTS:

      DO 100 J = 1, NY
        DO 101 I = 2, NX-1
          DHDX(I,J) = (H(I+1,J) - H(I-1,J))/2./DX
  101   CONTINUE
  100 CONTINUE
      DO 102 J = 1, NY
        I = 1
        DHDX(1,J) = (H(I+1,J)-H(I,J))/DX
        I = NX
        DHDX(I,J) = (H(I,J)-H(I-1,J))/DX
  102 CONTINUE
      DO 110 J = 2, NY-1
        DO 111 I = 1, NX
          DHDY(I,J) = (H(I,J+1)-H(I,J-1))/2./DY
  111   CONTINUE
  110 CONTINUE
      DO 112 I = 1, NX
        J = 1
        DHDY(I,J) = (H(I,J+1)-H(I,J))/DY
        J = NY
        DHDY(I,J) = (H(I,J)-H(I,J-1))/DY
  112 CONTINUE

      BETTMN = 0.0
      FLATX  = .TRUE.
      DO 200 J = 1, NY
        DO 210 I = 1, NX
          TOPBET(I,J) = BETA - F*DHDY(I,J)/H(I,J)
          BETTMN      = BETTMN + TOPBET(I,J)
          Q(I,J)      = F*DHDX(I,J)/TOPBET(I,J)
          FLATX       = FLATX .AND. (Q(I,J) .LE. 0.1*H(I,J))
  210   CONTINUE
  200 CONTINUE
      BETTMN = BETTMN/FLOAT(NX*NY)
      LB     = 4.*PI*(AM/BETTMN)**(1./3.)/SQRT(3.)
      NB     = AMAX0(5, INT(LB/DX + 0.5) )

C***********************************************************----------!!
C     COMPUTE ARRAYS WHICH CHANGE BETWEEN CALLS:
      DO 300 J = 1, NY
        DO 301 I = 1, NX
          DEN(I,J) = 3.*RHOS1P(SS(I,J)+SD(I,J),TS(I,J)+TD(I,J),0.)
     1                + RHOS1P(SS(I,J)-SD(I,J),TS(I,J)-TD(I,J),0.)
  301   CONTINUE
  300 CONTINUE
      DO 310 J = 2, NY-1
        DO 311 I = 2, NX-1
          BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J-1))/2./DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I-1,J))/2./DX )
  311   CONTINUE
  310 CONTINUE
      DO 312 J = 2, NY-1
        I = 1
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J-1))/2./DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I,J))/DX )
        I = NX
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J-1))/2./DY
     2                 -DHDY(I,J)*(DEN(I,J)-DEN(I-1,J))/DX )
  312 CONTINUE
      DO 313 I = 2, NX-1
        J = 1
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J))/DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I-1,J))/2./DX )
        J = NY
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J)-DEN(I,J-1))/DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I-1,J))/2./DX )
  313 CONTINUE
      BUOYVR(1,1)   = 0.0
      BUOYVR(1,NY)  = 0.0
      BUOYVR(NX,1)  = 0.0
      BUOYVR(NX,NY) = 0.0

      DO 400 J = 1, NY
        DO 401 I = 1, NX
          WPCLIN(I,J) = -WE(I,J)+UC(I,J)*DHDX(I,J)+VC(I,J)*DHDY(I,J)
          FVOR(I,J)   = -WPCLIN(I,J)*F/H(I,J) - BUOYVR(I,J)
  401   CONTINUE
  400 CONTINUE

      DO 500 J = 2, NY-1
        DO 501 I = 1, NX
          FDIV(I,J) = WPCLIN(I,J)
     1 - F/TOPBET(I,J) * (WPCLIN(I,J+1)-WPCLIN(I,J-1))/2./DY
     2 + F*F/TOPBET(I,J)/TOPBET(I,J)*WPCLIN(I,J)/H(I,J)
     3   *( (DHDY(I,J+1)-DHDY(I,J-1))/2./DY  - DHDY(I,J)**2/H(I,J) )
  501   CONTINUE
  500 CONTINUE
      DO 502 I = 1, NX
        J = NY
          FDIV(I,J) = WPCLIN(I,J)
     1 - F/TOPBET(I,J)* (WPCLIN(I,J)-WPCLIN(I,J-1))/DY
     2 + F*F/TOPBET(I,J)/TOPBET(I,J)*WPCLIN(I,J)/H(I,J)
     3   *( (DHDY(I,J)-DHDY(I,J-1))/DY  - DHDY(I,J)**2/H(I,J) )
        J = 1
          FDIV(I,J) = WPCLIN(I,J)
     1 - F/TOPBET(I,J)* (WPCLIN(I,J+1)-WPCLIN(I,J))/DY
     2 + F*F/TOPBET(I,J)/TOPBET(I,J)*WPCLIN(I,J)/H(I,J)
     3   *( (DHDY(I,J+1)-DHDY(I,J))/DY  - DHDY(I,J)**2/H(I,J) )
  502 CONTINUE

C***********************************************************----------!!

C     NOW ENTER THE XECTION TO EXTRAPOLATE THE INTERIOR SOLUTION:
      IF (.NOT. FLATX) THEN
        DO 1011 J = 2, NY-1
          DO 1012 I = 1, NX
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
     1  - UN(I,J) * (Q(I,J+1)-Q(I,J-1))/2./DY
     2  - Q(I,J)* (UN(I,J+1)-UN(I,J-1))/2./DY
 1012     CONTINUE
 1011   CONTINUE
        DO 1013 I = 1, NY
          J = 1
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
     1  - UN(I,J) * (Q(I,J+1)-Q(I,J))/DY
     2  - Q(I,J)* (UN(I,J+1)-UN(I,J))/DY
          J = NX
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
     1  - UN(I,J) * (Q(I,J)-Q(I,J-1))/DY
     2  - Q(I,J)* (UN(I,J)-UN(I,J-1))/DY
 1013   CONTINUE

       ELSE
        DO 1000 J = 1, NY
          DO 1010 I = 1, NX
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
 1010     CONTINUE
 1000   CONTINUE

      ENDIF

      ITERS = 0
 9999 CONTINUE
        ITERS = ITERS + 1

        DO 2000 J = 1, NY
          UNP1(NX, J) = 0.0
          DO 2010 I = NX-1, 1, -1
            UNP1(I, J) = UNP1(I+1,J) + FORCE(I+1,J) + FORCE(I,J)
 2010     CONTINUE
          DO 2020 I = 1, NX-1
            UNP1(I,J) = UNP1(I,J)*DX/2./H(I,J)
 2020     CONTINUE
 2000   CONTINUE

        IF (.NOT. FLATX .AND. NORM(UNP1, UN, NX*NY, L) .GT. CONTOL
     1                  .AND. ITERS .LT. ITMAX) THEN
          DO 3000 J = 2, NY-1
            DO 3010 I = 1, NX
              UN(I,J) = UNP1(I,J)
              FORCE(I,J) = FDIV(I,J)
     1    - UNP1(I,J) * (Q(I,J+1)-Q(I,J-1))/2./DY
     2    - Q(I,J)* (UNP1(I,J+1)-UNP1(I,J-1))/2./DY
 3010       CONTINUE
 3000     CONTINUE
          DO 3020 I = 1, NY
            J = 1
              UN(I,J) = UNP1(I,J)
              FORCE(I,J) = FDIV(I,J)
     1    - UNP1(I,J) * (Q(I,J+1)-Q(I,J))/DY
     2    - Q(I,J)* (UNP1(I,J+1)-UNP1(I,J))/DY
            J = NX
              UN(I,J) = UNP1(I,J)
              FORCE(I,J) = FDIV(I,J)
     1    - UNP1(I,J) * (Q(I,J)-Q(I,J-1))/DY
     2    - Q(I,J)* (UNP1(I,J)-UNP1(I,J-1))/DY
 3020     CONTINUE

          GO TO 9999
        ENDIF

      DO 4000 J = 1, NY
        DO 4010 I= 1, NX
          UT(I,J) = UNP1(I,J)
          VT(I,J) = (F/H(I,J)*DHDX(I,J)*UNP1(I,J) + FVOR(I,J) )
     1             / TOPBET(I,J)
 4010   CONTINUE
 4000 CONTINUE

C***********************************************************----------!!

      RETURN
      END
      SUBROUTINE QSEXT(QSS, QSD, H, NX, NY, TSTEP, LOY,
     1  XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     2  STRSPR, STRSUM, STRFLL, STRWIN               )
C     SUBROUTINE TO EXTRAPOLATE THE SALINIZATION FORCING TO THE NEXT
C       TIME STEP.  BG 3-25-88.
C     HIGH FORCING SPECIFICATION CHANGED TO A RECTANGLE 12-19-88, BG.
      INTEGER NX, NY, TSTEP
      REAL QSS(NX, NY), QSD(NX, NY), H(NX, NY)
      INTEGER I, J, T
      INTEGER XMIN, XMAX, YMIN, YMAX
C     FIND THE CENTER AND SIZE OF THE ENHANCED FREEZING REGION
C     FIND THE BOUNDS OF THE ENHANCED FREEZING RECTANGLE. 12-19-88.
      REAL QSFMAX, QSFREF
C     THE MAXIMUM AND NORMAL FREEZING RATES.
      REAL QSM
C     THE MELTING RATE (UNIFORM OVER DOMAIN)
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY
C     START OF SPRING, SUMMER, FALL, WINTER, AND LENGTH OF THE YEAR
C       FOR NOW, DISCONTINUOUS BEHAVIOR - WINTER HAS FREEZING, FALL
C       SPRING HAVE NORE MELT OR FREEZE, AND SUMMER HAS MELTING
      REAL PI, TIME
      PARAMETER (PI = 3.141592654)
      T = MOD(TSTEP,LOY)
      IF ((T .GE. STRWIN) .AND. (T .LT. STRSPR)) THEN
C     NOTE THAT THIS ASSUMES THAT TIME IS TOLD WITHIN A YEAR.
C     WINTERTIME
CD      PRINT *,'WINTER'
        DO 1000 J = 1, NY
          DO 1010 I = 1, NX
            IF ((I .GE. XMIN .AND. I .LE. XMAX) .AND.
     1          (J .GE. YMIN .AND. J .LE. YMAX) ) THEN
C             IN ENHANCED AREA
              QSS(I,J) = QSFMAX
              QSD(I,J) = QSFMAX
             ELSE
              QSS(I,J) = QSFREF
              QSD(I,J) = QSFREF
            ENDIF
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (T .GE. STRSPR .AND. T .LT. STRSUM) THEN
C       SPRING, QSS = QSD = 0.0
        DO 1100 J = 1, NY
          DO 1110 I = 1, NX
            QSS(I,J) = 0.0
            QSD(I,J) = 0.0
 1110     CONTINUE
 1100   CONTINUE
       ELSEIF (T .GE. STRSUM .AND. T .LT. STRFLL) THEN
        DO 2000 J = 1, NY
          DO 2010 I = 1, NX
            QSS(I,J) = QSM
            QSD(I,J) = QSM
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       FALL, DO NOTHING
        DO 2100 J = 1, NY
          DO 2110 I = 1, NX
            QSS(I,J) = 0.0
            QSD(I,J) = 0.0
 2110     CONTINUE
 2100   CONTINUE
      ENDIF
C     NOTE THE LOGICALLY BIZARRE STRUCTURE, SOMETHING TO CHANGE.
C     FIXED 3-30-88
      RETURN
      ENTRY QSEXTS(QSS, QSD, H, NX, NY, TSTEP, LOY,
     1  XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     2  STRSPR, STRSUM, STRFLL, STRWIN               )
C     SUBROUTINE TO EXTRAPOLATE THE SALINIZATION FORCING TO THE NEXT
C       TIME STEP.  BG 3-25-88.
C     SINUSOIDAL VARIATION IN TIME VERSION. 3-30-88. BG
C     ADDED AS AN ENTRY TO QSEXT 5-26-88. BG
C     THE PERIOD OF THE OSCILLATION (IN TIME STEPS) IS TAKEN AS STRWIN
C        8-10-88. BG
C     LOGICALLY OUTDATED BY RECTANGLE FORCING VERSION.
      TIME = FLOAT(TSTEP)/FLOAT(STRWIN)
      DO 3000 J = 1, NY
        DO 3010 I = 1, NX
          IF ((I .LE. XMIN .AND. I .GE. XMAX) .AND.
     1        (J .LE. YMIN .AND. J .GE. YMAX) ) THEN
C           IN ENHANCED AREA
            IF (SIN(2.*TIME*PI) .GT. 0.0) THEN
              QSS(I,J) = QSFMAX * SIN(2.*PI*TIME)
              QSD(I,J) = QSFMAX * SIN(2.*PI*TIME)
             ELSE
              QSS(I,J) = QSFREF * SIN(2.*PI*TIME)
              QSD(I,J) = QSFREF * SIN(2.*PI*TIME)
            ENDIF
           ELSE
            QSS(I,J) = QSFREF * SIN(2.*PI*TIME)
            QSD(I,J) = QSFREF * SIN(2.*PI*TIME)
          ENDIF
 3010   CONTINUE
 3000 CONTINUE
      RETURN
      END
      SUBROUTINE QCEXT(QCS, QCD, H, NX, NY, TSTEP, LOY,
     1  XMIN, XMAX, YMIN, YMAX, QFMAX, QFREF, QM,
     2  STRSPR, STRSUM, STRFLL, STRWIN, DCADT, DELT   )
C     HIGH FORCING SPECIFICATION CHANGED TO A RECTANGLE 12-19-88, BG.
C     FORCING FOR CHEMICALS, TAKEN FROM QSEXT.F OF 1-24-89.
      INTEGER NX, NY, TSTEP
      REAL QCS(NX, NY), QCD(NX, NY), H(NX, NY)
      INTEGER I, J, T
      INTEGER XMIN, XMAX, YMIN, YMAX
      REAL QFMAX, QFREF, QM
      REAL DCADT, DELT
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY
C     START OF SPRING, SUMMER, FALL, WINTER, AND LENGTH OF THE YEAR
      REAL PI, TIME
      PARAMETER (PI = 3.141592654)
      T = MOD(TSTEP,LOY)
      IF ((T .GE. STRWIN) .AND. (T .LT. STRSPR)) THEN
C     WINTERTIME
CD      PRINT *,'WINTER'
        DO 1000 J = 1, NY
          DO 1010 I = 1, NX
            IF ((I .GE. XMIN .AND. I .LE. XMAX) .AND.
     1          (J .GE. YMIN .AND. J .LE. YMAX) ) THEN
C             IN POLYNYA AREA
              QCS(I,J) = DCADT*DELT*FLOAT(TSTEP)
              QCD(I,J) = DCADT*DELT*FLOAT(TSTEP)
             ELSE
              QCS(I,J) = 0.0
              QCD(I,J) = 0.0
            ENDIF
 1010     CONTINUE
 1000   CONTINUE
       ELSE
        DO 1100 J = 1, NY
          DO 1110 I = 1, NX
            QCS(I,J) = DCADT*DELT*FLOAT(TSTEP)
            QCD(I,J) = DCADT*DELT*FLOAT(TSTEP)
 1110     CONTINUE
 1100   CONTINUE
      ENDIF
      RETURN
      END
      SUBROUTINE UVEXT (UB, VB, SS, SD, TS, TD,
     1                  UWIND, VWIND, H, LNH, NX, NY,
     2                  SREF, TREF, RHOREF, G, F,
     3                  AHM, AVM, DELX, DELY, DELT            )
C     EXTRAPOLATE U, V TO THE NEXT TIME LEVEL
C     COMPUTATION OF COMMON CONSTANTS ADDED PRIOR TO 5-26-88.
C     USE OF MULTI-ENTRY RHO ADDED 5-25-88.
C     FORCING ARRAYS ENLARGED TO 60X60 BY 5-26-88.
C     EDDY DIFFUSION (LESS TOPOGRAPHIC TERMS) RE-ENTERED 3-30-88.
C     BC 3-29 TO 5-25-88:
C       Y = 0 NO SLIP
C           YMAX  V = 0  DU/DY = 0
C       X = XMAX  U,V(XMAX) = U,V(1)
C       X = 0     U EXTRAPOLATED, DV/DX = 0
C     BC ON 5-26-88 CHANGED TO
C       X = 0     U, V BOTH EXTRAPOLATED USING FORWARD DIFFERENCES.
      INTEGER NX, NY
      REAL UB(NX, NY), VB(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), TS(NX, NY), TD(NX, NY)
      REAL UWIND(NX, NY), VWIND(NX, NY), H(NX, NY), LNH(NX, NY)
      REAL SREF, TREF, RHOREF, F, G, AHM, AVM
      REAL DELX, DELY, DELT
      REAL RHOS1P, RHOS(60,60), RHOD(60,60)
      REAL FU(60, 60), FV(60, 60)
      INTEGER I, J, K, L
C     PARAMS FOR SPEEDIER NUMERICS:
      REAL DX2, DY2, G8RREF, DIFUX, DIFUY
C     STATEMENT FUNCTION TO COMPUTE RHOS, RHOD.  CHANGED TO ARRAYS 8-4-8
C     RHOS(K,L) =
C    1  RHOS1P(SREF+.5*(SS(K,L)+SD(K,L)), TREF+.5*(TS(K,L)+TD(K,L)), 0.)
C    2+ RHOS1P(SREF+.5*(SS(K,L)-SD(K,L)), TREF+.5*(TS(K,L)-TD(K,L)), 0.)
C     RHOD(K,L) =
C    1  RHOS1P(SREF+.5*(SS(K,L)+SD(K,L)), TREF+.5*(TS(K,L)+TD(K,L)), 0.)
C    2- RHOS1P(SREF+.5*(SS(K,L)-SD(K,L)), TREF+.5*(TS(K,L)-TD(K,L)), 0.)
C     COMPUTE PARAMS FOR SPEEDIER NUMERICS:
      DX2   = 2.*DELX
      DY2   = 2.*DELY
      G8RREF = G/8./RHOREF
      DIFUX  = AHM/DELX/DELX
      DIFUY  = AHM/DELY/DELY
C     COMPUTE THE DENSITY FIELD BEFORE ENTERING THE EXTRAPOLATION.
C     THIS REDUCES THE NUMBER OF CALLS TO THE DENSITY FUNCTION BY
C       ALMOST A FACTOR OF 4.  8-4-88.
      DO 900 L = 1, NY
        DO 910 K = 1, NX
            RHOS(K,L) =
     1    RHOS1P( SS(K,L)+SD(K,L), TS(K,L)+TD(K,L), 0.)
     2  + RHOS1P( SS(K,L)-SD(K,L), TS(K,L)-TD(K,L), 0.)
  910   CONTINUE
  900 CONTINUE
C     COMPUTE THE FORCING FOR THE EXTRAPOLATION (INTERIOR POINTS):
      DO 1000 J = 2, NY-1
        DO 1010 I = 2, NX-1
CD        PRINT *,'X, Y COORDINATES= ',I,J
                    FU(I,J) =  F*VB(I,J)/2.
     1  + G8RREF*H(I,J)*
     2         ( RHOS(I+1,J) - RHOS(I-1,J) )/DX2
C       NEXT LINE REMOVED 8-3-88 DUE TO ERROR IN MARCH DERIVATION,
C          CAUGHT IN AUGUST DERIVATION.
C    3        - RHOD(I,J)*(LNH(I+1,J)-LNH(I-1,J))/DX2  )
     4  + DIFUX * (UB(I+1,J)-2.*UB(I,J)+UB(I-1,J))
     5  + DIFUY * (UB(I,J+1)-2.*UB(I,J)+UB(I,J-1))
C       VERTICAL DIFFUSION OF MOMENTUM ADDED 8-3-88.  QUESTIONABLE
C         NECESSITY.
     6  - 8.*AVM*UB(I,J)/H(I,J)/H(I,J)
                     FV(I,J) = -F*UB(I,J)/2.
     1    +G8RREF*H(I,J)*
     2         ( RHOS(I,J+1) - RHOS(I,J-1) )/DY2
C       NEXT LINE REMOVED 8-3-88 DUE TO ERROR IN MARCH DERIVATION,
C          CAUGHT IN AUGUST DERIVATION.
C    3        - RHOD(I,J)*(LNH(I,J+1)-LNH(I,J-1))/DY2  )
     4   + DIFUX * (VB(I+1,J)-2.*VB(I,J)+VB(I-1,J))
     5   + DIFUY * (VB(I,J+1)-2.*VB(I,J)+VB(I,J-1))
C       VERTICAL DIFFUSION OF MOMENTUM ADDED 8-3-88.  QUESTIONABLE
C         NECESSITY.
     6  - 8.*AVM*VB(I,J)/H(I,J)/H(I,J)
 1010   CONTINUE
 1000 CONTINUE
C     COMPUTE FU(I=1) WHERE ALPHA(I-1) = ALPHA(NX-1) [PERIODIC CONDITION
C     WARNING!! MUST CHANGE THIS, PERIODIC CONDITION DROPPED 3-9-88.
C     PERIODIC CONDITION IN U REINSTATED 3-29-88 WITH PROVISO THAT
C       VALUES USED IN THE EXTRAPOLATION MUST NOT WRAP AROUND TO THE
C       OTHER SIDE THROUGH THE PERIODICITY.  THIS ALSO REQUIRES THAT
C       THE BC ON S, T AT I=1 MUST NOT BE NO NORMAL GRAD FOR LARGE DU/DT
C     WITH THE CHANGE OF BC TO NO GRAD ON 7-26-88 THIS SECTION IS
C       NOT NECESSARY.  IN FACT IT CORRESPONDS TO A SIMPLE OPEN
C       BOUNDARY.
C     I = 1
C     DO 2000 J = 2, NY-1
CD      PRINT *,'X Y COORDINATES', I, J
C                   FU(I,J) =  F*V(I,J)/2.
C    1  + G8RREF*H(I,J)*
C    2      (   ( RHOS(I+1,J) - RHOS(I,J) )/DELX
C    3        - RHOD(I,J)*(LNH(I+1,J)-LNH(I,J))/DELX  )
C    4  + DIFUX * (U(I+2,J)-2.*U(I+1,J)+U(I,J))
C    5  + DIFUY * (U(I,J+1)-2.*U(I,J)+U(I,J-1))
C                   FV(I,J) =  -F*U(I,J)/2.
C    1  + G8RREF*H(I,J)*
C    2      (   ( RHOS(I,J+1) - RHOS(I,J) )/DELY
C    3        - RHOD(I,J)*(LNH(I,J+1)-LNH(I,J))/DELY  )
C    4  + DIFUX * (V(I+2,J)-2.*V(I+1,J)+V(I,J))
C    5  + DIFUY * (V(I,J+1)-2.*V(I,J)+V(I,J-1))
C2000 CONTINUE
C      NOW EXTRAPOLATE:
CD     PRINT *,'NOW CONDUCTING EXTRAPOLATION:'
C      WITH CHANGE OF I=1 BC TO NO GRAD, THE EXTRAPOLATION IS FROM
C        I = 2, NX-1 RATHER THANT I = 1, NX-1.  7-29-88
       DO 3000 J = 2, NY-1
         DO 3010 I = 2, NX-1
           UB(I,J) = (UB(I,J) + DELT*FU(I,J) +
     1                 (VB(I,J) + DELT*FV(I,J))*(F*DELT/2.) )
     2               / (1.+(ABS(F)*DELT/2.)**2.)
           VB(I,J) = (VB(I,J) + DELT*FV(I,J) -
     1                 (UB(I,J) + DELT*FU(I,J))*(F*DELT/2.) )
     2               / (1.+(ABS(F)*DELT/2.)**2.)
 3010   CONTINUE
 3000 CONTINUE
C     NOW CONSIDER THE BOUNDARY CONDITIONS:
C       AT J=1, V=0 DU/DY=0
C          J=1, V=0, U = 0, AS OF 3-30-88
C       AT J=NY, V=0 DU/DY=0
C       AT I=NX, DV/DX=0 U=PERIODIC (3-29)
C       AT I=1,  DV/DX=0 U=AS COMPUTED (3-29)
C                U, V AS COMPUTED 5-26-88.
C       AS OF 7-26-88, CHANGE U, V BOUNDARY CONDITIONS TO NO NORMAL
C         GRADIENT (FREE SLIP) AT I= 1, NX
C         AT I = 1   DV/DX = DU/DX = 0.0
C                NX  DV/DX = DU/DX = 0.0
C       7-29-88 NO NORMAL GRAD IN U, V AT J = JMAX
C       1-26-89:
C         AT I = 1  U = V = 0.0
C         AT I = NX U = V = 0.0
C         AT J = 1  U = V = 0.0
C         AT J = NY NORMAL DERIV = 0.0
      DO 4000 I = 1, NX
C       BC ON V AT THE Y BOUNDARIES
        VB(I,1)  = 0.0
        VB(I,NY) = VB(I, NY-1)
C       BC ON U AT THE Y BOUNDARIES
        UB(I,1)  = 0.0
        UB(I,NY) = UB(I, NY-1)
 4000 CONTINUE
      DO 4010 J = 1, NY
C       DV/DY = 0.0 IMPLEMENTED 7-26-88
C       V = 0.0 IMPLEMENTED 1-26-89
CD      VB(1,  J) = VB(2   , J)
CD      VB(NX, J) = VB(NX-1, J)
        VB(1,  J) = 0.0
        VB(NX, J) = 0.0
C       VB(NX,J) = VB(1,J)
C       PERIODICITY ON U IN X, CHANGED TO NO NORMAL GRAD 3-9-88.
C       RETURNED TO PERIODIC 3-29-88
C       U(1,J)  = U(1,J)
C       U(NX,J) = U(1,J)
C       BACK TO NO NORMAL GRAD 7-26-88
CD      UB(1 , J) = UB(2   , J)
CD      UB(NX, J) = UB(NX-1, J)
C       U = 0.0 IMPLEMENTED 1-26-89
        UB(1,  J) = 0.0
        UB(NX, J) = 0.0
 4010 CONTINUE
      RETURN
      END
      SUBROUTINE CHEXT(UC, VC, UT, VT, WE, CS, CD, QS, QD, H, LNH,
     1                 NX, NY, DELX, DELY, DELT, DREF, SREF, LAMBDA,
     2                 GAMMA, AH, AV, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE THE CHEMICAL TRACER FIELD TO THE NEXT
C       TIME STEP.
C     TAKEN FROM STDIF OF 1-24-89.
C     BC 1-24-89
C       Y = 0, YMAX D /DY = 0
C       X = 0, D /DX = 0
C       X = XMAX  D / DX = GAMMA*(A -A0)
      INTEGER NX, NY, TSTEP
      REAL WE(NX, NY), UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL QS(NX, NY), QD(NX, NY), CS(NX, NY), CD(NX, NY)
      REAL H(NX, NY), LNH(NX, NY)
      REAL DELX, DELY, DELT, DREF, SREF, GAMMA, LAMBDA, AH, AV
      INTEGER I, J
      REAL FCS(40, 40), FCD(40, 40)
      REAL DX2, DY2
      REAL PSI
      INTEGER K, L
      PSI(K,L) = 0.0
CD    PSI(K,L) = SIGN(1., WE(K,L)
CD   1  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CD   2  +  UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   3  +  VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2
CD   4  -  UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   5  -  VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2          )
      DX2 = 2.*DELX
      DY2 = 2.*DELY
C     COMPUTE FORCING FOR THE INTERIOR POINTS:
      DO 1000 J = 2, NY-1
        DO 1010 I = 2, NX-1
          FCS(I,J) =
C              ISO HALINE/THERMAL MIXED LAYER 8-15-88.
     1     - UT(I,J)*(CS(I+1,J)-CS(I-1,J))/DX2
     2     - VT(I,J)*(CS(I,J+1)-CS(I,J-1))/DY2
     3     - UC(I,J)*(CD(I+1,J)-CD(I-1,J))/DX2
     4     - VC(I,J)*(CD(I,J+1)-CD(I,J-1))/DY2
     5     - CD(I,J)*(  (UC(I+1,J)-UC(I-1,J))/DX2
     6                + (VC(I,J+1)-VC(I,J-1))/DY2   )
     7     + LAMBDA*(QS(I,J)-CS(I,J)-CD(I,J))/H(I,J)
     8     - WE(I,J)*CD(I,J)/H(I,J)
C          ADD DIFFUSIVE TERMS 3-30-88
     9     + AH*( (CS(I+1,J)-2.*CS(I,J)+CS(I-1,J))/(DELX*DELX)
     1           +(CS(I,J+1)-2.*CS(I,J)+CS(I,J-1))/(DELY*DELY) )
C          ADD TOPOGRAPHIC TERMS 1-18-89
     1     - (CD(I,J)/H(I,J))
     2          *2.*( UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     3               +VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
     4     + (CD(I,J)/H(I,J))
     5          *( UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     6            +VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
          FCD(I,J) =
     1     - UT(I,J)*(CD(I+1,J)-CD(I-1,J))/DX2
     2     - VT(I,J)*(CD(I,J+1)-CD(I,J-1))/DY2
     3     - UC(I,J)*(CS(I+1,J)-CS(I-1,J))/DX2
     4     - VC(I,J)*(CS(I,J+1)-CS(I,J-1))/DY2
     5     - CS(I,J)*(UC(I+1,J)-UC(I-1,J))/DX2
     6     - CS(I,J)*(VC(I,J+1)-VC(I,J-1))/DY2
     7     + LAMBDA*(QD(I,J)-CS(I,J)-CD(I,J))/H(I,J)
     8     - 8.*AV*CD(I,J)/H(I,J)**2
     9     + AH*( (CD(I+1,J)-2.*CD(I,J)+CD(I-1,J))/(DELX*DELX)
     1            +(CD(I,J+1)-2.*CD(I,J)+CD(I,J-1))/(DELY*DELY) )
     2     - WE(I,J)*CS(I,J)/H(I,J)
CU   3     - (2.*PSI(I,J)*CD(I,J)/H(I,J))* ( WE(I,J)
CU   4  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CU   5  + (UC(I,J)-UT(I,J))*(H(I+1,J)-H(I-1,J))/DX2
CU   6  + (VC(I,J)-VT(I,J))*(H(I,J+1)-H(I,J-1))/DY2     )
          FCD(I,J) = FCD(I,J)
C          ADD TOPOGRAPHY SEPARATELY TO AVOID PROBS WITH CONTINUATION
C             LINE LIMIT
     1  + UT(I,J)*(CS(I,J)-2.*CD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     2  + VT(I,J)*(CS(I,J)-2.*CD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
     3  - UC(I,J)*(2.*CS(I,J)-2.*CD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     4  - VC(I,J)*(2.*CS(I,J)-2.*CD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
 1010   CONTINUE
 1000 CONTINUE
C     EXTRAPOLATE THE INTERIOR VALUES:
      DO 3000 J = 2, NY-1
        DO 3010 I = 2, NX-1
          CS(I,J) = CS(I,J) + DELT*FCS(I,J)
          CD(I,J) = CD(I,J) + DELT*FCD(I,J)
 3010   CONTINUE
 3000 CONTINUE
C     NOW MUST APPLY THE BOUNDARY CONDITIONS.
C     CONDITION FOR THE Y=0 BNDY, NO FLUX: S(X,1) = S(X,2)
C                     Y = YMAX  ,          S(X,NY) = S(X,NY-1)
C         X = 0 BC CHANGED TO NO FLUX 5-26-88
C         X = XMAX BC CHANGED TO ROBIN 5-26-88.
C     BC:
      DO 4000 I = 1, NX-1
        CS(I,1)  = CS(I,2)
        CD(I,1)  = CD(I,2)
        CS(I,NY) = CS(I,NY-1)
        CD(I,NY) = CD(I,NY-1)
 4000 CONTINUE
      DO 4010 J = 1, NY
        CS(NX,J) = (CS(NX-1,J)+DELX*GAMMA*SREF)/(1.+GAMMA*DELX)
        CD(NX,J) = (CD(NX-1,J)+DELX*GAMMA*DREF)/(1.+GAMMA*DELX)
        CS(1,J)  = CS(2,J)
        CD(1,J)  = CD(2,J)
 4010 CONTINUE
      RETURN
      END
      SUBROUTINE STEXT(UC, VC, UT, VT, WE, SS, SD, QS, QD, H, LNH,
     1                 NX, NY, DELX, DELY, DELT, DREF, SREF, STREF,
     2                 GAMMA, ASH, ASV, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE THE SALINITY FIELD TO THE NEXT
C       TIME STEP.
C     MODIFIED (IN ARGUMENT NAMES) TO BE USED FOR THE TEMP. EXTRAP
C       AS WELL.  3-30-88, BG.
C     FORCING ARRAYS ENLARGED TO 60X60 BY 5-26-88.
C     DEL(US) COMPUTED AS D(US)/DX + D(VS)/DY 3-30-88.
C     EDDY DIFFUSION (LESS TOPOGRAPHIC TERMS) RE-ADDED 3-30-88
C     BC 3-29 TO 5-25-88:
C       Y = 0, YMAX D /DY = 0
C       X = 0  D /DX = GAMMA*(S -S0)
C       X = XMAX S = SREF (BOTH SD, SS)
C     CHANGE BC ON 5-26-88 AT X = 0,XMAX
C       X = 0, D /DX = 0
C       X = XMAX  D / DX = GAMMA*(A -A0)
C     TOPOGRAPHY ADDED 1-18-89
C     FORCING ARRAYS CUT BACK TO 40X40 1-24-89.
      INTEGER NX, NY, TSTEP
      REAL WE(NX, NY), UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL QS(NX, NY), QD(NX, NY), SS(NX, NY), SD(NX, NY)
      REAL H(NX, NY), LNH(NX, NY)
      REAL DELX, DELY, DELT, DREF, SREF, STREF, GAMMA, ASH, ASV
      INTEGER I, J
      REAL FSS(40, 40), FSD(40, 40)
      REAL DX2, DY2
      REAL PSI
      INTEGER K, L
      PSI(K,L) = 0.0
CD    PSI(K,L) = SIGN(1., WE(K,L)
CD   1  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CD   2  +  UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   3  +  VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2
CD   4  -  UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   5  -  VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2          )
      DX2 = 2.*DELX
      DY2 = 2.*DELY
C     COMPUTE FORCING FOR THE INTERIOR POINTS:
      DO 1000 J = 2, NY-1
        DO 1010 I = 2, NX-1
          FSS(I,J) =
C          ORIGINALLY USED DEL*(US), TRY S*(DELU)+ U*GRAD(S) INSTEAD.
C          RETURN TO THE DEL (US) METHOD. 3-30-88
C          VERSION FOR BAROTROPIC FLOW, WITH NO TOPOGRAPHY 8-15-88.
C              DEL(U) FIGURED 'ANALYTICALLY', CENTERED DIFFERENCE,
C              ISO HALINE/THERMAL MIXED LAYER 8-15-88.
     1     - UT(I,J)*(SS(I+1,J)-SS(I-1,J))/DX2
     2     - VT(I,J)*(SS(I,J+1)-SS(I,J-1))/DY2
     3     - UC(I,J)*(SD(I+1,J)-SD(I-1,J))/DX2
     4     - VC(I,J)*(SD(I,J+1)-SD(I,J-1))/DY2
     5     - SD(I,J)*(  (UC(I+1,J)-UC(I-1,J))/DX2
     6                + (VC(I,J+1)-VC(I,J-1))/DY2   )
     7     + QS(I,J)/H(I,J)
     8     - WE(I,J)*SD(I,J)/H(I,J)
C          ADD DIFFUSIVE TERMS 3-30-88
     9     + ASH*( (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))/(DELX*DELX)
     1            +(SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))/(DELY*DELY) )
C          ADD TOPOGRAPHIC TERMS 1-18-89
     1     - (SD(I,J)/H(I,J))
     2          *2.*( UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     3               +VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
     4     + (SD(I,J)/H(I,J))
     5          *( UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     6            +VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
          FSD(I,J) =
     1     - UT(I,J)*(SD(I+1,J)-SD(I-1,J))/DX2
     2     - VT(I,J)*(SD(I,J+1)-SD(I,J-1))/DY2
     3     - UC(I,J)*(SS(I+1,J)-SS(I-1,J))/DX2
     4     - VC(I,J)*(SS(I,J+1)-SS(I,J-1))/DY2
     5     - SS(I,J)*(UC(I+1,J)-UC(I-1,J))/DX2
     6     - SS(I,J)*(VC(I,J+1)-VC(I,J-1))/DY2
     7     + QD(I,J) / H(I,J)
     8     - 8.*ASV*SD(I,J)/H(I,J)**2
     9     + ASH*( (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))/(DELX*DELX)
     1            +(SD(I,J+1)-2.*SD(I,J)+SD(I,J-1))/(DELY*DELY) )
     2     - WE(I,J)*SS(I,J)/H(I,J)
CU   3     - (2.*PSI(I,J)*SD(I,J)/H(I,J))* ( WE(I,J)
CU   4  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CU   5  + (UC(I,J)-UT(I,J))*(H(I+1,J)-H(I-1,J))/DX2
CU   6  + (VC(I,J)-VT(I,J))*(H(I,J+1)-H(I,J-1))/DY2     )
          FSD(I,J) = FSD(I,J)
C          ADD TOPOGRAPHY SEPARATELY TO AVOID PROBS WITH CONTINUATION
C             LINE LIMIT
     1  + UT(I,J)*(SS(I,J)-2.*SD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     2  + VT(I,J)*(SS(I,J)-2.*SD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
     3  - UC(I,J)*(2.*SS(I,J)-2.*SD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     4  - VC(I,J)*(2.*SS(I,J)-2.*SD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
 1010   CONTINUE
 1000 CONTINUE
C     EXTRAPOLATE THE INTERIOR VALUES:
      DO 3000 J = 2, NY-1
        DO 3010 I = 2, NX-1
          SS(I,J) = SS(I,J) + DELT*FSS(I,J)
          SD(I,J) = SD(I,J) + DELT*FSD(I,J)
 3010   CONTINUE
 3000 CONTINUE
C     NOW MUST APPLY THE BOUNDARY CONDITIONS.
C     CONDITION FOR THE Y=0 BNDY, NO FLUX: S(X,1) = S(X,2)
C                     Y = YMAX  ,          S(X,NY) = S(X,NY-1)
C                     X = XMAX  , FIXED VALUE S(XMAX,Y) = DREF, SREF
C                     X = 0     , NO FLUX?? S(1,Y) = S(2,Y)
C         X=0 BC CHANGED TO A ROBIN CONDITION: DS/DX = GAMMA(S-STREF)
C           ON 3-29-88.  GAMMA IS SET AS INVERSE OF DISTANCE TRAVELED
C           BY CURRENT IN THE NON-DIMENSIONALIZATION TIME (1/FRO).
C         X=0 BC CHANGED TO NO FLUX 5-26-88
C         X = XMAX BC CHANGED TO ROBIN 5-26-88.
C     BC:
      DO 4000 I = 1, NX-1
        SS(I,1)  = SS(I,2)
        SD(I,1)  = SD(I,2)
        SS(I,NY) = SS(I,NY-1)
        SD(I,NY) = SD(I,NY-1)
 4000 CONTINUE
      DO 4010 J = 1, NY
        SS(NX,J) = (SS(NX-1,J)+DELX*GAMMA*SREF)/(1.+GAMMA*DELX)
        SD(NX,J) = (SD(NX-1,J)+DELX*GAMMA*DREF)/(1.+GAMMA*DELX)
        SS(1,J)  = SS(2,J)
        SD(1,J)  = SD(2,J)
 4010 CONTINUE
      RETURN
      END
      SUBROUTINE CONVEC(SS, SD, TS, TD, CD, NX, NY, SREF, TREF, TSTEP)
C     SUBROUTINE TO LOOK FOR CONVECTIVE OVERTURNING AND CABBELING.
C     OVERTURN ADDED 3-9-88.
C     USES SHORT FORM ENTRY POINT TO RHO 5-26-88.

      INTEGER NX, NY, TSTEP
      REAL SS(NX, NY), SD(NX, NY), TS(NX, NY), TD(NX, NY), CD(NX, NY)
      REAL SREF, TREF

      INTEGER I, J, TLAST
      REAL RHOS1P, RHO1, RHO2
      LOGICAL OVRTRN, OVRLST
      SAVE    OVRLST

      OVRTRN = .FALSE.
      DO 1000 J = 1, NY
        DO 1010 I = 1, NX
          RHO1 = RHOS1P(SS(I,J)+SD(I,J), TS(I,J)+TD(I,J), 0.)
          RHO2 = RHOS1P(SS(I,J)-SD(I,J), TS(I,J)-TD(I,J), 0.)
          IF (RHO1 .GT. RHO2) THEN
            SD(I,J) = 0.0
            TD(I,J) = 0.0
            CD(I,J) = 0.0
CD          PRINT *,'CONVECTIVE OVERTURN AT I,J',I,J,' TS=',TSTEP
            OVRTRN = .TRUE.
          ENDIF
 1010   CONTINUE
 1000 CONTINUE

CD    IF (OVRTRN) PRINT *,'CONVECTIVE OVERTURNING AT TS= ',TSTEP
      IF (TSTEP .EQ. 1) GO TO 9999

      IF (OVRTRN .AND. (.NOT. OVRLST) ) THEN
        PRINT *,'STARTED CONVECTION AT TSTEP',TSTEP
       ELSE IF ((.NOT. OVRTRN)  .AND. OVRLST ) THEN
        PRINT *,'STOPPED CONVECTION AT TSTEP',TSTEP
      ENDIF

 9999 CONTINUE
      OVRLST = OVRTRN

      RETURN
      END
      FUNCTION RHO(S, T, P)
C     COMPUTE THE DENSITY OF WATER AS A FUNCTION OF SALINITY,
C       TEMPERATURE, AND PRESSURE.
C     USE THE ENTIRE EQUATION OF STATE GIVEN IN GILL, 1982.
C     S IN PSU, T IN DEGREES C, P IN BARS

      REAL RHO
      REAL S, T, P

      DOUBLE PRECISION SIGMAW, SIGMA, SIGMAP
      DOUBLE PRECISION DELTAW, DELTA, DELTAP

CD    IF (ABS(T) .GT. 10. ) THEN
CD      PRINT *,'TEMPERATURE OUT OF RANGE.'
CD      STOP
CD     ELSE IF ( (S .GT. 40.) .OR. (S .LT. 30.) ) THEN
CD      PRINT *,'SALINITY OUT OF RANGE.'
CD      STOP
CD    ENDIF

CD    PRINT *,'SALINITY, TEMPERATURE ', S, T
      SIGMAW = -.157406 + T*(6.793952D-2 + T*(-9.09529D-3
     1      + T*(1.001685D-4 +T*(-1.120083D-6 + 6.536332D-9*T))))

      SIGMA  = SIGMAW +
     1    S* ( .824493 + T*(-4.0899D-3 + T*(7.6438D-5
     2                      + T*(-8.2467D-7 + T*5.3875D-9))) ) +
     3    S**1.5*( -5.72466D-3 + T*(1.0227D-4 - T*1.6546D-6)) +
     4    S*S   *( 4.8314D-4 )

C     NOW COMPUTE THE COMPRESSIBILITY TERMS:
      DELTAW = -347.79 + T*(148.4206 + T*(-2.327105 +
     1                     T*(-1.360477D-2 - T*5.155288D-5 )))

      DELTA  = DELTAW +
     1    S  * (54.676 + T*(-.603459 + T*(1.09987D-2 - T*6.167D-5)) )
     2  + S**1.5*( 7.944D-2 + T*(1.6483D-2 - T*5.3009D-4) )

      DELTAP = DELTA +
     1   P *  (3.239908+T*(1.43713D-3+T*(1.16092D-4-T*5.77905D-7)) )
     2  +P*S* (2.2838D-3 + T*(-1.0981D-5*T - T*1.6078D-6) )
     3  +P*S**1.5*( 1.91075D-4 )
     4  +P*P* (8.50935D-5 + T*(-6.12293D-6 + T*5.2787D-8) )
     5  +P*P*S*(-9.9348D-7 +T*(2.0816D-8 + T*9.1697D-10) )

C     NOW COMPUTE THE DENSITY:
      RHO = (1000. + SIGMA)/ (1 - P/(20000.+ DELTAP) )

      RETURN
      END

      FUNCTION RHOS1(S, T, P)
C     THIS IS AN APPROXIMATE REPRESENTATION OF RHO, FITTED OVER A
C       CHARACTERISTIC RANGE OF T, S TO THE COMPLETE EQN. OF STATE.
      REAL S, T, P, RHOS1
      REAL RHONOT, ALPHA, BETA, GAMMA
      PARAMETER (ALPHA = -8.0520E-2)
      PARAMETER (BETA  = -6.3285E-3)
      PARAMETER (GAMMA =  0.80112  )
      PARAMETER (RHONOT= 1029.2676 )
C     REFERENCE VALUES ARE T=+1.5, S=35.1

      RHOS1 = RHONOT + T*(ALPHA + BETA*T) + GAMMA*S

      RETURN
      END
      FUNCTION RHOS1P(S, T, P)
C     THIS IS AN APPROXIMATE REPRESENTATION OF RHO, FITTED OVER A
C       CHARACTERISTIC RANGE OF T, S TO THE COMPLETE EQN. OF STATE.
C     COMPUTE THE DEVIATION FROM THE REFERENCE DENSITY.
      REAL S, T, P, RHOS1P
      REAL ALPHA, BETA, GAMMA
      PARAMETER (ALPHA = -8.0520E-2)
      PARAMETER (BETA  = -6.3285E-3)
      PARAMETER (GAMMA =  0.80112  )
C     REFERENCE VALUES ARE T=+1.5, S=35.1, P = 250 M

      RHOS1P = T*(ALPHA + BETA*T) + GAMMA*S

      RETURN
      END
      FUNCTION NORM(X, Y, NP, L)
C     COMPUTE THE L NORM OF THE DIFFERENCE BETWEEN TWO VECTORS.
C     BG 12-20-88.

      INTEGER NP, L
      REAL X(NP), Y(NP), NORM

      INTEGER I
      REAL TEMA

      TEMA = 0.0
      IF (L .EQ. 1) THEN
        DO 1000 I = 1, NP
          TEMA = TEMA + X(I) - Y(I)
 1000   CONTINUE
        NORM = ABS(TEMA) / FLOAT(NP)
        RETURN
       ELSE IF (L .EQ. 2) THEN
        DO 2000 I = 1, NP
          TEMA = TEMA + (X(I) - Y(I)) * (X(I) - Y(I))
 2000   CONTINUE
        NORM = SQRT(TEMA)/FLOAT(NP)
        RETURN
       ELSE IF (L .EQ. 3) THEN
C       NOT A TRUE THREE NORM, ACTUALLY THE INFINITY NORM.
        DO 3000 I = 1, NP
          TEMA = AMAX1(TEMA, ABS(X(I)-Y(I)) )
 3000   CONTINUE
        NORM = TEMA
        RETURN
       ELSE
        NORM = TEMA
      ENDIF

      RETURN
      END
      SUBROUTINE QTEXT
C     COMPUTE THE TEMPERATURE FORCING FOR THE NEXT TIME STEP.
C     DUMMY ROUTINE
      RETURN
      END
      SUBROUTINE UWEXT
C     COMPUTE THE U COMPONENT OF THE WIND FIELD FOR THE NEXT TS.
C     DUMMY ROUTINE
      RETURN
      END
      SUBROUTINE VWEXT
C     COMPUTE THE V COMPONENT OF THE WIND FIELD FOR THE NEXT TS.
C     DUMMY ROUTINE
      RETURN
      END
      SUBROUTINE UVTEXT
C     DUMMY TO EXTRAPOLATE THE BAROTROPIC VELOCITY
      RETURN
      END

      SUBROUTINE READ2(X, NX, NY, UNIT, FNAME, OPE, CLOS)
C     READ IN A 2D, UNFORMATTED ARRAY, WITH EXTERNAL CONTROL ON
C       OPENING AND CLOSING.

      INTEGER NX, NY, UNIT
      REAL X(NX, NY)
      CHARACTER*6 FNAME
      LOGICAL OPE, CLOS

      IF (OPE) OPEN(UNIT, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
      READ (UNIT) X
      IF (CLOS) CLOSE(UNIT, STATUS='KEEP')

      RETURN
      END
      SUBROUTINE ARSET (X, NX, NY, VALUE)
C     SET ALL ELEMENTS OF ARRAY X EQUAL TO VALUE.

      INTEGER NX, NY
      REAL X(NX, NY), VALUE

      INTEGER I, J

      DO 1000 J = 1, NY
        DO 1010 I = 1, NX
          X(I,J) = VALUE
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
      FUNCTION YES(DEFALT)
C     FUNCTION TO RETURN .TRUE. IF THE USER RESPONDS Y, .FALSE. IF HE
C       SAYS N, AND THE DEFAULT VALUE OTHERWISE.

      LOGICAL YES, DEFALT
      CHARACTER RESP

      READ (*,9001) RESP
 9001 FORMAT(A1)

      YES = (RESP.EQ.'Y') .OR. (DEFALT .AND. RESP.NE.'Y'
     1                                 .AND. RESP.NE.'N')

      RETURN
      END
