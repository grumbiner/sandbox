      PROGRAM CSHELF
C     MODEL BOUYANCY (AND PERHAPS WIND) FORCED CONTINENTAL SHELF
C       MOTIONS AND CONDITIONS FOR THE POLAR REGIONS.

      INTEGER NX, NY
      PARAMETER (NX = 36, NY = 36)

C     DATA ARRAYS
      REAL UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), QSS(NX, NY), QSD(NX, NY)
      REAL WE(NX, NY), H(NX, NY)

C     PHYSICAL PARAMETERS
      REAL AHM, AVM, AHS, AVS
      REAL SDREF, SSREF, RHOREF
      REAL G, F, BETA
      INTEGER XMIN, XMAX, YMIN, YMAX, STRSPR, STRSUM, STRFLL, STRWIN
      REAL QSFMAX, QSFREF, QSM

C     NUMERICAL PARAMETERS
      REAL DELX, DELY, DELT
      INTEGER NOUT, NFLOUT, NTOT, LOY, TAV
      REAL SCRIT

C     LOCAL VARIABLES
      INTEGER I
      CHARACTER*60 FNAME
C     CONSERVATION TEST VARIABLES
      DOUBLE PRECISION S1, S2, S3, S4, S5, S6, S7, S8, S1OLD

C***********************************************************----------!!
C     BEGIN EXECUTION

C     INITIALIZE THE VARIABLES
      CALL INIT (UC, VC, UT, VT, SS, SD, WE, H, NX, NY,
     2           AHM, AVM, AHS, AVS,
     3           SDREF, SSREF, RHOREF, G, F,
     4           DELX, DELY, DELT,
     5           NOUT, NFLOUT, NTOT, SCRIT, TAV,
     6           XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     7           STRSPR, STRSUM, STRFLL, STRWIN, LOY,
     8           BETA                                         )
      I = 0
      S1OLD = 0.D0

C     OPEN THE OUTPUT FILES
CD    OPEN (1, FILE='QOUT', FORM='UNFORMATTED', STATUS='NEW')
      CALL OUTSTR(UC, VC, UT, VT, SS, SD, H, DELX, NX, NY)
      CALL UVTROP(UT, VT, WE, H, NX, NY, DELX, DELY, F, BETA, AHM)
      CALL HEADER(WE, UT, VT, H, NX, NY,
     1                AHM, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                DELX, DELY, DELT,
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,
     4                XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )
CM      OPEN (1, FILE='TIMEINFO', FORM='FORMATTED', STATUS='NEW')
CM      PRINT *,LONG(362)
CM      WRITE(1,9001) 0, LONG(362)
CM 9001 FORMAT (2I12)
C     EXTRAPOLATION LOOP
      DO 1000 I = 1, NTOT
CM        WRITE(*,9001) I, LONG(362)
CM        WRITE(1,9001) I, LONG(362)
        CALL UVEXT (UC, VC, SS, SD, H, NX, NY,
     2              RHOREF, G, F, AHM, AVM, DELX, DELY, DELT    )

        CALL QSEXT (QSS, QSD, H, WE, NX, NY, I, LOY,
     1              XMIN, XMAX, YMIN, YMAX,
     2              QSFMAX, QSFREF, QSM, DELX, DELY,
     3              STRSPR, STRSUM, STRFLL, STRWIN              )
CD      WRITE (1) QSS

CD      CALL SCONV (UC, VC, UT, VT, WE, SS, SD, QSS, QSD, H,
CD   1              NX, NY, DELX, DELY, DELT, SDREF, SSREF,
CD   2              AHS, AVS, I, S1, S2,
CD   3              S3, S4, S5, S6, S7, S8                      )
CD      WRITE (*,9015) I, (S1-S1OLD)/DELT, S2, S1,
CD   1                    S3, S4, S5, S6, S7, S8
CD      S1OLD = S1

        CALL STEXT (UC, VC, UT, VT, WE, SS, SD, QSS, QSD, H,
     1              NX, NY, DELX, DELY, DELT, SDREF, SSREF,
     2              AHS, AVS, I                                 )

        CALL CONVEC(SS, SD, NX, NY, I)
        CALL TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, I,
     1                     NX, NY, DELX, DELY                    )

        IF (MOD(I,NOUT) .EQ. 0) THEN
           CALL OUTDAT (UC, VC, UT, VT, SS, SD, H, DELX, NX, NY  )
           PRINT *,'TSTEP=',I
        ENDIF
        IF (MOD(I,NFLOUT) .EQ. 0)
     1      CALL OUTFL (UC, VC, UT, VT, SS, SD, H, DELX, NX, NY  )
 1000 CONTINUE

      CALL OUTEND (UC, VC, UT, VT, SS, SD, H, DELX, NX, NY       )

 9015 FORMAT (I4,3D16.10,/,6D12.6)

      END
