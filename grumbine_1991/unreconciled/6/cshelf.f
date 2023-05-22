C***********************************************************----------!!
      PROGRAM CSHELF
C     MODEL BOUYANCY (AND PERHAPS WIND) FORCED CONTINENTAL SHELF
C       MOTIONS AND CONDITIONS FOR THE POLAR REGIONS.
      IMPLICIT none
      INCLUDE "grid.inc"

C     DATA ARRAYS
      REAL UC(nx, ny), VC(nx, ny), UT(nx, ny), VT(nx, ny)
      REAL SS(nx, ny), SD(nx, ny), QSS(nx, ny), QSD(nx, ny)
      REAL WE(nx, ny), H(nx, ny)

C     PHYSICAL PARAMETERS
      REAL ahm, AVM, AHS, AVS
      REAL SDREF, SSREF, RHOREF
      REAL G, F, BETA
      INTEGER XMIN, XMAX, YMIN, YMAX, STRSPR, STRSUM, STRFLL, STRWIN
      REAL QSFMAX, QSFREF, QSM

C     NUMERICAL PARAMETERS
      REAL delx, dely, DELT
      INTEGER NOUT, NFLOUT, NTOT, LOY, TAV
      REAL SCRIT

C     LOCAL VARIABLES
      INTEGER I, J
      CHARACTER*60 FNAME
C     CONSERVATION TEST VARIABLES
      DOUBLE PRECISION S1, S2
      REAL href

C***********************************************************----------!!
C     BEGIN EXECUTION

C     INITIALIZE THE VARIABLES
      CALL INIT (UC, VC, UT, VT, SS, SD, WE, H, nx, ny,
     2           ahm, AVM, AHS, AVS,
     3           SDREF, SSREF, RHOREF, G, F,
     4           delx, dely, DELT,
     5           NOUT, NFLOUT, NTOT, SCRIT, TAV,
     6           XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     7           STRSPR, STRSUM, STRFLL, STRWIN, LOY,
     8           BETA                                         )

      I = 0
      href = H(1,1)
C     OPEN THE OUTPUT FILES
      CALL OUTSTR(UC, VC, UT, VT, SS, SD, H, delx           )
      CALL UVTROP(UT, VT, WE, href, delx, dely, F, BETA, ahm)

      CALL HEADER(WE, H,
     1                ahm, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                delx, dely, DELT,
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,
     4                XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )

      OPEN (1, FILE='TIMEINFO', FORM='FORMATTED', STATUS='NEW')
CMAC      PRINT *,LONG(362)
CMAC      WRITE(1,9001) 0, LONG(362)
 9001 FORMAT (2I12)
C     EXTRAPOLATION LOOP
      DO 1000 I = 1, NTOT
CMAC        WRITE(*,9001) I, LONG(362)
CMAC        WRITE(1,9001) I, LONG(362)
        CALL UVEXT (UC, VC, SS, H,
     2              RHOREF, G, F, BETA, ahm, delx, dely         )

        CALL QSEXT (SS, QSS, QSD, WE, H, nx, ny, I, LOY,
     1                 XMIN, XMAX, YMIN, YMAX,
     2                 QSFMAX, QSFREF, QSM, delx, dely,
     3                 STRSPR, STRSUM, STRFLL, STRWIN, DELT )

        CALL STEXT (UC, VC, UT, VT, WE, SS, SD, QSS, QSD, H,
     1              delx, dely, DELT, SDREF, SSREF,
     2              AHS, AVS, I                                 )

        CALL CONVEC(SD, nx, ny, I)

CD      WRITE (*,9002)  I, CHAR(9), S1, CHAR(9), S2
        IF (MOD(I,20) .EQ. 0) THEN
          CALL SUMMER(SS, nx, ny, S1)
          CALL SUMMER(SD, nx, ny, S2)
          WRITE (1,9002)  I, CHAR(9), S1, CHAR(9), S2
        ENDIF

        CALL TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, I,
     1                     delx, dely                            )

        IF (MOD(I,NOUT) .EQ. 0) THEN
           CALL OUTDAT (UC, VC, UT, VT, SS, SD, H, delx          )
           PRINT *,'TSTEP=',I
        ENDIF
        IF (MOD(I,NFLOUT) .EQ. 0)
     1      CALL OUTFL (UC, VC, UT, VT, SS, SD, H, delx          )
 1000 CONTINUE

      CALL OUTEND (UC, VC, UT, VT, SS, SD, H, delx               )

 9002 FORMAT (I5,A1,D20.14,A1,D20.14)


      END
