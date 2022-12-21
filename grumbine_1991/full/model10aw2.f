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
C***********************************************************----------!!
      SUBROUTINE HEADER(WE, UT, VT, H, NX, NY,
     1                AHM, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                DELX, DELY, DELT,
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,
     4                XCEN, XLEN, YCEN, YLEN, QFMAX, QFREF, QM,
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )
C     COMPUTE THE NONDIMENSIONAL PARAMETERS USED IN DESCRIBING THE
C        EXPERIMENTS.  2-1-90. BG

      REAL SECPYR
      PARAMETER (SECPYR = 3.1556908E7)
      REAL PI
      PARAMETER (PI = 3.141592654)

      INTEGER NX, NY
      REAL WE(NX, NY), H(NX, NY)
      REAL UT(NX, NY), VT(NX, NY)
      REAL AHM, AVM, AHS, AVS
      REAL SREF, SDREF, SSREF, RHOREF
      REAL G, F
      REAL DELX, DELY, DELT
      INTEGER NOUT, NFLOUT, NTOT, TAV
      REAL SCRIT

C     PARAMETERS FOR BUOYANCY FORCING
      INTEGER XCEN, XLEN, YCEN, YLEN
      REAL QFMAX, QFREF, QM
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY

C     PARMS FOR BAROTROPIC FLOW
      REAL BETA

      REAL GAMMA, ETA(18), DELS, CFLUX, TFLUX(18)
      REAL C, TOTQ, SUMW
      INTEGER NSCRIT, I, J, K
      REAL XNOT, SIGX, YNOT, SIGY, DX, DY

      XNOT = FLOAT(XCEN)
      SIGX = FLOAT(XLEN)
      YNOT = FLOAT(YCEN)
      SIGY = FLOAT(YLEN)
      C = 0.809
CD    PRINT *,QFMAX, QFREF, XNOT, SIGX, YNOT, SIGY
CD    PRINT *, H(1,1), WE(9,9)

      NSCRIT = 12
      DELS = QFMAX*STRSPR*DELT*EXP(-1.4**2/2-0.675**2/2)
     1            /H(NX/2,NY/2)
      CFLUX = G*H(1,1)**2*C/8/RHOREF/F * DELS
C***********************************************************----------!!
      TOTQ = 0.0
      DO 100 J = 2, NY
        DO 110 I = 1, NX
          TOTQ = TOTQ+QFREF+  EXP(-(I*DELX-XNOT)**2/2./SIGX**2
     1                            -(J*DELY-YNOT)**2/2./SIGY**2 )*QFMAX
 110    CONTINUE
 100  CONTINUE
      TOTQ = TOTQ * DELX*DELX
      WRITE (56, 9001) DELS, CFLUX, TOTQ
      DO 1000 I = 1, NSCRIT
        GAMMA = STRSPR*DELT*(QFMAX+QFREF)/H(1,1)
     1          /(SCRIT+0.005*(I-1))
        SUMW = 0.0
        DO 1100 J = 2, NY/2
          SUMW = 0.5*(MAX(0.0,VT(1,J))+MAX(0.0,VT(NX,J)))
          DO 1200 K = 2, NX-1
            SUMW = SUMW+MAX(0.0,VT(K,J))
 1200     CONTINUE
C         ETA(J) = (STRSPR/LOY)*(BETA/F)/((SCRIT+0.005*(I-1)))
C    1            *TOTQ /SUMW/DELX
          TFLUX(J) = SUMW*DELX*H(NX/2,NY/2)
          ETA(J) = STRSPR*TOTQ/(LOY*TFLUX(J)*
     1                (SCRIT+0.005*(I-1))                    )
 1100   CONTINUE
        IF (I.EQ.1) WRITE (56, 9001)(ABS(TFLUX(J)),J=2,NY/2)
        WRITE (56, 9002) GAMMA,(ABS(ETA(J)),J=2,NY/2)
 1000 CONTINUE

 9001 FORMAT (6E12.3)
 9002 FORMAT (10F7.3)
      RETURN
      END
      SUBROUTINE TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, TSTEP,
     1                     NX, NY, DELX, DELY)
C     PROGRAM TO COMPUTE AVERAGED QUANTITIES FROM MODEL OUTPUT
C     VERSION MODIFIED TO WORK 'ON THE FLY'.  ORIGINAL
C       ASSUMED THAT THE FILES WERE ALREADY WRITTEN AND MERELY
C       NEEDED AVERAGING.  NOW WORK WITH DATA AS IT IS BEING GENERATED.
C       BG 9-29-89
      INTEGER NX, NY, NNX, NNY
      PARAMETER (NNX = 36, NNY = 36)

      REAL UC(NX, NY), VC(NX, NY), SS(NX, NY), SD(NX, NY)
      REAL UT(NX, NY), VT(NX, NY), H(NX, NY)
      REAL SCRIT, DELX, DELY

      INTEGER NSCRIT
      PARAMETER (NSCRIT = 12)
      REAL DELSCRIT
      PARAMETER (DELSCRIT = 0.005)
      REAL FTAV
      INTEGER I, J, K, L
      INTEGER TSTEP, TAV
      CHARACTER*60 FNAME
      REAL R1(NNX, NNY), R2(NNX, NNY), R3(NNX, NNY), R4(NNX, NNY)
      REAL R5(NNX, NNY), R6(NNX, NNY)
      REAL F1(NNY,NSCRIT), F2(NNY,NSCRIT)
      REAL F1T(NNY,NSCRIT), F2T(NNY,NSCRIT)
      SAVE R1, R2, R3, R4, R5, R6, F1, F2, F1T, F2T

C***********************************************************----------!!

C     NUMBER OF STEPS TO AVERAGE OVER IS TAV.
      FTAV = FLOAT(TAV)
      IF (TSTEP .EQ. 1) THEN
        DO 1040 K = 1, NY
          DO 1050 L = 1, NX
            R1(L,K) = 0.0
            R2(L,K) = 0.0
            R3(L,K) = 0.0
            R4(L,K) = 0.0
            R5(L,K) = 0.0
            R6(L,K) = 0.0
 1050     CONTINUE
 1040   CONTINUE
        DO 1060 K = 1, NY
          DO 1070 L = 1, NSCRIT
            F1(K,L) = 0.0
            F2(K,L) = 0.0
 1070     CONTINUE
 1060   CONTINUE

       ELSE

        DO 1900 I = 1, NSCRIT
         CALL REFLUX(VT, VC, SS, SD, H, F1T(1,I), F2T(1,I),
     1                SCRIT+(I-1)*DELSCRIT, NX, NY, DELX, DELY)
          DO 1910 K = 1, NY
             F1(K,I) = F1(K,I)+F1T(K,I)
             F2(K,I) = F2(K,I)+F2T(K,I)
 1910     CONTINUE
 1900   CONTINUE

        DO 2000 K = 1, NY
          DO 2010 L = 1, NX
            R1(L,K) = R1(L,K) + UC(L,K)
            R2(L,K) = R2(L,K) + VC(L,K)
            R3(L,K) = R3(L,K) + UT(L,K)
            R4(L,K) = R4(L,K) + VT(L,K)
            R5(L,K) = R5(L,K) + SS(L,K)
            R6(L,K) = R6(L,K) + SD(L,K)
 2010     CONTINUE
 2000   CONTINUE

        IF ( MOD(TSTEP,TAV) .EQ. 0) THEN
          DO 2020 K = 1, NY
            DO 2030 L = 1, NX
              R1(L,K) = R1(L,K) / FTAV
              R2(L,K) = R2(L,K) / FTAV
              R3(L,K) = R3(L,K) / FTAV
              R4(L,K) = R4(L,K) / FTAV
              R5(L,K) = R5(L,K) / FTAV
              R6(L,K) = R6(L,K) / FTAV
 2030       CONTINUE
            DO 2040 I = 1, NSCRIT
       F1(K,I) = F1(K,I) / FTAV / 1.E5
              F2(K,I) = F2(K,I) / FTAV / 1.E5
 2040       CONTINUE
 2020     CONTINUE
          WRITE (50) R1
          WRITE (51) R2
          WRITE (52) R3
          WRITE (53) R4
          WRITE (54) R5
          WRITE (55) R6
          DO 2100 J = 1, NSCRIT
           WRITE (56,9009) (F1(I,J),I=1,NY)
           WRITE (57,9009) (F2(I,J),I=1,NY)
 2100   CONTINUE
         WRITE (56,9010)
          WRITE (57,9010)
          DO 2060 K = 1, NY
            DO 2050 L = 1, NX
              R1(L,K) = 0.0
              R2(L,K) = 0.0
              R3(L,K) = 0.0
              R4(L,K) = 0.0
              R5(L,K) = 0.0
              R6(L,K) = 0.0
 2050       CONTINUE
      DO 2070 I = 1, NSCRIT
              F1(K,I) = 0.0
              F2(K,I) = 0.0
 2070       CONTINUE
 2060     CONTINUE
        ENDIF

      ENDIF

 9001 FORMAT (BN, I5)

 9002 FORMAT (A60)

 9009 FORMAT (16F5.2)

 9010 FORMAT (' END OF STEP')

      RETURN
      END
      SUBROUTINE REFLUX(VT, VC, SS, SD, H, FLM, FLS,
     1                          SCRIT, NX, NY, DX, DY)
C     RECOMPUTE FLUXES USING A BOTTOM WATER DEFINITION

      INTEGER NX, NY
      REAL VT(NX, NY), VC(NX, NY), SS(NX, NY), SD(NX, NY)
      REAL H(NX, NY), FLM(NY), FLS(NY)
      REAL DX, DY, SCRIT
      INTEGER I, J

      DO 1000 J = 1, NY
        FLM(J) = 0.0
        FLS(J) = 0.0
        DO 1010 I = 1, NX
C         LOWER LAYER
          IF (VT(I,J)-VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)-SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + H(I,J)*0.5*(VT(I,J) - VC(I,J))
              FLS(J) = FLS(J) +H(I,J)*0.5*
     1                    (VT(I,J) - VC(I,J))*(SS(I,J)-SD(I,J))
            ENDIF
          ENDIF
C         UPPER LAYER
          IF (VT(I,J)+VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)+SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + H(I,J)*0.5*(VT(I,J) + VC(I,J))
              FLS(J) = FLS(J) +H(I,J)*0.5*
     1                    (VT(I,J) + VC(I,J))*(SS(I,J)+SD(I,J))
            ENDIF
          ENDIF
 1010   CONTINUE
        FLM(J) = FLM(J)*DX
        FLS(J) = FLS(J)*DX
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE UVTROP(UT, VT, WE, H, NX, NY, DX, DY, F, BETA, AM)
C     BAROTROPIC SOLUTION.
      INTEGER NX, NY
      REAL UT(NX, NY), VT(NX, NY), WE(NX, NY), H(NX, NY)
      REAL DX, DY, F, BETA, AM

      INTEGER NNX, NNY
      PARAMETER (NNX = 36)
      PARAMETER (NNY = 36)
      REAL PSI(NNX, NNY)
      REAL NUM, LB, WAVE
      INTEGER I, J

C     COMPUTE THE INTERIOR SOLUTION
      DO 1000 J = 1, NY
        PSI(NX, J) = WE(NX, J)
        DO 1010 I = NX-1, 1, -1
          PSI(I, J) = PSI(I+1,J) + WE(I+1,J) + WE(I,J)
 1010   CONTINUE
 1000 CONTINUE
      NUM = F*DX/2./BETA/H(1,1)
      DO 1020 J = 1, NY
        DO 1030 I= 1, NX
          PSI(I,J) = -PSI(I,J) * NUM
 1030   CONTINUE
 1020 CONTINUE

C     APPLY THE BOUNDARY LAYER CORRECTIONS.
      LB = (AM/BETA)**(1./3.)
      WAVE = SQRT(3.)*DX/2./LB
      DO 2000 J = 1, NY
        NUM = LB*(PSI(NX,J)-PSI(NX-1,J))/DX
        DO 2010 I = 1, NX
          PSI(I,J) = PSI(I,J)*(1.-EXP(-(I-1)*DX/2./LB)*
     1                 (COS(WAVE*(I-1)) + SIN(WAVE*(I-1))/SQRT(3.) ))
     2              - NUM*EXP( (I-NX)*DX/LB)
 2010   CONTINUE
 2000 CONTINUE

C     NOW THAT WE HAVE PSI, COMPUTE UT, VT.
      DO 3000 J = 2, NY-1
        DO 3010 I = 2, NX-1
          UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY
          VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX
 3010   CONTINUE
 3000 CONTINUE
      DO 3020 J = 2, NY-1
        I = 1
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY
CD      VT(I,J) =  (PSI(I+1,J)-PSI(I  ,J))   /DX
        VT(I,J) = 0.0
        I = NX
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY
        VT(I,J) =  (PSI(I  ,J)-PSI(I-1,J))   /DX
 3020 CONTINUE
      DO 3030 I = 2, NX-1
        J = 1
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J))/DY
        VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX
        J = NY
        UT(I,J) = -(PSI(I,J)-PSI(I,J-1))/DY
        VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX
 3030 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE INIT(UB, VB, UT, VT, SS, SD, WE, H, NX, NY,
     1                AHM, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                DELX, DELY, DELT,
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,
     4                XMIN, XMAX, YMIN, YMAX, QFMAX, QFREF, QM,
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )
C     INITIALIZE THE DATA ARRAYS AND CONSTANTS

      REAL SECPYR
      PARAMETER (SECPYR = 3.1556908E7)
      REAL PI
      PARAMETER (PI = 3.141592654)

      INTEGER NX, NY
      REAL UB(NX, NY), VB(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL SS(NX, NY), SD(NX, NY)
      REAL WE(NX, NY), H(NX, NY)

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
CI    PRINT *,'WHAT IS THE NAME OF THE PARAMETER FILE?'
CI    READ (*,9001) FNAME
CI    WRITE (*,9001) FNAME
      OPEN (11, FILE='MODEL9IN', FORM='FORMATTED', STATUS='OLD')
C***********************************************************----------**
C     GET ARRAY DATA:

      PRINT *,'WOULD YOU LIKE TO USE OUTPUT FROM AN OLD RUN?'
      IF (YES(.FALSE.)) THEN
        PRINT *,'WHAT IS THE FILE NAME FOR UB?'
        READ (11,9001) FNAME
        OPEN (12, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'WHAT IS THE FILE NAME FOR VB?'
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
          READ (12) UB
          READ (13) VB
          READ (14) UT
          READ (15) VT
          READ (16) SS
          READ (17) SD
 1000   CONTINUE

       ELSE
        WRITE (*,9011) 'UB   '
        READ (11,9002) VALUE
        CALL ARSET(UB, NX, NY, VALUE)
        WRITE (*,9011) 'VB   '
        READ (11,9002) VALUE
        CALL ARSET(VB, NX, NY, VALUE)
        WRITE (*,9011) 'UT   '
        READ (11,9002) VALUE
        CALL ARSET(UT, NX, NY, VALUE)
        WRITE (*,9011) 'VT   '
        READ (11,9002) VALUE
        CALL ARSET(VT, NX, NY, VALUE)
        WRITE (*,9011) 'SS   '
        READ (11,9002) VALUE
        CALL ARSET(SS, NX, NY, VALUE)
        WRITE (*,9011) 'SD   '
        READ (11,9002) VALUE
        CALL ARSET(SD, NX, NY, VALUE)
      ENDIF

CU    PRINT *,'WHAT IS THE NAME OF THE EKMAN PUMPING FILE?'
CU      READ (*,9001) FNAME
CU      CALL READ2(WE, NX, NY, 2, FNAME, .TRUE., .TRUE.)
      WRITE (*,9011) 'WE   '
      READ (11,9002) VALUE
      PRINT *,'WHAT IS THE WAVE LENGTH IN X?'
      READ (11,9002) A
      PRINT *,'WHAT IS THE WAVE LENGTH IN Y?'
      READ (11,9002) B
      DO 7000 J = 0, NY-1
        DO 7010 I = 0, NX-1
          WE(I+1,J+1) = VALUE*SIN(2.*PI*I/A)*SIN(2.*PI*J/B)
CD           WE(I+1,J+1) = VALUE*SIN(2.*PI*J/B)
 7010   CONTINUE
 7000 CONTINUE

      WRITE (*,9011) 'H    '
      READ (11,9002) VALUE
      CALL ARSET(H, NX, NY, VALUE)

 9011 FORMAT (' WHAT IS THE VALUE OF ',A5,'?')

C***********************************************************----------**
CU    PRINT *,'WHAT IS THE NAME OF THE SCALAR PARAMETER FILE?'
CU    READ (*,9001) FNAME
CU    WRITE (*,9001) FNAME
CU    OPEN (11, FILE=FNAME, FORM='FORMATTED', STATUS='OLD')
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
CU    CLOSE (11, STATUS='KEEP')

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
C***********************************************************----------!!
      SUBROUTINE OUTDAT (UC, VC, UT, VT, SS, SD, H, DX, NX, NY       )

C     WRITE OUT THE VELOCITY, SALINITY, AND TEMPERATURE.
      INTEGER NFIELD
      PARAMETER (NFIELD = 6)

      INTEGER NX, NY
      REAL UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), H(NX, NY)
      REAL DX

      INTEGER I, J
      REAL UCTEMP, VCTEMP
      INTEGER NFL
      PARAMETER (NFL = 36)
      REAL FLM(NFL), FLS(NFL)

      CHARACTER*60 FNAME(NFIELD)
      SAVE FLM, FLS, FNAME

      UCTEMP  = UC(1,1)
      VCTEMP  = VC(1,1)
      UC(1,1) = 0.0125
      VC(1,1) = 0.0125
      WRITE (30) UC
      WRITE (31) VC
      WRITE (32) UT
      WRITE (33) VT
      WRITE (34) SS
      WRITE (35) SD

      UC(1,1) = UCTEMP
      VC(1,1) = VCTEMP

      RETURN

      ENTRY OUTFL(UC, VC, UT, VT, SS, SD, H, DX, NX, NY)

      DO 1000 J = 1, NY
        FLM(J) = 0.0
        FLS(J) = 0.0
        DO 1010 I = 1, NX
          FLM(J) = FLM(J) + 0.5*H(I,J)*DX*(VT(I,J) - VC(I,J))
          FLS(J) = FLS(J) + 0.5*H(I,J)*DX*
     1                (VT(I,J) - VC(I,J))*(SS(I,J) - SD(I,J))
 1010   CONTINUE
 1000 CONTINUE

      WRITE (40) FLM
      WRITE (41) FLS

      RETURN

      ENTRY OUTSTR(UC, VC, UT, VT, SS, SD, H, DX, NX, NY)

C     OPEN THE NECESSARY OUTPUT FILES
      DO 2000 I = 1, NFIELD
        PRINT *,'WHAT IS THE NAME OF INSTANTANEOUS OUTPUT FILE # ',I
        READ  (11,9002) FNAME(I)
        WRITE (*,9002) FNAME(I)
        OPEN (29+I, FILE=FNAME(I), FORM='UNFORMATTED', STATUS='NEW')
 2000 CONTINUE
      PRINT *,'NAME FOR THE MASS FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
      OPEN (40, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'NAME FOR THE SALT FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
      OPEN (41, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')
C     AVERAGED OUTPUT FILES
      DO 2010 I = 1, NFIELD
        PRINT *,'WHAT IS THE NAME OF AVERAGED OUTPUT FILE # ',I
        READ  (11,9002) FNAME(I)
        WRITE (*,9002) FNAME(I)
        OPEN (49+I, FILE=FNAME(I), FORM='UNFORMATTED', STATUS='NEW')
 2010 CONTINUE
      PRINT *,'NAME FOR THE MASS FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
      OPEN (56, FILE=FNAME(1), FORM='FORMATTED', STATUS='NEW')
      PRINT *,'NAME FOR THE SALT FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
      OPEN (57, FILE=FNAME(1), FORM='FORMATTED', STATUS='NEW')
 9002 FORMAT (A60)

      RETURN

      ENTRY OUTEND(UC, VC, UT, VT, SS, SD, H, DX, NX, NY)

C     CLOSE THE DATA FILES:
      CLOSE (30, STATUS='KEEP')
      CLOSE (31, STATUS='KEEP')
      CLOSE (32, STATUS='KEEP')
      CLOSE (33, STATUS='KEEP')
      CLOSE (34, STATUS='KEEP')
      CLOSE (35, STATUS='KEEP')
      CLOSE (40, STATUS='KEEP')
      CLOSE (41, STATUS='KEEP')
      CLOSE (50, STATUS='KEEP')
      CLOSE (51, STATUS='KEEP')
      CLOSE (52, STATUS='KEEP')
      CLOSE (53, STATUS='KEEP')
      CLOSE (54, STATUS='KEEP')
      CLOSE (55, STATUS='KEEP')
      CLOSE (56, STATUS='KEEP')
      CLOSE (57, STATUS='KEEP')

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE UVEXT (UB, VB, SS, SD, H, NX, NY,
     1                  RHOREF, G, F, AHM, AVM, DELX, DELY, DELT)
C     EXTRAPOLATE U, V TO THE NEXT TIME LEVEL
C     COMPUTATION OF COMMON CONSTANTS ADDED PRIOR TO 5-26-88.
C     VERSION REWRITTEN FOR GEOSTROPHY, A LA DERIVATION. 4-5-89.
C       MUCH COMMENTED PROGRAM DELETED 4-5-89.

      INTEGER NX, NY
      REAL UB(NX, NY), VB(NX, NY)
      REAL SS(NX, NY), SD(NX, NY)
      REAL H(NX, NY)
      REAL RHOREF, F, G, AHM, AVM
      REAL DELX, DELY, DELT

      INTEGER NNX, NNY
      PARAMETER (NNX = 36)
      PARAMETER (NNY = 36)
      REAL RHOS1P, RHOS(NNX, NNY)
      INTEGER I, J, K, L
C     PARAMS FOR SPEEDIER NUMERICS:
      REAL DX2, DY2, G8RREF

C     COMPUTE PARAMS FOR SPEEDIER NUMERICS:
      DX2    = 2.*DELX
      DY2    = 2.*DELY
      G8RREF = G*H(1,1)/4./RHOREF/F

C     COMPUTE THE DENSITY FIELD BEFORE ENTERING THE EXTRAPOLATION.
C     THIS REDUCES THE NUMBER OF CALLS TO THE DENSITY FUNCTION BY
C       ALMOST A FACTOR OF 4.  8-4-88.
      DO 900 L = 1, NY
        DO 910 K = 1, NX
            RHOS(K,L) = RHOS1P( SS(K,L), 0.0, 0.0)
  910   CONTINUE
  900 CONTINUE

C     COMPUTE THE GEOSTROPHIC VELOCITY
      DO 1000 J = 2, NY-1
        DO 1010 I = 2, NX-1

          UB(I,J) = +G8RREF*( RHOS(I,J+1) - RHOS(I,J-1) )/DY2
          VB(I,J) = -G8RREF*( RHOS(I+1,J) - RHOS(I-1,J) )/DX2

 1010   CONTINUE
 1000 CONTINUE

C     NOW CONSIDER THE BOUNDARY CONDITIONS:
C       1-26-89:
C         AT I = 1         U = V = 0.0
C         AT I = NX        U = V = 0.0
C         AT J = 1         U = V = 0.0
C         AT J = NY NORMAL DERIV = 0.0

      DO 2000 I = 1, NX
C       BC ON V AT THE Y BOUNDARIES
        VB(I,1)  = 0.0
        VB(I,NY) = VB(I, NY-1)
C       BC ON U AT THE Y BOUNDARIES
        UB(I,1)  = 0.0
        UB(I,NY) = UB(I, NY-1)
 2000 CONTINUE

      DO 2010 J = 1, NY
C       V = 0.0 IMPLEMENTED 1-26-89
        VB(1,  J) = 0.0
        VB(NX, J) = 0.0
C       U = 0.0 IMPLEMENTED 1-26-89
        UB(1,  J) = 0.0
        UB(NX, J) = 0.0
 2010 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      FUNCTION RHO(S, T, P)
C     COMPUTE THE DENSITY OF WATER AS A FUNCTION OF SALINITY,
C       TEMPERATURE, AND PRESSURE.
C     USE THE ENTIRE EQUATION OF STATE GIVEN IN GILL, 1982.
C     S IN PSU, T IN DEGREES C, P IN BARS

      REAL RHO
      REAL S, T, P

      DOUBLE PRECISION SIGMAW, SIGMA, SIGMAP
      DOUBLE PRECISION DELTAW, DELTA, DELTAP

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
      FUNCTION RHOS1P(S, T, P)
C     THIS IS AN APPROXIMATE REPRESENTATION OF RHO, FITTED OVER A
C       CHARACTERISTIC RANGE OF T, S TO THE COMPLETE EQN. OF STATE.
C     COMPUTE THE DEVIATION FROM THE REFERENCE DENSITY.
      REAL S, T, P, RHOS1P
      REAL ALPHA, BETA, GAMMA
      PARAMETER (ALPHA = -4.5795E-2)
      PARAMETER (BETA  = -6.8927E-3)
      PARAMETER (GAMMA =  0.80908  )
C     REFERENCE VALUES ARE T= -0.5, S=34.6, P = 0.0

      RHOS1P = T*(ALPHA + BETA*T) + GAMMA*S

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE CONVEC(SS, SD, NX, NY, TSTEP)
C     SUBROUTINE TO LOOK FOR CONVECTIVE OVERTURNING AND CABBELING.
C     OVERTURN ADDED 3-9-88.
C     USES SHORT FORM ENTRY POINT TO RHO 5-26-88.

      INTEGER NX, NY, TSTEP
      REAL SS(NX, NY), SD(NX, NY)

      INTEGER I, J
      LOGICAL OVRTRN, OVRLST
      SAVE    OVRLST

      OVRTRN = .FALSE.
      DO 1000 J = 1, NY
        DO 1010 I = 1, NX
CF        RHO1 = RHOS1P(SS(I,J)+SD(I,J), 0.0, 0.)
CF        RHO2 = RHOS1P(SS(I,J)-SD(I,J), 0.0, 0.)
          IF (SD(I,J) .GT. 0.0) THEN
            SD(I,J) = 0.0
            OVRTRN = .TRUE.
          ENDIF
 1010   CONTINUE
 1000 CONTINUE

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
C***********************************************************----------!!
      SUBROUTINE READ2(X, NX, NY, UNIT, FNAME, OPE, CLOS)
C     READ IN A 2D, UNFORMATTED ARRAY, WITH EXTERNAL CONTROL ON
C       OPENING AND CLOSING.

      INTEGER NX, NY, UNIT
      REAL X(NX, NY)
      CHARACTER*60 FNAME
      LOGICAL OPE, CLOS

      IF (OPE) OPEN(UNIT, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')
      READ (UNIT) X
      IF (CLOS) CLOSE(UNIT, STATUS='KEEP')

      RETURN
      END
      FUNCTION YES(DEFALT)
C     FUNCTION TO RETURN .TRUE. IF THE USER RESPONDS Y, .FALSE. IF HE
C       SAYS N, AND THE DEFAULT VALUE OTHERWISE.

      LOGICAL YES, DEFALT
      CHARACTER RESP

      READ (11,9001) RESP
 9001 FORMAT(A1)

      YES = (RESP.EQ.'Y') .OR. (DEFALT .AND. RESP.NE.'Y'
     1                                 .AND. RESP.NE.'N')

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
      SUBROUTINE STEXT(UC, VC, UT, VT, WE, SS, SD, QS, QD, H,
     1                 NX, NY, DELX, DELY, DELT, DREF, SREF,
     2                 ASH, ASV, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE THE SALINITY FIELD TO THE NEXT
C       TIME STEP.
C     DEL(US) COMPUTED AS D(US)/DX + D(VS)/DY 3-30-88.

      INTEGER NX, NY, TSTEP
      REAL WE(NX, NY), UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL QS(NX, NY), QD(NX, NY), SS(NX, NY), SD(NX, NY)
      REAL H(NX, NY)
      REAL DELX, DELY, DELT, DREF, SREF, ASH, ASV

      INTEGER I, J, NNX, NNY
      PARAMETER (NNX = 36)
      PARAMETER (NNY = 36)
      REAL FSS(NNX, NNY), FSD(NNX, NNY)
      REAL DX2, DY2, DXTDX, DYTDY, HREF, PVDIF

      REAL PSI
      INTEGER K, L
      PSI(K,L) = 0.0
CD    PSI(K,L) = SIGN(1., WE(K,L)
CD   1  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CD   2  +  UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   3  +  VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2
CD   4  -  UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   5  -  VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2          )

      DX2 = DELX+DELX
      DY2 = DELY+DELY
      DXTDX = DELX*DELX
      DYTDY = DELY*DELY
      HREF  = H(NX/2, NY/2)
      PVDIF = 8.*ASV/HREF

C     COMPUTE FORCING FOR THE INTERIOR POINTS:
      DO 1000 J = 2, NY-1
        DO 1010 I = 2, NX-1

          FSS(I,J) =
     1     - (UT(I,J)*(SS(I+1,J)-SS(I-1,J))
     2       +VT(I,J)*(SS(I,J+1)-SS(I,J-1))
     3       +UC(I,J)*(SD(I+1,J)-SD(I-1,J))
     4       +VC(I,J)*(SD(I,J+1)-SD(I,J-1))
     5       +SD(I,J)*(4./3.)*(  (UC(I+1,J)-UC(I-1,J))
     6                         + (VC(I,J+1)-VC(I,J-1)) )  )/DX2
     7     + (QS(I,J) - 2.*WE(I,J)*SD(I,J)) / HREF
CT   8     - 2.*WE(I,J)*SD(I,J)/HREF
C          ADD DIFFUSIVE TERMS 3-30-88
CT   9     + ASH*( (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))/DXTDX
CT   1            +(SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))/DYTDY )
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.
     2     + ( (ASH + UT(I,J)*UT(I,J)*DELT*0.5)
     3           * (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))
     4       + (ASH + VT(I,J)*VT(I,J)*DELT*0.5)
     5           * (SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))  ) / DXTDX


          FSD(I,J) =
     1     - (UT(I,J)*(SD(I+1,J)-SD(I-1,J))
     2       +VT(I,J)*(SD(I,J+1)-SD(I,J-1))
     3       +UC(I,J)*(SS(I+1,J)-SS(I-1,J))
     4       +VC(I,J)*(SS(I,J+1)-SS(I,J-1))     ) / DX2
     5     + (QD(I,J) - SD(I,J)*(PVDIF-WE(I,J)) ) / HREF
CT   6     - 8.*ASV*SD(I,J)/HREF/HREF
CT   9     - WE(I,J)*SD(I,J)/HREF
CT   7     + ASH*( (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))/DXTDX
CT   8            +(SD(I,J+1)-2.*SD(I,J)+SD(I,J-1))/DYTDY )
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.
     2     + ((ASH + UT(I,J)*UT(I,J)*DELT*0.5 )
     3           * (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))
     4       +(ASH + VT(I,J)*VT(I,J)*DELT*0.5 )
     5           * (SD(I,J+1)-2.*SD(I,J)+SD(I,J-1)) ) / DXTDX


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
C         X=0 BC CHANGED TO NO FLUX 5-26-88
C         X = XMAX BC CHANGED TO ROBIN 5-26-88.
C     BC:
      DO 4000 I = 2, NX-1
        SS(I,1)  = SS(I,2)
        SD(I,1)  = SD(I,2)
        SS(I,NY) = SS(I,NY-1)
        SD(I,NY) = SD(I,NY-1)
 4000 CONTINUE
      DO 4010 J = 2, NY-1
        SS(NX,J) = SS(NX-1,J)
        SD(NX,J) = SD(NX-1,J)
        SS(1,J)  = SS(2,J)
        SD(1,J)  = SD(2,J)
 4010 CONTINUE
      SS(1,1)   = SS(2,2)
      SS(1,NY)  = SS(2,NY-1)
      SS(NX,1)  = SS(NX-1,2)
      SS(NX,NY) = SS(NX-1, NY-1)
      SD(1,1)   = SD(2,2)
      SD(1,NY)  = SD(2,NY-1)
      SD(NX,1)  = SD(NX-1,2)
      SD(NX,NY) = SD(NX-1, NY-1)

      RETURN
      END
      SUBROUTINE QSEXT(QSS, QSD, H, WE, NX, NY, TSTEP, LOY,
     1                 XCEN, XLEN, YCEN, YLEN,
     2                 QSFMAX, QSFREF, QSM, DELX, DELY,
     3                 STRSPR, STRSUM, STRFLL, STRWIN            )
C     SUBROUTINE TO EXTRAPOLATE THE SALINIZATION FORCING TO THE NEXT
C       TIME STEP.  BG 3-25-88.

      INTEGER NX, NY, TSTEP
      REAL QSS(NX, NY), QSD(NX, NY), H(NX, NY)
      REAL WE(NX, NY)
      REAL DELX, DELY

      INTEGER I, J, T

      INTEGER XCEN, YCEN, XLEN, YLEN
      REAL QSFMAX, QSFREF
      REAL QSM
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY

      REAL PI
      PARAMETER (PI = 3.141592654)
      REAL XREF, YREF, DX, DY, SIGX, SIGY
      REAL A, SUMQ, SUMW
      REAL YTERM, XPART, QSSUM
      SAVE A

      T = MOD(TSTEP,LOY)
      XREF = FLOAT(XCEN)
      YREF = FLOAT(YCEN)
      SIGX = FLOAT(XLEN)
      SIGY = FLOAT(YLEN)

      IF (T .EQ. 1) THEN
C       COMPUTE THE REQUIRED SLOPE FOR SALT CONSERVATION.
        SUMQ = 0.0
        SUMW = 0.0
        DO 100 J = 2, NY-1
          DY = DELY*FLOAT(J)
          DO 101 I = 2, NX-1
            DX = DELX*FLOAT(I)
            SUMQ = SUMQ + QSFREF + QSFMAX*
     1      EXP((-1.)*(( DX-XREF )**2/2./SIGX**2
     2                +( DY-YREF )**2/2./SIGY**2 ))
            SUMW = SUMW + WE(I,J)
 101      CONTINUE
 100    CONTINUE
        SUMQ = SUMQ*FLOAT(STRSPR-STRWIN)/FLOAT(STRFLL-STRSUM)/
     1         FLOAT(NX*NY-2*NX-2*NY+4)
C       WARNING!! SUMW HAS THE MEAN STRATIFICATION AND LENGTH OF YEAR
C         HARD CODED  3-7-90.
        SUMW = SUMW*(-0.1*2.*FLOAT(LOY))/FLOAT(STRFLL-STRSUM)/
     1         FLOAT(NX*NY-2*NX-2*NY+4)

        A    = SUMQ*2./FLOAT(NY+1)/DELY
        PRINT *,'LINEAR MELTING PARAMETER = ',A
      ENDIF

      XPART = 0.5/SIGX/SIGX
      IF ((T .GE. STRWIN) .AND. (T .LT. STRSPR)) THEN
        DO 1000 J = 1, NY
          DY = DELY*FLOAT(J)
          YTERM = (DY - YREF)*(DY - YREF)*0.5/SIGY/SIGY
          DO 1010 I = 1, NX
            DX = DELX*FLOAT(I)
            QSS(I,J) = QSFREF + QSFMAX*
     1        EXP( -(DX-XREF)*(DX-XREF)*XPART - YTERM )
            QSD(I,J) = QSS(I,J)
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
          QSSUM = QSM - A*FLOAT(J)*DELY
          DO 2010 I = 1, NX
            QSS(I,J) = QSSUM
            QSD(I,J) = QSSUM
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

      RETURN
      END
