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
