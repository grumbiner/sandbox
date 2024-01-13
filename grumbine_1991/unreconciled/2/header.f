C***********************************************************----------!!
      SUBROUTINE HEADER(WE, H,
     1                ahm, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                delx, dely, DELT,
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,
     4                XCEN, XLEN, YCEN, YLEN, QFMAX, QFREF, QM,
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )
C     COMPUTE THE NONDIMENSIONAL PARAMETERS USED IN DESCRIBING THE
C        EXPERIMENTS.  2-1-90. BG
      IMPLICIT none
      REAL SECPYR
      PARAMETER (SECPYR = 3.1556908E7)
      REAL PI
      PARAMETER (PI = 3.141592654)

      INCLUDE "grid.inc"
      REAL WE(nx, ny), H(nx, ny)

      REAL ahm, AVM, AHS, AVS
      REAL SDREF, SSREF, RHOREF
      REAL G, F
      REAL delx, dely, DELT
      INTEGER NOUT, NFLOUT, NTOT, TAV
      REAL SCRIT

C     PARAMETERS FOR BUOYANCY FORCING
      INTEGER XCEN, XLEN, YCEN, YLEN
      REAL QFMAX, QFREF, QM
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY

C     PARMS FOR BAROTROPIC FLOW
      REAL BETA

      REAL GAMMA, ETA(ny/2), DELS, CFLUX, TFLUX(ny/2)
      REAL C, TOTQ, SUMW
      INTEGER NSCRIT, I, J, K
      REAL XNOT, SIGX, YNOT, SIGY

      XNOT = FLOAT(XCEN)
      SIGX = FLOAT(XLEN)
      YNOT = FLOAT(YCEN)
      SIGY = FLOAT(YLEN)
      C = 0.809

      NSCRIT = 12
      DELS = QFMAX*STRSPR*DELT*EXP(-1.4**2/2-0.675**2/2)
     1            /H(nx/2,ny/2)
      CFLUX = G*H(1,1)**2*C/8/RHOREF/F * DELS
      TOTQ = 0.0
      DO 100 J = 2, ny
        DO 110 I = 1, nx
          TOTQ = TOTQ+QFREF+  EXP(-(I*delx-XNOT)**2/2./SIGX**2
     1                            -(J*dely-YNOT)**2/2./SIGY**2 )*QFMAX
 110    CONTINUE
 100  CONTINUE
      TOTQ = TOTQ * delx*delx
      WRITE (56, 9001) DELS, CFLUX, TOTQ
      DO 1000 I = 1, NSCRIT
        GAMMA = STRSPR*DELT*(QFMAX+QFREF)/H(1,1)
     1          /(SCRIT+0.005*(I-1))
        SUMW = 0.0
        DO 1100 J = 2, ny/2
          SUMW = 0.5*(WE(1,J)+WE(nx,J))
          DO 1200 K = 2, nx-1
            SUMW = SUMW+WE(K,J)
 1200     CONTINUE
C         ETA(J) = (STRSPR/LOY)*(BETA/F)/((SCRIT+0.005*(I-1)))
C    1            *TOTQ /SUMW/delx
          ETA(J) = STRSPR*BETA*TOTQ/(LOY*F*delx*SUMW*
     1                (SCRIT+0.005*(I-1))                    )
          TFLUX(J) = SUMW*(F/BETA)*delx
 1100   CONTINUE
        IF (I.EQ.1) WRITE (56, 9001)(ABS(TFLUX(J)),J=2,ny/2)
        WRITE (56, 9002) GAMMA,(ABS(ETA(J)),J=2,ny/2)
 1000 CONTINUE

 9001 FORMAT (6E12.2)
 9002 FORMAT (10F7.3)
      RETURN
      END
