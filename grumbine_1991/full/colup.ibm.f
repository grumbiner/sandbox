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
C***********************************************************----------!!
      SUBROUTINE ARSET (X, nx, ny, VALUE)
C     SET ALL ELEMENTS OF ARRAY X EQUAL TO VALUE.

      INTEGER nx, ny
      REAL X(nx, ny), VALUE

      INTEGER I, J

      DO 1000 J = 1, ny
        DO 1010 I = 1, nx
          X(I,J) = VALUE
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
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
C***********************************************************----------!!
      SUBROUTINE REFLUX(VT, VC, SS, SD, H, FLM, FLS,
     1                          SCRIT, nx, ny, DX   )
C     RECOMPUTE FLUXES USING A BOTTOM WATER DEFINITION

      INTEGER nx, ny
      REAL VT(nx, ny), VC(nx, ny), SS(nx, ny), SD(nx, ny)
      REAL H(nx, ny), FLM(ny), FLS(ny)
      REAL DX, DY, SCRIT
      INTEGER I, J
      REAL iconst

      iconst = DX*0.5*H(1,1)
      DO 1000 J = 1, ny
        FLM(J) = 0.0
        FLS(J) = 0.0
        DO 1010 I = 1, nx
C         LOWER LAYER
          IF (VT(I,J)-VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)-SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + (VT(I,J) - VC(I,J))
              FLS(J) = FLS(J) +
     1                    (VT(I,J) - VC(I,J))*(SS(I,J)-SD(I,J))
            ENDIF
          ENDIF
C         UPPER LAYER
          IF (VT(I,J)+VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)+SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + (VT(I,J) + VC(I,J))
              FLS(J) = FLS(J) +
     1                    (VT(I,J) + VC(I,J))*(SS(I,J)+SD(I,J))
            ENDIF
          ENDIF
 1010   CONTINUE
        FLM(J) = FLM(J)*iconst
        FLS(J) = FLS(J)*iconst
 1000 CONTINUE

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE stmfc4(psi, ut, vt, dx, dy, nx, ny)
C     Now, given a psi function, compute the velocities.
C     Now should have a complete soln for psi.
      IMPLICIT none
      INTEGER nx, ny
      REAL psi(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL dx, dy

      INTEGER i, j
      DO 1000 j = 1, ny
        i = 1
C       Use a forward fourth order difference.
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx
        i = 2
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx
        DO 1010 i = 3, nx-2
C     Implement a 4th order accurate first derivative in x
C       in order to get a decent treatment of v near the boundaries.
          vt(i,j) = (-psi(i+2,j)+8.*psi(i+1,j)
     1                          -8.*psi(i-1,j)+psi(i-2,j))/12./dx
 1010   CONTINUE
C       Use traditional forms for the eastern boundary, where
C         there is less structure.
        i = nx-1
        vt(i,j) = (psi(i+1,j)-psi(i-1,j))/2./dx
        i = nx
        vt(i,j) = (psi(i,j)-psi(i-1,j))/dx
 1000 CONTINUE

C     Use ordinary centered differences for u velocity.
      DO 4000 j = 2, ny-1
        DO 4010 i = 1, nx
          ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
 4010   CONTINUE
 4000 CONTINUE
      DO 4030 i = 1, nx
        j = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j))/dy
        j = ny
        ut(i,j) = -(psi(i,j)-psi(i,j-1))/dy
 4030 CONTINUE

C     Give values to the corner points
      ut(1,1)    = 0.0
      ut(1,ny)   = 0.0
      ut(nx,1)   = 0.0
      ut(nx, ny) = 0.0
      vt(1,1)    = 0.0
      vt(1,ny)   = 0.0
      vt(nx,1)   = 0.0
      vt(nx, ny) = 0.0

      RETURN
      END
C***********************************************************----------!!
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
CI    OPEN (11, FILE='MODEL9IN', FORM='FORMATTED', STATUS='OLD')
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
C***********************************************************----------!!
      SUBROUTINE QSEXT(SS, QSS, QSD, WE, H, nx, ny, TSTEP, LOY,
     1                 XCEN, XLEN, YCEN, YLEN,
     2                 QSFMAX, QSFREF, QSM, DELX, DELY,
     3                 STRSPR, STRSUM, STRFLL, STRWIN, delt )
C     SUBROUTINE TO EXTRAPOLATE THE SALINIZATION FORCING TO THE NEXT
C       TIME STEP.  BG 3-25-88.

      IMPLICIT none
      INTEGER nx, ny, TSTEP
      REAL QSS(nx, ny), QSD(nx, ny)
      REAL WE(nx, ny), H(nx, ny), SS(nx, ny)
      REAL DELX, DELY

      INTEGER I, J, T

      INTEGER XCEN, YCEN, XLEN, YLEN
      REAL QSFMAX, QSFREF
      REAL QSM, delt
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY

      REAL PI
      PARAMETER (PI = 3.141592654)
      REAL XREF, YREF, DX, DY, SIGX, SIGY
      REAL A, SUMQ, SUMW
      DOUBLE PRECISION qfix
      REAL YTERM, XPART, QSSUM
      SAVE A, qfix

      T = MOD(TSTEP,LOY)
      XREF = FLOAT(XCEN)
      YREF = FLOAT(YCEN)
      SIGX = FLOAT(XLEN)
      SIGY = FLOAT(YLEN)
      XPART = 0.5/SIGX/SIGX

      IF (T .EQ. 1) THEN
C       COMPUTE THE REQUIRED SLOPE FOR SALT CONSERVATION.
        SUMQ = 0.0
        SUMW = 0.0
        DO 100 J = 1, ny
          DY = DELY*FLOAT(J)
          YTERM = (DY - YREF)*(DY - YREF)*0.5/SIGY/SIGY
          DO 101 I = 1, nx
            DX = DELX*FLOAT(I)
            SUMQ = SUMQ + QSFREF + QSFMAX*
     1        EXP( -(DX-XREF)*(DX-XREF)*XPART - YTERM )
            SUMW = SUMW + WE(I,J)
 101      CONTINUE
 100    CONTINUE
        SUMQ = SUMQ*FLOAT(STRSPR-STRWIN+1)/FLOAT(STRFLL-STRSUM+1)
        SUMW = SUMW*(-0.1*2.*FLOAT(LOY))/FLOAT(STRFLL-STRSUM+1)
        SUMQ = SUMQ + SUMW + QSM*FLOAT(nx*ny)
        A    = SUMQ*2./DELY/FLOAT( ny*(ny+1) )/FLOAT(nx)
        PRINT *,'LINEAR MELTING PARAMETER = ',A
C       Compute the fix required for salt conservation interannually.
        CALL SUMMER(SS, nx, ny, qfix)
        qfix = -qfix*H(nx/2,ny/2)/DBLE(STRSPR-STRWIN+1)
     1             /DBLE((nx-2)*(ny-2))/delt
        PRINT *,'qfix correction',qfix
        WRITE (1, 9001) qfix
      ENDIF
 9001 FORMAT ('qfix correction',E13.6)

      IF ((T .GE. STRWIN) .AND. (T .LT. STRSPR)) THEN
        DO 1000 J = 1, ny
          DY = DELY*FLOAT(J)
          YTERM = (DY - YREF)*(DY - YREF)*0.5/SIGY/SIGY
          DO 1010 I = 1, nx
            DX = DELX*FLOAT(I)
            QSS(I,J) = QSFREF + SNGL(qfix) + QSFMAX*
     1        EXP( -(DX-XREF)*(DX-XREF)*XPART - YTERM )
            QSD(I,J) = QSS(I,J)
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (T .GE. STRSPR .AND. T .LT. STRSUM) THEN
C       SPRING, QSS = QSD = 0.0
        DO 1100 J = 1, ny
          DO 1110 I = 1, nx
            QSS(I,J) = 0.0
            QSD(I,J) = 0.0
 1110     CONTINUE
 1100   CONTINUE
       ELSEIF (T .GE. STRSUM .AND. T .LT. STRFLL) THEN
        DO 2000 J = 1, ny
          QSSUM = QSM - A*FLOAT(J)*DELY
          DO 2010 I = 1, nx
            QSS(I,J) = QSSUM
            QSD(I,J) = QSSUM
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       FALL, DO NOTHING
        DO 2100 J = 1, ny
          DO 2110 I = 1, nx
            QSS(I,J) = 0.0
            QSD(I,J) = 0.0
 2110     CONTINUE
 2100   CONTINUE
      ENDIF

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, TSTEP,
     1                     DELX, DELY)
C     PROGRAM TO COMPUTE AVERAGED QUANTITIES FROM MODEL OUTPUT
C     VERSION MODIFIED TO WORK 'ON THE FLY'.  ORIGINAL
C       ASSUMED THAT THE FILES WERE ALREADY WRITTEN AND MERELY
C       NEEDED AVERAGING.  NOW WORK WITH DATA AS IT IS BEING GENERATED.
C       BG 9-29-89
      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)

      REAL UC(nx, ny), VC(nx, ny), SS(nx, ny), SD(nx, ny)
      REAL UT(nx, ny), VT(nx, ny), H(nx, ny)
      REAL SCRIT, DELX, DELY

      INTEGER NSCRIT
      PARAMETER (NSCRIT = 12)
      REAL DELSCRIT
      PARAMETER (DELSCRIT = 0.005)
      REAL FTAV
      INTEGER I, J, K, L
      INTEGER TSTEP, TAV
      CHARACTER*60 FNAME
      REAL R1(nx, ny), R2(nx, ny), R3(nx, ny), R4(nx, ny)
      REAL R5(nx, ny), R6(nx, ny)
      REAL F1(ny,NSCRIT), F2(ny,NSCRIT)
      REAL F1T(ny,NSCRIT), F2T(ny,NSCRIT)
      SAVE R1, R2, R3, R4, R5, R6, F1, F2, F1T, F2T

C***********************************************************----------!!

C     NUMBER OF STEPS TO AVERAGE OVER IS TAV.
      FTAV = FLOAT(TAV)
      IF (TSTEP .EQ. 1) THEN
        DO 1040 K = 1, ny
          DO 1050 L = 1, nx
            R1(L,K) = 0.0
            R2(L,K) = 0.0
            R3(L,K) = 0.0
            R4(L,K) = 0.0
            R5(L,K) = 0.0
            R6(L,K) = 0.0
 1050     CONTINUE
 1040   CONTINUE
        DO 1060 K = 1, ny
          DO 1070 L = 1, NSCRIT
            F1(K,L) = 0.0
            F2(K,L) = 0.0
 1070     CONTINUE
 1060   CONTINUE

       ELSE

        DO 1900 I = 1, NSCRIT
         CALL REFLUX(VT, VC, SS, SD, H, F1T(1,I), F2T(1,I),
     1                SCRIT+(I-1)*DELSCRIT, nx, ny, DELX)
          DO 1910 K = 1, ny
             F1(K,I) = F1(K,I)+F1T(K,I)
             F2(K,I) = F2(K,I)+F2T(K,I)
 1910     CONTINUE
 1900   CONTINUE

        DO 2000 K = 1, ny
          DO 2010 L = 1, nx
            R1(L,K) = R1(L,K) + UC(L,K)
            R2(L,K) = R2(L,K) + VC(L,K)
            R3(L,K) = R3(L,K) + UT(L,K)
            R4(L,K) = R4(L,K) + VT(L,K)
            R5(L,K) = R5(L,K) + SS(L,K)
            R6(L,K) = R6(L,K) + SD(L,K)
 2010     CONTINUE
 2000   CONTINUE

        IF ( MOD(TSTEP,TAV) .EQ. 0) THEN
          DO 2020 K = 1, ny
            DO 2030 L = 1, nx
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
           WRITE (56,9009) (F1(I,J),I=1,ny)
           WRITE (57,9009) (F2(I,J),I=1,ny)
 2100   CONTINUE
         WRITE (56,9010)
          WRITE (57,9010)
          DO 2060 K = 1, ny
            DO 2050 L = 1, nx
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
C***********************************************************----------!!
      SUBROUTINE UVEXT (UC, VC, SS, H,
     1                  RHOREF, G, F, DELX, DELY)
C     EXTRAPOLATE U, V TO THE NEXT TIME LEVEL
C     COMPUTATION OF COMMON CONSTANTS ADDED PRIOR TO 5-26-88.
C     VERSION REWRITTEN FOR GEOSTROPHY, A LA DERIVATION. 4-5-89.
C       MUCH COMMENTED PROGRAM DELETED 4-5-89.
C     Unused variables deleted 7-16-90.
C     Per reviewer notes, overspecified boundary conditions removed.--
C       Note though that the bc. on SS effectively produce the same
C       result.  7-16-90.

      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      REAL UC(nx, ny), VC(nx, ny)
      REAL SS(nx, ny)
      REAL H(nx, ny)
      REAL RHOREF, F, G
      REAL DELX, DELY

      REAL RHOS1P, RHOS(nx, ny)
      INTEGER I, J

C     PARAMS FOR SPEEDIER NUMERICS
      REAL DX2, DY2, G8RREF

C     COMPUTE PARAMS FOR SPEEDIER NUMERICS
      DX2    = 2.*DELX
      DY2    = 2.*DELY
      G8RREF = G*H(1,1)/4./RHOREF/F

C     COMPUTE THE DENSITY FIELD BEFORE ENTERING THE EXTRAPOLATION.
C     THIS REDUCES THE NUMBER OF CALLS TO THE DENSITY FUNCTION BY
C       ALMOST A FACTOR OF 4.  8-4-88.
      DO 900 J = 1, ny
        DO 910 I = 1, nx
            RHOS(I,J) = RHOS1P( SS(I,J), 0.0, 0.0)
  910   CONTINUE
  900 CONTINUE

C     COMPUTE THE GEOSTROPHIC VELOCITY (Thermal Wind Relation)
      DO 1000 J = 2, ny-1
        DO 1010 I = 2, nx-1

          UC(I,J) = +G8RREF*( RHOS(I,J+1) - RHOS(I,J-1) )/DY2
          VC(I,J) = -G8RREF*( RHOS(I+1,J) - RHOS(I-1,J) )/DX2

 1010   CONTINUE
 1000 CONTINUE

C     NOW CONSIDER THE BOUNDARY CONDITIONS
C       7-16-90
C         AT I = 1         U = 0.0
C         AT I = nx        U = 0.0
C         AT J = 1         V = 0.0
C       8-2  J = ny        V = 0.0
      DO 2000 I = 1, nx
C       BC ON V AT THE Y BOUNDARIES
        VC(I,1)  = 0.0
        VC(I,ny) = 0.0
 2000 CONTINUE

      DO 2010 J = 1, ny
C       No Normal flow.
        UC(1,  J) = 0.0
        UC(nx, J) = 0.0
 2010 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE UVTROP(ut, vt, WE, H, dx, dy, F, BETA, AM)
C     BAROTROPIC SOLUTION
      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      REAL ut(nx, ny), vt(nx, ny), WE(nx, ny)
      REAL dx, dy, F, BETA, AM, H

      REAL psi(nx, ny)
      REAL NUM, LB, WAVE
      INTEGER I, J

C     COMPUTE THE INTERIOR SOLUTION
      DO 1001 I = 1, nx
        psi(I,1) = 0.0
 1001 CONTINUE
      DO 1000 J = 2, ny
CD        psi(nx, J) = WE(nx, J)
        psi(nx, J) = 0.0
        psi(nx-1,J) = 0.0
        DO 1010 I = nx-2, 1, -1
          psi(I, J) = psi(I+1,J) + WE(I+1,J) + WE(I,J)
 1010   CONTINUE
 1000 CONTINUE
      NUM = F*dx/2./BETA/H
      DO 1020 J = 1, ny
        DO 1030 I= 1, nx
          psi(I,J) = -psi(I,J) * NUM
 1030   CONTINUE
 1020 CONTINUE

C     APPLY THE BOUNDARY LAYER CORRECTIONS.
      LB = (AM/BETA)**(1./3.)
      WAVE = SQRT(3.)*dx/2./LB
      DO 2000 J = 1, ny
        NUM = LB*(psi(nx,J)-psi(nx-1,J))/dx
        DO 2010 I = 1, nx
          psi(I,J) = psi(I,J)*(1.-EXP(-(I-1)*dx/2./LB)*
     1                 (COS(WAVE*(I-1)) + SIN(WAVE*(I-1))/SQRT(3.) ))
     2              - NUM*EXP( (I-nx)*dx/LB)
 2010   CONTINUE
 2000 CONTINUE

C     NOW THAT WE HAVE psi, COMPUTE ut, VT.
      CALL stmfc4(psi, ut, vt, dx, dy, nx, ny)

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE CONVEC(SD, nx, ny, TSTEP)
C     SUBROUTINE TO LOOK FOR CONVECTIVE OVERTURNING.
C     OVERTURN ADDED 3-9-88.
C     Drop the passing of SS  7-16-90.
C     Note that this method is at least as fast as using sd = min(sd, 0)
C       - on the macintosh.  7-16-90.  don't know about other machines.
      IMPLICIT none
      INTEGER nx, ny, TSTEP
      REAL SD(nx, ny)

      INTEGER I, J
      LOGICAL OVRTRN, OVRLST
      SAVE    OVRLST

      OVRTRN = .FALSE.
      DO 1000 J = 1, ny
        DO 1010 I = 1, nx
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
      SUBROUTINE HEADER(WE, H,
     1                AHM, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                DELX, DELY, DELT,
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

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      REAL WE(nx, ny), H(nx, ny)

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

      REAL GAMMA, ETA(ny/2), DELS, CFLUX, TFLUX(ny/2)
      REAL C, TOTQ, SUMW
      INTEGER NSCRIT, I, J, K
      REAL XNOT, SIGX, YNOT, SIGY, DX, DY

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
        DO 1100 J = 2, ny/2
          SUMW = 0.5*(WE(1,J)+WE(nx,J))
          DO 1200 K = 2, nx-1
            SUMW = SUMW+WE(K,J)
 1200     CONTINUE
C         ETA(J) = (STRSPR/LOY)*(BETA/F)/((SCRIT+0.005*(I-1)))
C    1            *TOTQ /SUMW/DELX
          ETA(J) = STRSPR*BETA*TOTQ/(LOY*F*DELX*SUMW*
     1                (SCRIT+0.005*(I-1))                    )
          TFLUX(J) = SUMW*(F/BETA)*DELX
 1100   CONTINUE
        IF (I.EQ.1) WRITE (56, 9001)(ABS(TFLUX(J)),J=2,ny/2)
        WRITE (56, 9002) GAMMA,(ABS(ETA(J)),J=2,ny/2)
 1000 CONTINUE

 9001 FORMAT (6E12.2)
 9002 FORMAT (10F7.3)
      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE OUTDAT (UC, VC, UT, VT, SS, SD, H, DX)

C     WRITE OUT THE VELOCITIES and SALINITY
      INTEGER NFIELD
      PARAMETER (NFIELD = 6)

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      REAL UC(nx, ny), VC(nx, ny), UT(nx, ny), VT(nx, ny)
      REAL SS(nx, ny), SD(nx, ny), H(nx, ny)
      REAL DX

      INTEGER I, J
      REAL UCTEMP, VCTEMP, iconst
      INTEGER NFL
      REAL FLM(ny), FLS(ny)

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

      ENTRY OUTFL(UC, VC, UT, VT, SS, SD, H, DX)
      iconst = 0.5*H(1,1)*DX
      DO 1000 J = 1, ny
        FLM(J) = 0.0
        FLS(J) = 0.0
        DO 1010 I = 1, nx
          FLM(J) = FLM(J) + (VT(I,J) - VC(I,J))
          FLS(J) = FLS(J) +
     1                (VT(I,J) - VC(I,J))*(SS(I,J) - SD(I,J))
 1010   CONTINUE
 1000 CONTINUE
      DO 1100 J = 1, ny
        FLM(J) = FLM(J)*iconst
        FLS(J) = FLS(J)*iconst
 1100 CONTINUE
      WRITE (40) FLM
      WRITE (41) FLS

      RETURN

      ENTRY OUTSTR(UC, VC, UT, VT, SS, SD, H, DX)

C     OPEN THE NECESSARY OUTPUT FILES
      DO 2000 I = 1, NFIELD
        PRINT *,'WHAT IS THE NAME OF INSTANTANEOUS OUTPUT FILE # ',I
        READ  (11,9002) FNAME(I)
        WRITE (*,9002) FNAME(I)
CI      OPEN (29+I, FILE=FNAME(I), FORM='UNFORMATTED', STATUS='NEW')
 2000 CONTINUE
      PRINT *,'NAME FOR THE MASS FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
CI    OPEN (40, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'NAME FOR THE SALT FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
CI    OPEN (41, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')
C     AVERAGED OUTPUT FILES
      DO 2010 I = 1, NFIELD
        PRINT *,'WHAT IS THE NAME OF AVERAGED OUTPUT FILE # ',I
        READ  (11,9002) FNAME(I)
        WRITE (*,9002) FNAME(I)
CI      OPEN (49+I, FILE=FNAME(I), FORM='UNFORMATTED', STATUS='NEW')
 2010 CONTINUE
      PRINT *,'NAME FOR THE MASS FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
CI    OPEN (56, FILE=FNAME(1), FORM='FORMATTED', STATUS='NEW')
      PRINT *,'NAME FOR THE SALT FLUX FILE '
      READ  (11, 9002) FNAME(1)
      WRITE (*, 9002) FNAME(1)
CI    OPEN (57, FILE=FNAME(1), FORM='FORMATTED', STATUS='NEW')
 9002 FORMAT (A60)

      RETURN

      ENTRY OUTEND(UC, VC, UT, VT, SS, SD, H, DX)

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
      SUBROUTINE STEXT(UC, VC, UT, VT, WE, SS, SD, QS, QD, H,
     1                 DELX, DELY, DELT, DREF, SREF,
     2                 ASH, ASV, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE THE SALINITY FIELD TO THE NEXT
C       TIME STEP.

      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      INTEGER TSTEP
      REAL WE(nx, ny), UC(nx, ny), VC(nx, ny), UT(nx, ny), VT(nx, ny)
      REAL QS(nx, ny), QD(nx, ny), SS(nx, ny), SD(nx, ny)
      REAL H(nx, ny)
      REAL DELX, DELY, DELT, DREF, SREF, ASH, ASV

      INTEGER I, J

      REAL FSS(nx, ny), FSD(nx, ny), lapl(nx, ny)
      REAL DXTDX, HREF, DIFU
      REAL DYTDY, DX2, DY2, PVDIF

      DX2   = DELX+DELX
      DY2   = DELY+DELY
      DXTDX = DELX*DELX
      DYTDY = DELY*DELY
      HREF  = H(nx/2, ny/2)
      PVDIF = 8.*ASV/HREF

      DIFU = ASH/DXTDX

C     COMPUTE FORCING FOR THE INTERIOR POINTS:
      DO 100 J = 3, ny-2
        DO 101 I = 3, nx-2
          lapl(I,J) =
     1    (-SS(I+2,J)+16.*SS(I+1,J)-30.*SS(I,J)
     2               +16.*SS(I-1,J)-SS(I-2,J)   )/48.
     3   +(-SS(I,J+2)+16.*SS(I,J+1)-30.*SS(I,J)
     4               +16.*SS(I,J-1)-SS(I,J-2)   )/48.
  101   CONTINUE
        I = 2
        lapl(I,J) = (11.*SS(I-1,J)-20.*SS(I,J)+6.*SS(I+1,J)
     1               +4.*SS(I+2,J)-SS(I+3,J) )/48.
     3   +(-SS(I,J+2)+16.*SS(I,J+1)-30.*SS(I,J)
     4               +16.*SS(I,J-1)-SS(I,J-2)   )/48.
        I = nx-1
        lapl(I,J) = SS(I+1,J)-2.*SS(I,J)+SS(I-1,J)
     3   +(-SS(I,J+2)+16.*SS(I,J+1)-30.*SS(I,J)
     4               +16.*SS(I,J-1)-SS(I,J-2)   )/48.

  100 CONTINUE
      DO 102 I = 2, nx-1
        J = 2
        lapl(I,J) = SS(I+1,J)-2.*SS(I,J)+SS(I-1,J)
     1   +(11.*SS(I,J-1)-20.*SS(I,J)+6.*SS(I,J+1)
     2               +4.*SS(I,J+2)-SS(I,J+3) )/48.
        J = ny-1
        lapl(I,J) = SS(I+1,J)-4.*SS(I,J)+SS(I-1,J)+SS(I,J+1)+SS(I,J-1)
  102 CONTINUE

      DO 1000 J = 2, ny-1
        DO 1010 I = 2, nx-1

          FSS(I,J) =
     1     + DIFU*lapl(I,J)
     1     - (UT(I,J)*(SS(I+1,J)-SS(I-1,J))
     2       +VT(I,J)*(SS(I,J+1)-SS(I,J-1))
     3       +UC(I+1,J)*SD(I+1,J)-UC(I-1,J)*SD(I-1,J)
     4       +VC(I,J+1)*SD(I,J+1)-VC(I,J-1)*SD(I,J-1) )/DX2
     6     + (QS(I,J) - WE(I,J)*SD(I,J)) / HREF
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.
     3     + ( (UT(I,J)*UT(I,J)*DELT*0.5)
     4           * (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))
     5       + (VT(I,J)*VT(I,J)*DELT*0.5)
     6           * (SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))  ) / DXTDX

          FSD(I,J) =
     1     - (UT(I,J)*(SD(I+1,J)-SD(I-1,J))
     2       +VT(I,J)*(SD(I,J+1)-SD(I,J-1))
     3       +UC(I,J)*(SS(I+1,J)-SS(I-1,J))
     4       +VC(I,J)*(SS(I,J+1)-SS(I,J-1))     ) / DX2
     5     + (QD(I,J) - SD(I,J)* PVDIF          ) / HREF
C      Adopt upwind vertical differencing for test 8-10-90.BG
     6     - SD(I,J)*ABS(WE(I,J)/H(I,J)+(UC(I+1,J)-UC(I-1,J)+
     7                                   VC(I,J+1)-VC(I,J-1))/DX2 )
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.
     2     + ((ASH + UT(I,J)*UT(I,J)*DELT*0.5 )
     3           * (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))
     4       +(ASH + VT(I,J)*VT(I,J)*DELT*0.5 )
     5           * (SD(I,J+1)-2.*SD(I,J)+SD(I,J-1)) ) / DXTDX

 1010   CONTINUE
 1000 CONTINUE

C     EXTRAPOLATE THE INTERIOR VALUES:
      DO 3000 J = 2, ny-1
        DO 3010 I = 2, nx-1
          SS(I,J) = SS(I,J) + DELT*FSS(I,J)
          SD(I,J) = SD(I,J) + DELT*FSD(I,J)
 3010   CONTINUE
 3000 CONTINUE

C     NOW MUST APPLY THE BOUNDARY CONDITIONS.
C     CONDITION FOR THE Y=0 BNDY, NO FLUX: S(X,1) = S(X,2)
C                     Y = YMAX  ,          S(X,ny) = S(X,ny-1)
C         X=0 BC CHANGED TO NO FLUX 5-26-88
C     BC:
      DO 4000 I = 2, nx-1
        SS(I,1)  = SS(I,2)
        SD(I,1)  = SD(I,2)
        SS(I,ny) = SS(I,ny-1)
        SD(I,ny) = SD(I,ny-1)
 4000 CONTINUE
      DO 4010 J = 2, ny-1
        SS(nx,J) = SS(nx-1,J)
        SD(nx,J) = SD(nx-1,J)
        SS(1,J)  = SS(2,J)
        SD(1,J)  = SD(2,J)
 4010 CONTINUE
      SS(1,1)   = SS(2,2)
      SS(1,ny)  = SS(2,ny-1)
      SS(nx,1)  = SS(nx-1,2)
      SS(nx,ny) = SS(nx-1, ny-1)
      SD(1,1)   = SD(2,2)
      SD(1,ny)  = SD(2,ny-1)
      SD(nx,1)  = SD(nx-1,2)
      SD(nx,ny) = SD(nx-1, ny-1)

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE SUMMER(A, NX, NY, SUM)
C     SUM AN ARRAY
      INTEGER NX, NY, I, J
      REAL A(NX, NY)
      DOUBLE PRECISION SUM

      SUM = 0.D0
      DO 1000 J = 1, NY
        DO 1010 I = 1, NX
             SUM = SUM+DBLE(A(I,J))
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      PROGRAM CSHELF
C     MODEL BOUYANCY (AND PERHAPS WIND) FORCED CONTINENTAL SHELF
C       MOTIONS AND CONDITIONS FOR THE POLAR REGIONS.
      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)

C     DATA ARRAYS
      REAL UC(nx, ny), VC(nx, ny), UT(nx, ny), VT(nx, ny)
      REAL SS(nx, ny), SD(nx, ny), QSS(nx, ny), QSD(nx, ny)
      REAL WE(nx, ny), H(nx, ny)

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
      INTEGER I, J
      CHARACTER*60 FNAME
C     CONSERVATION TEST VARIABLES
      DOUBLE PRECISION S1, S2
      REAL href

C***********************************************************----------!!
C     BEGIN EXECUTION

C     INITIALIZE THE VARIABLES
      CALL INIT (UC, VC, UT, VT, SS, SD, WE, H, nx, ny,
     2           AHM, AVM, AHS, AVS,
     3           SDREF, SSREF, RHOREF, G, F,
     4           DELX, DELY, DELT,
     5           NOUT, NFLOUT, NTOT, SCRIT, TAV,
     6           XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     7           STRSPR, STRSUM, STRFLL, STRWIN, LOY,
     8           BETA                                         )

      I = 0
      href = H(1,1)
C     OPEN THE OUTPUT FILES
      CALL OUTSTR(UC, VC, UT, VT, SS, SD, H, DELX           )
      CALL UVTROP(UT, VT, WE, href, DELX, DELY, F, BETA, AHM)

      CALL HEADER(WE, H,
     1                AHM, AVM, AHS, AVS,
     2                SDREF, SSREF, RHOREF, G, F,
     3                DELX, DELY, DELT,
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,
     4                XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )

CI    OPEN (1, FILE='TIMEINFO', FORM='FORMATTED', STATUS='NEW')
CM    PRINT *,LONG(362)
CM    WRITE(1,9001) 0, LONG(362)
 9001 FORMAT (2I12)
C     EXTRAPOLATION LOOP
      DO 1000 I = 1, NTOT
CM      WRITE(*,9001) I, LONG(362)
CM      WRITE(1,9001) I, LONG(362)
        CALL UVEXT (UC, VC, SS, H,
     2              RHOREF, G, F, DELX, DELY          )

        CALL QSEXT (SS, QSS, QSD, WE, H, nx, ny, I, LOY,
     1                 XMIN, XMAX, YMIN, YMAX,
     2                 QSFMAX, QSFREF, QSM, DELX, DELY,
     3                 STRSPR, STRSUM, STRFLL, STRWIN, DELT )

        CALL STEXT (UC, VC, UT, VT, WE, SS, SD, QSS, QSD, H,
     1              DELX, DELY, DELT, SDREF, SSREF,
     2              AHS, AVS, I                                 )

        CALL CONVEC(SD, nx, ny, I)

CD      WRITE (*,9002)  I, CHAR(9), S1, CHAR(9), S2
        IF (MOD(I,20) .EQ. 0) THEN
          CALL SUMMER(SS, nx, ny, S1)
          CALL SUMMER(SD, nx, ny, S2)
          WRITE (1,9002)  I, CHAR(9), S1, CHAR(9), S2
        ENDIF

        CALL TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, I,
     1                     DELX, DELY                            )

        IF (MOD(I,NOUT) .EQ. 0) THEN
           CALL OUTDAT (UC, VC, UT, VT, SS, SD, H, DELX          )
           PRINT *,'TSTEP=',I
        ENDIF
        IF (MOD(I,NFLOUT) .EQ. 0)
     1      CALL OUTFL (UC, VC, UT, VT, SS, SD, H, DELX          )
 1000 CONTINUE

      CALL OUTEND (UC, VC, UT, VT, SS, SD, H, DELX               )

 9002 FORMAT (I5,A1,D20.14,A1,D20.14)


      END
