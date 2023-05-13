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
