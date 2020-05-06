C=======================================================================
      SUBROUTINE output (
     1    FLAGI1, FLAGI2, FLAGI, 
     2    OM, TAUX, TAUY, TA, TICM, QH ,
     3    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     4    CLO, T, 
     5    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS )
C=======================================================================
      IMPLICIT none
C=======================================================================
C  Programmed by:
C     Robert Grumbine       NMC, Camp Springs                     Oct.92
C     Robert Grumbine       NCEP, Camp Springs, MD                Sep 96
C  Purpose:
C     -Localize all model output to a single routine.  Clarify 
C       program logic (previously output was from main program).
C     -Output averaging removed 9/14/96 -- Print out every step.
C     -  Prepare for grib output.  RG.
C  Interface:
C     -INPUT,OUTPUT: standard control output
C     -TAPE15: ice thickness (unformatted)
C     -TAPE16: results printed in domain's shape
C     -TAPE17: ice concentration (unformatted)
C     -TAPE18: accumulated results for summation plots
C     -TAPE19: ice velocity (unformatted)
C     -TAPE20: ice compactness results for selected dates (plots)
C     -TAPE21: Run Parameters
C     -TAPE22: Mixed layer temperature
C     -TAPE23: Mixed layer salinity
C     -TAPE24: Mixed layer depth
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
C  Parameter:
C     -L: number of grid points in X-direction (even number only!)
C     -M: number of grid points in Y-direction (even number only!)
C=======================================================================

C     Arguments
      REAL OM(0:L,0:M), TAUX(L,M), TAUY(L,M), TA(0:L,0:M)
      REAL TICM(0:L,0:M,NLEVEL), QH(0:L,0:M)
      REAL QTM(0:L,0:M), ATMFLX(0:L,0:M)
      REAL A(0:L,0:M,2), H(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL FW(0:L,0:M), U(L,M,3), V(L,M,3)
      REAL PN(0:L,0:M), PM(0:L,0:M)
      REAL QS(0:L,0:M), QT(0:L,0:M), VM(L,M)
      REAL QTB(0:L,0:M), QSB(0:L,0:M), QHB(0:L,0:M)
      REAL QDT(0:L,0:M), QDS(0:L,0:M), TICE(0:L,0:M)
      REAL CLO, T
      INTEGER IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES

C=======================================================================
C     Local variables
      REAL FLAGI1(L,M), FLAGI2(L,M), FLAGI(0:L,0:M)  
      REAL FLAG, FLAG1, HEX, HEX1, HEX2, HEXI
      REAL HOSNSUM, HOTSUM, HSNSUM, HSUM, HU, HV, HWEX, HWEX2
      REAL HOSNTSUM, HOSUM
      REAL Z1, Z2, SHSUM, QTMSUM
      REAL CONC(0:L, 0:M), THICK(0:L, 0:M)
      REAL tempor(0:l, 0:M)
C=======================================================================
C     -FLAGI1:  GRID EDGE MASK FOR PLOTS SHOWING ICE COVERED AREAS ONLY
C     -FLAGI2:  SAME AS FLAGI1, BUT FOR MONTHLY AVERAGED VALUES
C     -FLAGI:   SAME AS FLAGI1, BUT FOR GRID CENTER POINTS
C=======================================================================
      INTEGER I, J
      INTEGER NRREC, NRST
C     ARMAG is the magic number used to determine compact ice conditions
      REAL ARMAG
      PARAMETER (ARMAG = 0.85)
      REAL uout(L, M), vout(L, M)

CD      SAVE
C-----------------------------------------------------------------------
C  SUMMATION OF VARIABLES FOR CONTINUOUS STATISTICS
C-----------------------------------------------------------------------
C**NEXT COMPUTATIONS START AT SPECIFIED STATISTICS INTERVAL ONLY:
       IF (MOD(IIC,NSTAT).EQ.0) THEN
C-----------------------------------------------------------------------
C  AVERAGE OCEANIC AND ATMOSPHERIC HEAT FLUXES
C-----------------------------------------------------------------------
        Z1=FLOAT(NSTAT)
C-----------------------------------------------------------------------
C  SET RUNNING SUM VALUES TO 0
C-----------------------------------------------------------------------
        HEX=.0
C  NUMBER OF ICE COVERED GRID CELLS:
        HEXI=.0
C  ICE EXTENT OF HIGH ICE COMPACTNESS:
        HEX1=.0
        HWEX=.0
        HSUM=.0
        HSNSUM=.0
C  AVERAGE OCEANIC HEAT FLUX:
        QTMSUM=.0
C  AVERAGE ATMOSPHERIC HEAT FLUX:
        SHSUM=.0
C  TIME AVERAGED ICE EXTENT OF HIGH ICE COMPACTNESS:
        HEX2=.0
C  TIME AVERAGED ICE AREA OF HIGH ICE COMPACTNESS:
        HWEX2=.0
C  HORIZONTAL GRID CELL AREA IN KM**2 (UNCORR.IF SPHER.COORD.ARE USED):
        Z1=DX*DY*1.0E-6
C-----------------------------------------------------------------------
C  STATISTIC FOR OUTFLOW
C-----------------------------------------------------------------------
C  CHANGE OF OUTFLOW IN TERMS OF ICE VOLUME:
       HOSUM=HOSUM*Z1*1.E-5
       HOTSUM=HOTSUM+HOSUM*1.E-1
C  CHANGE OF OUTFLOW IN TERMS OF SNOW VOLUME:
CD       HOSNTSUM=HOSNTSUM+HOSNSUM
C-----------------------------------------------------------------------
C  INTEGRATIONS OVER ENTIRE DOMAIN
C-----------------------------------------------------------------------
        DO 150 J=1,MM
        DO 150 I=2,LM
         FLAG=.5*(1.-SIGN(1., 1.-ARMAG-A(I,J,LNEW)))
         FLAG1=.5*(1.-SIGN(1., ARMAG-A(I,J,LNEW)))
         FLAGI(I,J)=.5*(1.-SIGN(1.,.0-H(I,J,LNEW)))
         HEX=HEX+OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))*FLAG
         HEXI=HEXI+FLAG*OM(I,J)
         HEX1=HEX1+OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))*FLAG1
         HWEX=HWEX+OM(I,J)*Z1*1.E-6*A(I,J,LNEW)/(PN(I,J)*PM(I,J))
         HSUM=HSUM+OM(I,J)*Z1*1.E-6*H(I,J,LNEW)/(PN(I,J)*PM(I,J))
         HSNSUM=HSNSUM+OM(I,J)*Z1*1.E-6*HSN(I,J,LNEW)/(PN(I,J)*PM(I,J))
         QTMSUM=QTMSUM+OM(I,J)*FLAGI(I,J)*QTM(I,J)
         SHSUM=SHSUM+OM(I,J)*FLAGI(I,J)*ATMFLX(I,J)
  150   CONTINUE
        Z2=AMAX1(HEXI,1.)
        QTMSUM=QTMSUM/Z2
        SHSUM=SHSUM/Z2*1.E-1
C-----------------------------------------------------------------------
C  WRITE OUT DATA FOR SUMMATION PLOTS (SEASONAL CYCLES)
C-----------------------------------------------------------------------
        WRITE(18,960) HSUM,HEX,HEX1,HWEX
C-----------------------------------------------------------------------
C  WRITE OUT STATISTICS
C-----------------------------------------------------------------------
C        Mixed layer
         WRITE (22) QT
         WRITE (23) QS
         WRITE (24) QH
C        Fluxes 
         WRITE (25) ATMFLX
         WRITE (26) QTM

         WRITE(*, 902) IIC, HSUM, HEX, HWEX, HSNSUM, HOSUM, HOTSUM, 
     1                   QTMSUM, SHSUM
         WRITE(16, 902) IIC, HSUM, HEX, HWEX, HSNSUM, HOSUM, HOTSUM, 
     1                   QTMSUM, SHSUM
         DO 1000 j = 0, M
           DO 1100 i = 0, L
             CONC(i,j) = A(i,j,LNEW)
             THICK(i,j) = H(i,j,LNEW)
 1100      CONTINUE
 1000   CONTINUE
        WRITE (17) CONC
        WRITE (15) THICK

        DO 1200 j = 1, M
          DO 1300 i = 1, L
            IF (CONC(i-1,j-1)+CONC(i-1,j)
     1         +CONC(i,j)+CONC(i,j-1) .NE. 0.) THEN
              uout(i,j) = U(i, j, LNEW)
              vout(i,j) = V(I, j, LNEW)
             ELSE
              uout(i,j) = 0.0
              vout(i,j) = 0.0
             ENDIF
 1300     CONTINUE
 1200   CONTINUE
        WRITE (19) uout
        WRITE (19) vout

CD        PRINT *,'Passed write 19 uout, vout'
C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
C-----------------------------------------------------------------------
C  PRINT OUT GEOGRAPHICAL PATTERNS
C-----------------------------------------------------------------------
         IF (MOD(IIC,NSTA).EQ.0) THEN
          CALL DRUCKF(OM(0,0),OM,10.,0.,    'OM      ',IIC,LP,MP)
          CALL DRUCKF(H(0,0,LNEW),OM,10.,0.,'H*10 [M]',IIC,LP,MP)
          CALL DRUCKF(A(0,0,LNEW),OM,100.,0.,'A [%]',IIC,LP,MP)
          CALL DRUCKF(U(1,1,LNEW),VM,100.,0.,'U [CM/S]',IIC,L,M)
          CALL DRUCKF(V(1,1,LNEW),VM,100.,0.,'V [CM/S]',IIC,L,M)
          CALL DRUCKF(HSN(0,0,LNEW),OM,100.,0.,'HSNOW [CM]',IIC,LP,MP)
          CALL DRUCKF(QT,OM,10.,0.,'M L TEMP*10 [DEG C]',IIC,LP,MP)
          CALL DRUCKF(TICM(0,0,INT(0.5+(NLEVEL+1)/2)), 
     1                    OM,1.,0.,'ICE TEMP [DEG C]',IIC,LP,MP)
          CALL DRUCKF(QS,OM,10.,-300.,'SALT M L *10 -300',IIC,LP,MP)
          CALL DRUCKF(QH,OM,1.,0.,'M L DEPTH [M]',IIC,LP,MP)
CD          PRINT *,'Wrote QH'
          CALL DRUCKF(QTM,OM,1.,0.,'OC. HEAT FLUX [W/M**2]',IIC,LP,MP)
CD          PRINT *,'Wrote QTM'
          CALL DRUCKF(ATMFLX,OM,.1,0.,'ATM. HEAT FL./10[W/M**2]',
     1       IIC,LP,MP)
CD          PRINT *,'Wrote ATMFLUX'
          WRITE(16,901)
C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
C**REMARK: MOD. FOR REAL-TIME DAILY FORCING: 6TH YEAR:+5; 5TH YEAR:+20.
         IF (MOD((IIC+5),NPLT).EQ.0) THEN
C-----------------------------------------------------------------------
C  DETERMINATION OF MONTHLY MEAN VALUES
C-----------------------------------------------------------------------
CD          DO 309 J=1,MM
CD          DO 309 I=2,LM
CD           HV=H(I,J,LNEW)+H(I,J-1,LNEW)+H(I-1,J-1,LNEW)+H(I-1,J,LNEW)
CD           FLAGI1(I,J)=.5*(1.-SIGN(1.,.0-HV))
CD           FLAGI2(I,J)=.5*(1.-SIGN(1.,.0-HU))
CD           HEX2=HEX2+FLAG1*OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))
CDCD           HWEX2=HWEX2+OM(I,J)*Z1*1.E-6*BM(I,J)/(PN(I,J)*PM(I,J))
CD  309     CONTINUE
          ENDIF
        ENDIF
       END IF
C-----------------------------------------------------------------------
C  WRITE OUT RESTART DATA
C-----------------------------------------------------------------------
      IF (MOD(IIC, NRST).EQ.0) THEN
        DO 9800 j = 0, M
          DO 9801 i = 0, L
            tempor(i,j) = H(i,j,LNEW)
 9801     CONTINUE
 9800   CONTINUE 
        WRITE (14) tempor

        DO 9802 j = 0, M
          DO 9803 i = 0, L
            tempor(i,j) = A(i,j,LNEW)
 9803     CONTINUE
 9802   CONTINUE 
CD        WRITE (14) A(1,1,LNEW)
        WRITE (14) tempor

        DO 9804 j = 0, M
          DO 9805 i = 0, L
            tempor(i,j) = HSN(i,j,LNEW)
 9805     CONTINUE
 9804   CONTINUE 
CD        WRITE (14) HSN(1,1,LNEW)
        WRITE (14) tempor

        WRITE (14) TICE
        WRITE (14) QT
        WRITE (14) QS
        WRITE (14) QH
        WRITE (14) QTB
        WRITE (14) QSB
        WRITE (14) QHB
        WRITE (14) QDT
        WRITE (14) QDS
        WRITE (14) TICM
        WRITE (14) U
        WRITE (14) V
        NRREC=NRREC+1
      END IF
  
  901 FORMAT (' TIME',3X,' ICE  ',2X,' ICE  ',2X,' ICE ',3X,' SNOW ',2X,
     1       ' ICE OUTFLOW ',2X,'AVG. HEAT FLUXES'/
     2       ' STEP',3X,'VOLUME',2X,'EXTENT',2X,' AREA',3X,'VOLUME',2X,
     3       '        CUMM ',2X,' OCEAN    ATMOS ')
  903 FORMAT (8X,        '10**3 ',2X,'10**6 ',2X,'10**6',3X,'10**3 ',2X,
     1       '10**2   10**3',2X,'            10  '/
     2       8X,        'KM**3 ',2X,'KM**2 ',2X,'KM**2',3X,'KM**3 ',2X,
     3       'KM**3   KM**3',2X,'W/M**2    W/M**2')
  902 FORMAT (1X, I4, 7F8.4, F8.3)
  960 FORMAT (5F8.4)
 9101 FORMAT (5E13.4)

      RETURN

C-----------------------------------------------------------------------
      ENTRY outstr (
     1    FLAGI1, FLAGI2, FLAGI, 
     5    OM, TAUX, TAUY, TA, TICM, QH ,
     6    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     7    CLO, T, 
     8    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS )
C     Initialize output fields and print initial conditions
      NRREC = 0
C-----------------------------------------------------------------------
C  SET RUNNING SUM VALUES TO 0
C-----------------------------------------------------------------------
C  CUMULATIVE OUTFLOW IN TERMS OF ICE THICKNESS:
      HOTSUM=.0
      HOSUM=.0
C  CUMULATIVE OUTFLOW IN TERMS OF SNOW THICKNESS:
CD      HOSNTSUM=.0
C  ICE EXTENT:
      HEX=.0
C  ICE AREA:
      HWEX=.0
C  ICE VOLUME:
      HSUM=.0
C  SNOW VOLUME:
      HSNSUM=.0
C  HORIZONTAL GRID CELL AREA IN KM**2 (UNCORR.IF SPHER.COORD.ARE USED):
      Z1=DX*DY*1.0E-6
C-----------------------------------------------------------------------
C  INITIAL INTEGRATIONS OVER ENTIRE DOMAIN
C-----------------------------------------------------------------------
CD      PRINT *,"starting 5 loop in outstr"
      DO 5 J=1,MM
      DO 5 I=2,LM
       FLAG=.5*(1.-SIGN(1., 1.-ARMAG-A(I,J,1)))
       HEX=HEX+OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))*FLAG
       HWEX=HWEX+OM(I,J)*Z1*1.E-6*A(I,J,1)/(PN(I,J)*PM(I,J))
       HSUM=HSUM+OM(I,J)*Z1*1.E-6*H(I,J,1)/(PN(I,J)*PM(I,J))
       HSNSUM=HSNSUM+OM(I,J)*Z1*1.E-6*HSN(I,J,1)/(PN(I,J)*PM(I,J))
    5 CONTINUE
CBG   Latter half is a test against NaN.  8 July 2003
      IF ((HSNSUM .GE. 10. .OR. HSNSUM .LT. 0) .OR. 
     1    (.NOT. ((HSNSUM .GT.0) .OR. (HSNSUM .LE. 0) ))  ) THEN
        PRINT *,'z1 = ',Z1
        PRINT *,'zzzzz hsn = ',HSN
        PRINT *,'zzzzz om = ',OM
        STOP "HSN problem"
      ELSE
        PRINT *,'zzzz hsnsum ok at ',HSNSUM
      ENDIF
C-----------------------------------------------------------------------
C  PRINT OUT STATISTICS
C-----------------------------------------------------------------------
      WRITE(*, 901)
      WRITE(*, 903)
      WRITE(16, 901)
      WRITE(16, 903)
      WRITE(*, 902) 0, HSUM, HEX, HWEX, HSNSUM
      WRITE(16, 902) 0, HSUM, HEX, HWEX, HSNSUM
CREF  902 FORMAT (1X, I4, 7F8.4, F8.3)
CD      PRINT *,'Leaving outstr'

      RETURN
      END
