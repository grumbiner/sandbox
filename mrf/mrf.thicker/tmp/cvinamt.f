      SUBROUTINE CVINAMT(CVIN,            IIN,JTWIDL,JIN,
     1                  CVOUT,              IOUT,JPOUT,JOUT,
     2                  ILEFT,IRGHT,WGTLON,INSLAT,WGTLAT,
     3                  XX,WGT,      SUM,NN,
     4                  LTWIDL,LATRD1,LATINB)
C--   *****************************************************************
C     *  CODE BILINEARLY INTERPOLATES CLD AMT BETWEEN GAUSSIAN GRIDS--*
C     *  CLONE OF CVINTFX WITHOUT THE CLOUD TOP/BASE INTERPOLATION    *
C-    *  J = 1 IS JUST BELO N.POLE, I = 1 IS GREENWICH (THEN GO EAST).*
C     * IIN,JIN ARE I,J DIMENSIONS OF INPUT GRID--IOUT,JOUT FOR OUTPUT*
C     * JIN2,JOUT2=JIN/2,JOUT/2                                       *
C     *                           CAMPANA+KATZ+CAMPANA(AGAIN) NOV94   *
C--   *****************************************************************
      DIMENSION CVIN(IIN,JTWIDL)
      DIMENSION CVOUT(IOUT,JPOUT)
      DIMENSION ILEFT(IOUT),IRGHT(IOUT),WGTLON(IOUT)
      DIMENSION INSLAT(JOUT),WGTLAT(JOUT)
      DIMENSION XX(IOUT,4),WGT(IOUT,4),                      SUM(IOUT,4)
      DIMENSION NN(IOUT)
      III = IIN
      JBB = JTWIDL
      JJJ = JIN
      IIIOUT = IOUT
      LBB = LTWIDL
      LR1 = LATRD1
      DO 50 LATOUT=1,JPOUT
       LAT=LATOUT+LATINB-1
CCC     PRINT 100,LAT,XLAT
C===>    IF OUTPUT LAT IS POLEWARD OF INPUT LAT=1 ,THEN SIMPL AVERAGE
C          (SMALL REGION AND CLD AMT WOULDN T EXTRAPOLATE WELL)
       CALL CINPAMT(III,JBB,JJJ,IIIOUT,
     1            ILEFT,IRGHT,WGTLON,INSLAT(LAT),WGTLAT(LAT),
     2            CVIN,            CVOUT(1,LATOUT),
     3            XX,WGT,      SUM,NN,LBB,LR1)
   50 CONTINUE
CK100 FORMAT(1H ,' ROW =',I5,'  LAT =',E15.5)
      RETURN
      END
