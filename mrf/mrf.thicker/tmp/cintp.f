      SUBROUTINE CINTP(IIN,JTWIDL,JIN,IOUT,
     1                 ILEFT,IRGHT,WGTLON,INSLAT,WGTLAT,
     2                 CV,CVT,CVB,CAMT,CTOP,CBOT,
     3                 XX,WGT,TT,BB,SUM,NN,LTWIDL,LATRD1)
      DIMENSION CV(IIN,JTWIDL),CVT(IIN,JTWIDL),CVB(IIN,JTWIDL)
      DIMENSION CAMT(IOUT),CTOP(IOUT),CBOT(IOUT)
      DIMENSION ILEFT(IOUT),IRGHT(IOUT),WGTLON(IOUT)
      DIMENSION XX(IOUT,4),WGT(IOUT,4),TT(IOUT,4),BB(IOUT,4),SUM(IOUT,4)
      DIMENSION NN(IOUT)
C        SIMPL LINEAR INTERPOLATION OF CLDAMT, UNLESS ONLY 1,2 OF THE
C         SURROUNDING PTS HAS CV. THEN,IF OUTPUT GRIDPT NOT CLOSE ENUF
C         DO NOT INTERPOLATE TO IT(PREVENTS SPREADING OF CV CLDS)..
C           FOR 1 PT CONVECTION-INTRP WGT GE (.7)**2 ...
C           FOR 2 PT CONVECTION-SUM OF INTRP WGT GE .45...
C              .45 USED RATHER THAN .5 TO GIVE BETTER RESULT FOR
C              DIAGONALLY OPPOSED PTS...
C===>    FOR TOPS(CVT) AND BOTS(CVB) JUST TAKE AVERAGE OF SURROUNDING
C         NON-ZERO CV POINTS.....
C         NN WILL BE NUMBER OF SURROUNDING PTS WITH CLD (GT ZERO)
C---     NHSH = 1,-1 FOR NORTHERN,SOUTHERN HEMISPHERE
C         HERE INSTEAD OF AN EXTRAPOLATION,JUST DO A SIMPLE MEAN....
C
      IF (INSLAT.LT.0) GO TO 600
      INTH = MOD(LTWIDL + INSLAT - LATRD1 - 1,JTWIDL) + 1
      INTH1 = MOD(INTH,JTWIDL) + 1
      IF (INSLAT.EQ.JIN) GO TO 105
      DO 100 I=1,IOUT
C----   NORMALIZED DISTANCE FROM UPPER LAT TO GAUSSIAN LAT
        XX(I,1) = CV(ILEFT(I),INTH)
        XX(I,2) = CV(ILEFT(I),INTH1)
        XX(I,3) = CV(IRGHT(I),INTH)
        XX(I,4) = CV(IRGHT(I),INTH1)
        WGT(I,1) = (1. E  0-WGTLON(I))*(1. E  0-WGTLAT)
        WGT(I,2) = (1. E  0-WGTLON(I))*WGTLAT
        WGT(I,3) = WGTLON(I)*(1. E  0-WGTLAT)
        WGT(I,4) = WGTLON(I)*WGTLAT
        TT(I,1) = CVT(ILEFT(I),INTH)
        TT(I,2) = CVT(ILEFT(I),INTH1)
        TT(I,3) = CVT(IRGHT(I),INTH)
        TT(I,4) = CVT(IRGHT(I),INTH1)
        BB(I,1) = CVB(ILEFT(I),INTH)
        BB(I,2) = CVB(ILEFT(I),INTH1)
        BB(I,3) = CVB(IRGHT(I),INTH)
        BB(I,4) = CVB(IRGHT(I),INTH1)
  100 CONTINUE
      GO TO 130
  105 DO 110 I=1,IOUT
C----   NORMALIZED DISTANCE FROM UPPER LAT TO GAUSSIAN LAT
        XX(I,1) = CV(ILEFT(I),INTH)
        XX(I,3) = CV(IRGHT(I),INTH)
        WGT(I,1) = (1. E  0-WGTLON(I))*(1. E  0-WGTLAT)
        WGT(I,2) = (1. E  0-WGTLON(I))*WGTLAT
        WGT(I,3) = WGTLON(I)*(1. E  0-WGTLAT)
        WGT(I,4) = WGTLON(I)*WGTLAT
        TT(I,1) = CVT(ILEFT(I),INTH)
        TT(I,3) = CVT(IRGHT(I),INTH)
        BB(I,1) = CVB(ILEFT(I),INTH)
        BB(I,3) = CVB(IRGHT(I),INTH)
  110 CONTINUE
      IOUT2 = IOUT / 2
      DO 120 I=1,IOUT2
        XX(I,2) = CV(ILEFT(I+IOUT2),INTH)
        XX(I+IOUT2,2) = CV(ILEFT(I),INTH)
        XX(I,4) = CV(IRGHT(I+IOUT2),INTH)
        XX(I+IOUT2,4) = CV(IRGHT(I),INTH)
        BB(I,2) = CVB(ILEFT(I+IOUT2),INTH)
        BB(I+IOUT2,2) = CVB(ILEFT(I),INTH)
        BB(I,4) = CVB(IRGHT(I+IOUT2),INTH)
        BB(I+IOUT2,4) = CVB(IRGHT(I),INTH)
        TT(I,2) = CVT(ILEFT(I+IOUT2),INTH)
        TT(I+IOUT2,2) = CVT(ILEFT(I),INTH)
        TT(I,4) = CVT(IRGHT(I+IOUT2),INTH)
        TT(I+IOUT2,4) = CVT(IRGHT(I),INTH)
  120 CONTINUE
C---      NN WILL BE NUMBER OF SURROUNDING PTS WITH CLD (GT ZERO)
CKAC    NN(1;IOUT) = 0
CKAC    SUM(1,1;IOUT*4) = 0. E  0
  130 DO 10 I=1,IOUT
        NN(I) = 0
   10 CONTINUE
      DO 12 J=1,4
       DO 12 I=1,IOUT
        SUM(I,J) = 0. E 0
   12 CONTINUE
        DO 150 KPT=1,4
CKAC      WHERE (XX(1,KPT;IOUT).GT.0. E  0)
CKAC        NN(1;IOUT) = NN(1;IOUT) + 1
CKAC        SUM(1,1;IOUT) = SUM(1,1;IOUT) + WGT(1,KPT;IOUT)
CKAC        SUM(1,2;IOUT) = SUM(1,2;IOUT) + TT(1,KPT;IOUT)
CKAC        SUM(1,3;IOUT) = SUM(1,3;IOUT) + BB(1,KPT;IOUT)
CKAC      ENDWHERE
CKAC      SUM(1,4;IOUT) = SUM(1,4;IOUT) + WGT(1,KPT;IOUT) *
CKAC 1                    XX(1,KPT;IOUT)
          DO 14 I=1,IOUT
            IF (XX(I,KPT).GT.0. E 0) THEN
              NN(I) = NN(I) + 1
              SUM(I,1) = SUM(I,1) + WGT(I,KPT)
              SUM(I,2) = SUM(I,2) + TT(I,KPT)
              SUM(I,3) = SUM(I,3) + BB(I,KPT)
            ENDIF
   14     CONTINUE
          DO 15 I=1,IOUT
            SUM(I,4) = SUM(I,4) + WGT(I,KPT) * XX(I,KPT)
   15     CONTINUE
  150   CONTINUE
CKAC    WHERE((NN(1;IOUT).EQ.1 .AND. SUM(1,1;IOUT).GT.0.49 E  0) .OR.
CKAC 1        (NN(1;IOUT).EQ.2 .AND. SUM(1,1;IOUT).GE.0.45 E  0) .OR.
CKAC 2         NN(1;IOUT).GE.3)
CKAC      CTOP(1;IOUT) = VAINT(SUM(1,2;IOUT)/NN(1;IOUT)+0.5 E  0;
CKAC 1                         CTOP(1;IOUT))
CKAC      CBOT(1;IOUT) = VAINT(SUM(1,3;IOUT)/NN(1;IOUT)+0.5 E  0;
CKAC 1                         CBOT(1;IOUT))
CKAC      CAMT(1;IOUT) = SUM(1,4;IOUT)
CKAC    OTHERWISE
CKAC      CTOP(1;IOUT) = 0. E  0
CKAC      CBOT(1;IOUT) = 100. E  0
CKAC      CAMT(1;IOUT) = 0. E  0
CKAC    ENDWHERE
        DO 16 I=1,IOUT
          IF (NN(I).EQ.1.AND.SUM(I,1).GT.0.49 E 0) GO TO 17
          IF (NN(I).EQ.2.AND.SUM(I,1).GE.0.45 E 0) GO TO 17
          IF (NN(I).GE.3) GO TO 17
            CTOP(I) = 0. E 0
            CBOT(I) = 100. E 0
            CAMT(I) = 0. E 0
            GO TO 18
   17     CONTINUE
            LTOP = SUM(I,2)/NN(I) + 0.5 E 0
            LBOT = SUM(I,3)/NN(I) + 0.5 E 0
            CTOP(I) = LTOP
            CBOT(I) = LBOT
            CAMT(I) = SUM(I,4)
   18     CONTINUE
   16   CONTINUE
      RETURN
C--- POLAR REGION-NO EXTRAPOLATION
  600 CONTINUE
      JA = IABS(INSLAT)
      DO 200 I=1,IOUT
C----    GET LEFT POINT ON NEAREST LATITUDE
        XX(I,1) = CV(ILEFT(I),JA)
        XX(I,2) = CV(IRGHT(I),JA)
        WGT(I,1) = 1. E  0-WGTLON(I)
        WGT(I,2) = WGTLON(I)
        TT(I,1) = CVT(ILEFT(I),JA)
        TT(I,2) = CVT(IRGHT(I),JA)
        BB(I,1) = CVB(ILEFT(I),JA)
        BB(I,2) = CVB(IRGHT(I),JA)
  200 CONTINUE
C---      NN WILL BE NUMBER OF SURROUNDING PTS WITH CLD (GT ZERO)
CKAC    NN(1;IOUT) = 0
CKAC    SUM(1,1;IOUT*4) = 0. E  0
      DO 20 I=1,IOUT
        NN(I) = 0
   20 CONTINUE
      DO 22 J=1,4
       DO 22 I=1,IOUT
        SUM(I,J) = 0. E 0
   22 CONTINUE
        DO 202 KPT=1,2
CKAC      WHERE (XX(1,KPT;IOUT).GT.0. E  0)
CKAC        NN(1;IOUT) = NN(1;IOUT) + 1
CKAC        SUM(1,1;IOUT) = SUM(1,1;IOUT) + WGT(1,KPT;IOUT)
CKAC        SUM(1,2;IOUT) = SUM(1,2;IOUT) + TT(1,KPT;IOUT)
CKAC        SUM(1,3;IOUT) = SUM(1,3;IOUT) + BB(1,KPT;IOUT)
CKAC      ENDWHERE
CKAC      SUM(1,4;IOUT) = SUM(1,4;IOUT) + WGT(1,KPT;IOUT) *
CKAC 1                    XX(1,KPT;IOUT)
          DO 24 I=1,IOUT
            IF (XX(I,KPT).GT.0. E 0) THEN
              NN(I) = NN(I) + 1
              SUM(I,1) = SUM(I,1) + WGT(I,KPT)
              SUM(I,2) = SUM(I,2) + TT(I,KPT)
              SUM(I,3) = SUM(I,3) + BB(I,KPT)
            ENDIF
   24     CONTINUE
          DO 25 I=1,IOUT
            SUM(I,4) = SUM(I,4) + WGT(I,KPT) * XX(I,KPT)
   25     CONTINUE
  202   CONTINUE
CKAC    WHERE((NN(1;IOUT).EQ.1 .AND. SUM(1,1;IOUT).GT.0.7 E  0) .OR.
CKAC 1         NN(1;IOUT).EQ.2)
CKAC      CTOP(1;IOUT) = VAINT(SUM(1,2;IOUT)/NN(1;IOUT)+0.5 E  0;
CKAC 1                         CTOP(1;IOUT))
CKAC      CBOT(1;IOUT) = VAINT(SUM(1,3;IOUT)/NN(1;IOUT)+0.5 E  0;
CKAC 1                         CBOT(1;IOUT))
CKAC      CAMT(1;IOUT) = SUM(1,4;IOUT)
CKAC    OTHERWISE
CKAC      CTOP(1;IOUT) = 0. E  0
CKAC      CBOT(1;IOUT) = 100. E  0
CKAC      CAMT(1;IOUT) = 0. E  0
CKAC    ENDWHERE
        DO 26 I=1,IOUT
          IF (NN(I).EQ.1.AND.SUM(I,1).GT.0.7 E 0) GO TO 27
          IF (NN(I).EQ.2) GO TO 27
            CTOP(I) = 0. E 0
            CBOT(I) = 100. E 0
            CAMT(I) = 0. E 0
            GO TO 28
   27     CONTINUE
            LTOP = SUM(I,2)/NN(I) + 0.5 E 0
            LBOT = SUM(I,3)/NN(I) + 0.5 E 0
            CTOP(I) = LTOP
            CBOT(I) = LBOT
            CAMT(I) = SUM(I,4)
   28     CONTINUE
   26   CONTINUE
      RETURN
      END
