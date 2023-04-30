      PROGRAM adel
C     Intercompare two area files
      REAL a1, a2, e1, e2
      INTEGER i

      OPEN (10, FILE="area55.92", FORM="FORMATTED", STATUS="OLD")
      OPEN (11, FILE="area55.92.new", FORM="FORMATTED", STATUS="OLD")

      DO 1000 i = 1, 366
        READ (10, 9001) a1, e1
CD        PRINT *,a1, e1
        READ (11, 9001) a2, e2
CD        PRINT *,a2, e2
        WRITE (*,9002) i, (a2-a1), 100.*(a2-a1)/a2, 
     1      (e2-e1), 100.*(e2-e1)/e2
 1000 CONTINUE
 9001 FORMAT (26x,F6.3,19x,F6.3)
 9002 FORMAT (I3, 2x, 2F6.3, 2x, 2F6.3)

      STOP
      END
C    
Cice.920101 Global area =  16.879  Global extent =  20.922
Cice.920102 Global area =  16.765  Global extent =  20.749
Cice.920103 Global area =  16.640  Global extent =  20.564
Cice.920104 Global area =  16.520  Global extent =  20.351
Cice.920105 Global area =  16.472  Global extent =  20.301
Cice.920106 Global area =  16.377  Global extent =  20.100
Cice.920107 Global area =  16.421  Global extent =  20.028
Cice.920108 Global area =  16.455  Global extent =  19.983
Cice.920109 Global area =  16.404  Global extent =  19.967
Cice.920110 Global area =  16.342  Global extent =  19.978
