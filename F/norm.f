      FUNCTION NORM(X, Y, NP, L)                                        MOD14440
C     COMPUTE THE L NORM OF THE DIFFERENCE BETWEEN TWO VECTORS.         MOD14450
C     Robert Grumbine 12-20-1988.  
                                                                        MOD14470
      INTEGER NP, L                                                     MOD14480
      REAL X(NP), Y(NP), NORM                                           MOD14490
                                                                        MOD14500
      INTEGER I                                                         MOD14510
      REAL TEMA                                                         MOD14520
                                                                        MOD14530
      TEMA = 0.0                                                        MOD14540
      IF (L .EQ. 1) THEN                                                MOD14550
        DO 1000 I = 1, NP                                               MOD14560
          TEMA = TEMA + X(I) - Y(I)                                     MOD14570
 1000   CONTINUE                                                        MOD14580
        NORM = ABS(TEMA) / FLOAT(NP)                                    MOD14590
        RETURN                                                          MOD14600
       ELSE IF (L .EQ. 2) THEN                                          MOD14610
        DO 2000 I = 1, NP                                               MOD14620
          TEMA = TEMA + (X(I) - Y(I)) * (X(I) - Y(I))                   MOD14630
 2000   CONTINUE                                                        MOD14640
        NORM = SQRT(TEMA)/FLOAT(NP)                                     MOD14650
        RETURN                                                          MOD14660
       ELSE IF (L .EQ. 3) THEN                                          MOD14670
C       NOT A TRUE THREE NORM, ACTUALLY THE INFINITY NORM.              MOD14680
        DO 3000 I = 1, NP                                               MOD14690
          TEMA = AMAX1(TEMA, ABS(X(I)-Y(I)) )                           MOD14700
 3000   CONTINUE                                                        MOD14710
        NORM = TEMA                                                     MOD14720
        RETURN                                                          MOD14730
       ELSE                                                             MOD14740
        NORM = TEMA                                                     MOD14750
      ENDIF                                                             MOD14760
                                                                        MOD14770
      RETURN                                                            MOD14780
      END                                                               MOD14790
