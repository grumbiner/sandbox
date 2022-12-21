      FUNCTION NORM(X, Y, NP, L)
C     COMPUTE THE L NORM OF THE DIFFERENCE BETWEEN TWO VECTORS.
C     BG 12-20-88.

      INTEGER NP, L
      REAL X(NP), Y(NP), NORM

      INTEGER I
      REAL TEMA

      TEMA = 0.0
      IF (L .EQ. 1) THEN
        DO 1000 I = 1, NP
          TEMA = TEMA + X(I) - Y(I)
 1000   CONTINUE
        NORM = ABS(TEMA) / FLOAT(NP)
        RETURN
       ELSE IF (L .EQ. 2) THEN
        DO 2000 I = 1, NP
          TEMA = TEMA + (X(I) - Y(I)) * (X(I) - Y(I))
 2000   CONTINUE
        NORM = SQRT(TEMA)/FLOAT(NP)
        RETURN
       ELSE IF (L .EQ. 3) THEN
C       NOT A TRUE THREE NORM, ACTUALLY THE INFINITY NORM.
        DO 3000 I = 1, NP
          TEMA = AMAX1(TEMA, ABS(X(I)-Y(I)) )
 3000   CONTINUE
        NORM = TEMA
        RETURN
       ELSE
        NORM = TEMA
      ENDIF

      RETURN
      END
