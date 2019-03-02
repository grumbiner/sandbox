      SUBROUTINE SORT(X, Y, N, M, KEY)                                  00012000
C     SORT 2 VECTORS BY SPECIFIED KEY FIELD.                            00012100
                                                                        00012200
      INTEGER N, M, KEY                                                 00012300
      REAL X(N), Y(N)                                                   00012400
                                                                        00012500
C     LOCAL                                                             00012600
      INTEGER I, JUMP, J, K                                             00012700
      LOGICAL DONE                                                      00012800
      REAL DUMMY                                                        00012900
                                                                        00013000
      IF (KEY .EQ. 1) THEN                                              00013100
C     SORT BY Y VALUES.                                                 00013200
        JUMP = N                                                        00013300
 1000   IF (JUMP .GT. 1) THEN                                           00013400
          JUMP = JUMP/2                                                 00013500
 2000     CONTINUE                                                      00013600
            DONE = .TRUE.                                               00013700
            DO 3000 I = 1, N-JUMP                                       00013800
              J = I + JUMP                                              00013900
              IF (Y(J) .GT. Y(I) ) THEN                                 00014000
                DUMMY = Y(I)                                            00014100
                Y(I)  = Y(J)                                            00014200
                Y(J)  = DUMMY                                           00014300
                DUMMY = X(I)                                            00014400
                X(I)  = X(J)                                            00014500
                X(J)  = DUMMY                                           00014600
                DONE = .FALSE.                                          00014700
              ENDIF                                                     00014800
 3000       CONTINUE                                                    00014900
            IF (.NOT. DONE) GO TO 2000                                  00015000
          GO TO 1000                                                    00015100
        ENDIF                                                           00015200
                                                                        00015300
CD      WRITE (*,9001) (X(I), Y(I), I=1, N)                             00015400
                                                                        00015500
 9001   FORMAT (2E15.7)                                                 00015600
                                                                        00015700
       ELSE                                                             00015800
        PRINT *,'WOULD HAVE SORTED BY X VALUES.'                        00015900
                                                                        00016000
      ENDIF                                                             00016100
                                                                        00016200
      RETURN                                                            00016300
      END                                                               00016400
