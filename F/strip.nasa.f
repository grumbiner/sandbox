      PROGRAM STRIP                                                     STR00010
C     PROGRAM TO STRIP OUT THE U AND V VELOCITY COMPONENTS FROM THE     STR00020
C       ROSS SEA DATA TAPE.  7-27-87  Robert Grumbine
                                                                        STR00040
      REAL U(9500), V(9500)                                             STR00050
      INTEGER I, J                                                      STR00060
      CHARACTER*79 HEADER                                               STR00070
      CHARACTER*23 INFO                                                 STR00080
                                                                        STR00090
      OPEN (10, FILE='DUMMY', FORM='FORMATTED', STATUS='OLD')           STR00100
      OPEN (11, FILE='U', FORM='UNFORMATTED', STATUS='NEW')             STR00110
      OPEN (12, FILE='V', FORM='UNFORMATTED', STATUS='NEW')             STR00120
                                                                        STR00130
      I = 1                                                             STR00140
                                                                        STR00150
      READ (10, 9002) HEADER                                            STR00160
      WRITE (*,9002) HEADER                                             STR00170
 9002 FORMAT (1X, A79)                                                  STR00180
                                                                        STR00190
      READ (10, 9005) INFO, U(1), V(1)                                  STR00200
      WRITE (*, 9004) INFO, U(1), V(1)                                  STR00210
 9005 FORMAT (A23, F5.1, 1X, F5.1)                                      STR00220
 9004 FORMAT ('  ', A23, F5.1, 1X, F5.1)                                STR00230
                                                                        STR00240
 1000 CONTINUE                                                          STR00250
        I = I + 1                                                       STR00260
        READ (10, 9001, END=2000) U(I), V(I)                            STR00270
C       PRINT *, I, U(I), V(I)                                          STR00280
        GO TO 1000                                                      STR00290
                                                                        STR00300
 2000 CONTINUE                                                          STR00310
                                                                        STR00320
      PRINT *, I                                                        STR00330
      WRITE (11) U                                                      STR00340
      WRITE (12) V                                                      STR00350
C     WRITE (11, 9003)(U(J),J = 1, I)                                   STR00360
C     WRITE (12, 9003)(V(J),J = 1, I)                                   STR00370
 9001 FORMAT (23X, F5.1, 1X, F5.1)                                      STR00380
                                                                        STR00390
 9003 FORMAT (2X, F5.1)                                                 STR00400
                                                                        STR00410
      CLOSE (10, STATUS='KEEP')                                         STR00420
      CLOSE (11, STATUS='KEEP')                                         STR00430
      CLOSE (12, STATUS='KEEP')                                         STR00440
                                                                        STR00450
      END                                                               STR00460
