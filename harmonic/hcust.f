      PROGRAM HCUST                                                     HAR00010
C     PROGRAM TO ACT AS A FRONT END TO THE HARMONIC ANALYSIS ROUTINES.  HAR00020
C     Robert Grumbine 8-25-86.
C     CUSTOMIZED 11-6-86 TO BE FOR A 'SMALL' NUMBER OF DATA POINTS,     HAR00040
C       AND TO EXAMINE MULTIPLE FILES AT THE SAME SET OF FREQUENCIES.  BHAR00050
      INTEGER M, N                                                      HAR00060
      REAL A(400), B(400), FREQ(400)                                    HAR00070
      REAL X(10000)                                                     HAR00080
                                                                        HAR00090
      INTEGER I, UNIT                                                   HAR00100
      CHARACTER*60 FNAME                                                HAR00110
      LOGICAL YES                                                       HAR00120
                                                                        HAR00130
      REAL PI                                                           HAR00140
      PARAMETER (PI = 3.141592654)                                      HAR00150
                                                                        HAR00160
      UNIT = 12                                                         HAR00170
                                                                        HAR00180
      OPEN (5, FILE='HIN', FORM='FORMATTED', STATUS='OLD')              HAR00190
                                                                        HAR00200
CI    PRINT *,'HOW MANY FREQUENCIES DO YOU WANT TO ANALYZE?'            HAR00210
      READ (*,9001) N                                                   HAR00220
                                                                        HAR00230
      DO 1000 I = 1, N                                                  HAR00240
CI      PRINT *,'ENTER THE FREQUENCY (CPH)'                             HAR00250
        READ (*,9002) FREQ(I)                                           HAR00260
        FREQ(I) = FREQ(I) * 2.*PI                                       HAR00270
 1000 CONTINUE                                                          HAR00280
                                                                        HAR00290
 3000 CONTINUE                                                          HAR00300
C       REPEAT UNTIL LOOP, USE THE GIVEN SET OF FREQUENCIES FOR ALL FILEHAR00310
        UNIT = UNIT + 1                                                 HAR00320
                                                                        HAR00330
CI      PRINT *,'HOW MANY DATA POINTS ARE THERE?'                       HAR00340
        READ (*,9001) M                                                 HAR00350
                                                                        HAR00360
        CALL READIN(X, M)                                               HAR00370
                                                                        HAR00380
C       FIND TO BEST FIT AMPLITUDES FOR THE GIVEN FREQUENCIES.          HAR00390
        IF (FREQ(1) .EQ. 0.0) THEN                                      HAR00400
          CALL HARMRM(X, M, FREQ, A, B, N)                              HAR00410
         ELSE                                                           HAR00420
          CALL HARMRN(X, M, FREQ, A, B, N)                              HAR00430
        ENDIF                                                           HAR00440
                                                                        HAR00450
C       WRITE OUT RESULTS:                                              HAR00460
CI      PRINT *,'WHAT DO YOU WANT TO CALL THE HARMONIC DATA FILE?'      HAR00470
        READ (*,9003) FNAME                                             HAR00480
        OPEN (UNIT, FILE=FNAME, FORM='FORMATTED', STATUS='NEW')         HAR00490
                                                                        HAR00500
        DO 2000 I = 1, N                                                HAR00510
C         WRITE (*,9002) FREQ(I)/2./PI, (A(I)*A(I)+B(I)*B(I))**.5,      HAR00520
C    1              ATAN2(B(I),A(I))                                    HAR00530
          WRITE (UNIT,9002) FREQ(I)/2./PI, (A(I)*A(I)+B(I)*B(I))**.5,   HAR00540
     1              ATAN2(B(I),A(I))                                    HAR00550
 2000   CONTINUE                                                        HAR00560
                                                                        HAR00570
C       SUBTRACT THE ANALYZED COMPONENTS FROM THE DATA.                 HAR00580
CI      PRINT *,'DO YOU WANT TO SUBTRACT THE BEST FIT HARMONICS?'       HAR00590
        IF (YES(.FALSE.)) THEN                                          HAR00600
          CALL DEMOD (A, B, FREQ, N, X, M)                              HAR00610
                                                                        HAR00620
          PRINT *,'WHAT DO YOU WANT TO CALL THE RESIDUAL FILE?'         HAR00630
          CALL RITOUT(X, M, 11)                                         HAR00640
                                                                        HAR00650
        ENDIF                                                           HAR00660
                                                                        HAR00670
        CLOSE (UNIT, STATUS='KEEP')                                     HAR00680
        PRINT *,'DO YOU WANT TO PROCESS ANOTHER FILE?'                  HAR00690
      IF (YES(.FALSE.)) GO TO 3000                                      HAR00700
                                                                        HAR00710
 9001 FORMAT (BN, I5)                                                   HAR00720
                                                                        HAR00730
 9002 FORMAT (3E14.6)                                                   HAR00740
                                                                        HAR00750
 9003 FORMAT (A60)                                                      HAR00760
                                                                        HAR00770
      END                                                               HAR00780
