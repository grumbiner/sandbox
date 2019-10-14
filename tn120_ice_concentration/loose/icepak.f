        SUBROUTINE ICECAP(IDATA,LDATA,MDATA,INFILE,KDATE,ICEDAY)        00010000
C===>     FOR 2 DEGREE LATITUDE LONGITUDE GRID....                      00020000
C                                                                       00030000
       CHARACTER*1  NINE/'9'/                                           00040000
       CHARACTER*1  EW(200)                                             00050001
       CHARACTER*1  NS(200)                                             00060001
       CHARACTER*1  EAST /'E'/ , WEST /'W'/ , NORTH /'N'/, SOUTH /'S'/  00070000
       CHARACTER*8  ON85,TPRT                                           00080000
       INTEGER*2    KDATE(4)                                            00090000
C                                                                       00100000
       REAL*4 IDATA(65,65)                                              00110000
       REAL*4 LDATA(145,37)                                             00120000
       REAL*4 MDATA(181,46)                                             00130000
       INTEGER*4    IX(200),JX(200),ICUT(200)                           00140001
       REAL*4       XI(200),XJ(200)                                     00150001
       INTEGER*4 IIDATA(65,65)  /4225*0/                                00160000
       INTEGER*4 LLDATA(145,37) /5365*0/                                00170000
       INTEGER*4 MMDATA(181,46) /8326*0/                                00180000
C                                                                       00190000
       REAL*4       XLAT(200),XLON(200)                                 00200001
       REAL*4       WLAT(200),WLON(200)                                 00210001
       REAL*4       S(200),AIL(200),HORZ(200),AJL(200),VERT(200)        00220001
C                                                                       00230000
       REAL*4       ORIENT, XMESHL                                      00240000
C----  NUM PTS IN GRID LONGITUDE---                                     00250000
      XLONPT = 180.                                                     00260000
      DEG10 = 360. / XLONPT * 10.                                       00270000
      XINCR = 1. * DEG10/20.                                            00280000
C                                                                       00290000
C  RETRIEVE AND UNPACK THE ICE DATA.                                    00300000
C                                                                       00310000
C    READ IN THE ICE DATE                                               00320000
C                                                                       00330000
       READ (INFILE,1001) ON85,KDATE                                    00340000
 1001  FORMAT (A8,4I2)                                                  00350000
C                                                                       00360000
C    SET UP THE AGE OF THE ICE DATA                                     00370000
C                                                                       00380000
       IYR = KDATE(1)                                                   00390000
       IMO = KDATE(2)                                                   00400000
       IDA = KDATE(3)                                                   00410000
       CALL W3FS13 (IYR,IMO,IDA,ICEDAY)                                 00420000
C                                                                       00430000
C    READ IN THE ICE-EDGE POINTS                                        00440000
C                                                                       00450000
      I = 1                                                             00460000
C                                                                       00470000
    5 CONTINUE                                                          00480000
      I1 = I                                                            00490000
      I2 = I+3                                                          00500000
C---    UPDATED DIMENSION SIZE TO 200+PRINT IF EXCEEDED...KAC AUG89     00500101
      IF (I2.GT.200) PRINT 66,I2                                        00501001
      IF (I2.GT.200) STOP 66                                            00501102
   66 FORMAT(1H ,'--NUMPTS EXCEEDING DIMENSION(200) IN ICEPAK =',I4)    00502002
C                                                                       00510000
      READ (INFILE,10,END=20) (XLAT(I),NS(I),XLON(I),EW(I),I=I1,I2)     00520000
   10 FORMAT (4(F3.0,A1,F4.0,A1,1X))                                    00530000
C                                                                       00540000
         IF(NS(I1).EQ.NINE .OR.NS(I1+1).EQ.NINE .OR.NS(I1+2).EQ.NINE    00550000
     1       .OR. NS(I1+3).EQ. NINE) GOTO 20                            00560000
C                                                                       00570000
C     CHECK FOR THE END OF THE PRIMARY ICE EDGE LINE.                   00580000
C                                                                       00590000
      IF (I1 .EQ. 1) GO TO 5                                            00600000
        DO 40 IXX= I1, I2                                               00610000
          NUMPTS= IXX - 1                                               00620000
        IF (XLAT(IXX).EQ.XLAT(1) .AND. XLON(IXX).EQ.XLON(1)) GO TO 45   00630000
   40   CONTINUE                                                        00640000
C                                                                       00650000
C                                                                       00660000
      GOTO 5                                                            00670000
C                                                                       00680000
C    DETERMINE THE NUMBER OF ICE-EDGE POINTS                            00690000
C                                                                       00700000
   20 CONTINUE                                                          00710000
      N1 = I-5                                                          00720000
      N2 = I-1                                                          00730000
      DO 15 NN = N1,N2                                                  00740000
        IF(EW(NN).EQ.EAST.OR.EW(NN).EQ.WEST) NUMPTS = NN                00750000
   15 CONTINUE                                                          00760000
   45 CONTINUE                                                          00770000
C     WRITE (6,16) NUMPTS                                               00780000
C  16 FORMAT(' NUMPTS =',I3)                                            00790000
C                                                                       00800000
C    SET LOLA POINTS; LONGITUDE MUST BE SET TO DEGREES WEST.            00810000
C===>   TO AVOID GRAPHICS PROBLEMS,RESET THE ICE ANALYSIS WHEN          00820000
C        THE JIC POINTS FALL EXACTLY ON LAT AND LON OF OUTPUT GRID..    00830000
C--    DEG10=OUTPUT GRID RESOLUTION IN DEG*10.                          00840000
C--    XINCR=CHANGE MADE TO OFFENDING POINTS (IE. WE ALTER ANALYSIS)    00850000
C        FOR 1./4. DEG GRID WBOS USED XINCR=.1                          00860000
C                                                                       00870000
      DO 30 J = 1,NUMPTS                                                00880000
        IF (AMOD(XLON(J),DEG10).EQ. 0.) XLON(J)= XLON(J) + XINCR        00890000
        IF (AMOD(XLAT(J),DEG10).EQ. 0.) XLAT(J)= XLAT(J) + XINCR        00900000
        XLON(J) = XLON(J)/10.                                           00910000
        XLAT(J) = XLAT(J)/10.                                           00920000
        IF(EW(J).EQ.EAST) XLON(J) = 360.0 - XLON(J)                     00930000
   30 CONTINUE                                                          00940000
C                                                                       00950000
C  FILL IN THE 65X65 ICE-COVER GRID                                     00960000
C                                                                       00970000
C        (FOR ICEFLD IN SHEM,  XMESHL=-391. ,  ORIENT=260.)             00980000
C                                                                       00990000
        IF (NS(1).EQ.NORTH) THEN                                        01000000
          XMESHL = 391.                                                 01010000
          ORIENT = 80.                                                  01020000
          TPRT = 'NORTHERN'                                             01030000
        ELSE                                                            01040000
          XMESHL = -391.                                                01050000
          ORIENT = 260.                                                 01060000
          TPRT = 'SOUTHERN'                                             01070000
        ENDIF                                                           01080000
        IPOLE = 33                                                      01090000
        JPOLE = 33                                                      01100000
C                                                                       01110000
C       CALL ICEFLD (IDATA,65,65,IX,JX,XI,XJ,XLAT,XLON,                 01120000
C    1           S,AIL,HORZ,NUMPTS,XMESHL,ORIENT,IPOLE,JPOLE)           01130000
C                                                                       01140000
C  PRINT OUT THE ICE DATA                                               01150000
C                                                                       01160000
C      PRINT 59, (KDATE(I),I=1,3) , TPRT                                01170000
C  59  FORMAT('1ICE DATA:  ',I2,'-',I2,'-',I2,                          01180000
C    &        ////' (65X65) ',A8,' HEMISPHERE GRID:'//)                 01190000
C      DO 60 J = 65,1,-1                                                01200000
C        DO 61 I = 1,65                                                 01210000
C          IIDATA(I,J) = IDATA(I,J)                                     01220000
C  61    CONTINUE                                                       01230000
C        WRITE(6,70) (IIDATA(I,J),I=1,65)                               01240000
C  60  CONTINUE                                                         01250000
C  70  FORMAT(1X,65I1)                                                  01260000
C                                                                       01270000
C  FILL IN THE 2.5-DEGREE 145X37 ICE-COVER GRID                         01280000
C                                                                       01290000
C       CALL ICEGRD(LDATA,145,37,XLON,XLAT,WLON,WLAT,S,AIL,HORZ,NUMPTS, 01300000
C    1    36,ICUT,XMESHL)                                               01310000
C                                                                       01320000
C  PRINT OUT THE 145X37 ICE DATA                                        01330000
C                                                                       01340000
C      PRINT 1159,TPRT                                                  01350000
C1159  FORMAT('1(145X37) ',A8,' HEMISPHERE 2.5-DEGREE GRID:'//)         01360000
C      DO 1160 J = 37,1,-1                                              01370000
C        DO 1161 I = 1,101                                              01380000
C          LLDATA(I,J) = LDATA(I,J)                                     01390000
C1161    CONTINUE                                                       01400000
C        WRITE(6,1170) (LLDATA(I,J),I=1,101)                            01410000
C1160  CONTINUE                                                         01420000
C1170  FORMAT(1X,101I1)                                                 01430000
C      PRINT 1166                                                       01440000
C1166  FORMAT(1X,'+',10(9X,'+')/                                        01450000
C    &        1X,'0',9X, '25',8X, '50',8X, '75',7X,'100',7X,'125',7X,   01460000
C    &         '150',7X,'175',7X,'200',7X,'225',7X,'250')               01470000
C      PRINT 2159, TPRT                                                 01480000
C2159  FORMAT('1(145X37) ',A8,' HEMISPHERE 2.5-DEGREE ',                01490000
C    &    'GRID: (CONTINUED)'//)                                        01500000
C      DO 2160 J = 37,1,-1                                              01510000
C        DO 2161 I = 102,145                                            01520000
C          LLDATA(I,J) = LDATA(I,J)                                     01530000
C2161    CONTINUE                                                       01540000
C        WRITE(6,2170) (LLDATA(I,J),I=101,145)                          01550000
C2160  CONTINUE                                                         01560000
C2170  FORMAT(1X,45I1)                                                  01570000
C      PRINT 2166                                                       01580000
C2166  FORMAT(1X,'+',4(9X,'+'),3X,'+'/                                  01590000
C    &        1X,'250',6X,'275',7X,'300',7X,'325',11X,'360')            01600000
C                                                                       01610000
C                                                                       01620000
C  FILL IN THE 2.0-DEGREE 181X46 GRID ICE-COVER DATA                    01630000
C                                                                       01640000
        CALL ICEGRD(MDATA,181,46,XLON,XLAT,WLON,WLAT,S,AIL,             01650000
     1                       HORZ,NUMPTS,45,ICUT,XMESHL)                01660000
C                                                                       01670000
C  PRINT OUT THE 181X46 ICE DATA FOR NHEM                               01680000
C                                                                       01690000
       PRINT 1359 ,TPRT                                                 01700000
 1359  FORMAT('1(181X46) ',A8,' HEMISPHERE 2.0-DEGREE GRID:'//)         01710000
       DO 1360 J = 46,1,-1                                              01720000
         DO 1361 I = 1,101                                              01730000
           MMDATA(I,J) = MDATA(I,J)                                     01740000
 1361    CONTINUE                                                       01750000
         WRITE(6,1370) (MMDATA(I,J),I=1,101)                            01760000
 1360  CONTINUE                                                         01770000
 1370  FORMAT(1X,101I1)                                                 01780000
       PRINT 1366                                                       01790000
 1366  FORMAT(1X,'+',10(9X,'+')/                                        01800000
     &        1X,'0',9X, '20',8X, '40',8X, '60',8X, '80',7X,'100',7X,   01810000
     &         '120',7X,'140',7X,'160',7X,'180',7X,'200'/               01820000
     &         38X,'DEGREES EAST LONGITUDE')                            01830000
       PRINT 2359                                                       01840000
 2359  FORMAT('1(181X46) ',A8,' HEMISPHERE 2.0-DEGREE GRID: ',          01850000
     &        ' (CONTINUED)'//)                                         01860000
       DO 2360 J = 46,1,-1                                              01870000
         DO 2361 I = 101,181                                            01880000
           MMDATA(I,J) = MDATA(I,J)                                     01890000
 2361    CONTINUE                                                       01900000
         WRITE(6,2370) (MMDATA(I,J),I=101,181)                          01910000
 2360  CONTINUE                                                         01920000
 2370  FORMAT(1X,81I1)                                                  01930000
       PRINT 2366                                                       01940000
 2366  FORMAT(1X,'+',8(9X,'+')/                                         01950000
     &        1X,'200',6X,'220',7X,'240',7X,'260',7X,'280',7X,          01960000
     &           '300',7X,'320',7X,'340',7X,'360')                      01970000
                                                                        01980000
       RETURN                                                           01990000
       END                                                              02000000
