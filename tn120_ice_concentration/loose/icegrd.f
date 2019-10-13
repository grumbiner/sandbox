C          DATA SET ICEGRD     AT LEVEL 028 AS OF 11/15/85              00000010
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK                                 00000020
C                                                                       00000030
C SUBPROGRAM: ICEGRD         CONNECTS THE DOTS AND FILLS IN A GRID      00000040
C   AUTHOR: STACKPOLE/COBBS  ORG: W/NMC42    DATE: 85-10-07             00000050
C                                                                       00000060
C ABSTRACT: TAKES ORDERED LAT,LON POINTS, AND FILLS, ON A               00000070
C   PREDETERMINED GRID, A POLYGON DEFINED BY CONNECTING THE LON/LAT     00000080
C   POINTS IN ORDER.                                                    00000090
C                                                                       00000100
C                                                                       00000110
C USAGE:  CALL ICEGRD(ICE,IMAX,JMAX,CCI,CCJ,CI,CJ,                      00000120
C                     S,AJL,VERT,NUMPTS,NEP,ICUT,XMESHL)                00000130
C                                                                       00000140
C   INPUT VARIABLES:                                                    00000150
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES               00000160
C     ------ --------- -----------------------------------------------  00000170
C     IMAX   ARG LIST  NUMBER OF COLUMNS IN THE OUTPUT GRID             00000180
C     JMAX   ARG LIST  NUMBER OF ROWS IN THE OUTPUT GRID                00000190
C     CCI    ARG LIST  LONGITUDE VALUES FOR THE ORDERED POINTS          00000200
C     CCJ    ARG LIST  LATITUDE VALUES FOR THE ORDERED POINTS           00000210
C     NUMPTS ARG LIST  LATITUDE VALUES FOR THE ORDERED POINTS           00000220
C     NEP    ARG LIST  LATITUDE VALUES FOR THE ORDERED POINTS           00000230
C     XMESHL ARG LIST  MESH LENGTH AT 60N (XMESHL<0 INDICATES SHEM)     00000240
C                                                                       00000250
C   OUTPUT VARIABLES:                                                   00000260
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES               00000270
C     ------ --------- -----------------------------------------------  00000280
C     ICE    ARG LIST  OUTPUT GRID                                      00000290
C                                                                       00000300
C   REMARKS: THE LON/LAT POINTS AND THE OUTPUT GRID ARE ALL ARRAYS      00000310
C     OF REAL NUMBERS.                                                  00000320
C     ALSO, NOTE THAT:  CI  =  XLON                                     00000330
C                       CJ  =  XLAT,                                    00000340
C     SO THAT CI REMAINS THE COLUMN COUNTER AND CJ REMAINS              00000350
C     THE ROW COUNTER.  NOTE ALSO THAT CCI AND CCJ ARE THE              00000360
C     INPUT VALUES OF LON/LAT, BUT WORK SPACES CI AND CJ ARE            00000370
C     CREATED AND USED TO PRESERVE THE ORIGINAL DATA.                   00000380
C                                                                       00000390
C     SOUTHERN HEMISPHERE CONSIDERATIONS:                               00000400
C     FOR A SHEM GRID THE DATA POINTS ARE STILL POSITIVE,               00000410
C     SO TO GET THE SOUTH POLE AT THE BOTTOM OF THE                     00000420
C     LAT/LON GRIDS, THE GRID MUST BE INVERTED.                         00000430
C            ROW-NUMBER = JMAX - ROW NUMBER + 1                         00000440
C                                                                       00000450
C     THEN ICEGRD FILLS FROM THE LINE ACROSS THE GRID - UPWARDS!        00000460
C     THEREFORE AT THE VERY END OF THE PROGRAM ALL OF THE GRID          00000470
C     VALUES ARE REVERSED TO PLACE ICE OVER THE POLE.                   00000480
C                                                                       00000490
C                                                                       00000500
C     JOHN STACKPOLE ACTUALLY INVENTED THIS SCHEME FOR THE OPERATIONAL  00000510
C     CODE SNOGRDAL;I (W.BOSTELMAN) HAVE MODIFIED IT FOR MY PURPOSES.   00000520
C                                                                       00000530
C ATTRIBUTES:                                                           00000540
C   LANGUAGE: FORTRAN 77                                                00000550
C   SOURCE STATEMENTS:                                                  00000560
C                                                                       00000570
C$$$                                                                    00000580
      SUBROUTINE ICEGRD(ICE,IMAX,JMAX,CCI,CCJ,CI,CJ,S,AJL,              00000590
     1              VERT,NUMPTS,NEP,ICUT,XMESHL)                        00000600
C                                                                       00000610
      INTEGER*4  ICUT(NUMPTS)                                           00000620
      REAL*4     ICE(IMAX,JMAX), CCI(NUMPTS),CCJ(NUMPTS)                00000630
      REAL*4     CI(NUMPTS),CJ(NUMPTS),S(NUMPTS),AJL(NUMPTS)            00000640
      LOGICAL*1  VERT(NUMPTS),INSIDE                                    00000650
C                                                                       00000660
      IMAXM = IMAX - 1                                                  00000670
      JMAXM = JMAX - 1                                                  00000680
C                                                                       00000690
      XDEG = 90./NEP                                                    00000700
      YDEG = XDEG                                                       00000710
C                                                                       00000720
C      CHANGE LAT/LON TO GRID INTERVALS                                 00000730
C                                                                       00000740
      NP = NUMPTS                                                       00000750
C                                                                       00000760
      DO 1 K = 1,NP                                                     00000770
        CI(K) =  (360.-CCI(K)) / XDEG + 1.                              00000780
        CJ(K) =  CCJ(K) / YDEG + 1.                                     00000790
        IF (XMESHL.LT.0) CJ(K) = JMAX - CJ(K) + 1                       00000800
    1 CONTINUE                                                          00000810
C                                                                       00000820
C    ELIMINATE DUPLICATE POINTS                                         00000830
C                                                                       00000840
       LESS = 0                                                         00000850
       DO 500 J = 1,NP                                                  00000860
         JP = MOD(J,NP) + 1                                             00000870
         IF(CI(J).EQ.CI(JP). AND .CJ(J).EQ.CJ(JP)) THEN                 00000880
           LESS = LESS + 1                                              00000890
           GOTO 500                                                     00000900
         ENDIF                                                          00000910
         JJ = J - LESS                                                  00000920
         CI(JJ) = CI(J)                                                 00000930
         CJ(JJ) = CJ(J)                                                 00000940
  500  CONTINUE                                                         00000950
       NP = NP - LESS                                                   00000960
C                                                                       00000970
C      ZERO THE OUTPUT GRID                                             00000980
C                                                                       00000990
      DO 2 I = 1,IMAX                                                   00001000
      DO 2 J=1,JMAX                                                     00001010
      ICE(I,J) = 0.0                                                    00001020
    2 CONTINUE                                                          00001030
C                                                                       00001040
C      COMPUTE SLOPES                                                   00001050
C                                                                       00001060
      IFCUT = IMAX* 0.66667                                             00001070
      DO 3 K = 1,NP                                                     00001080
      ICUT(K) = 0                                                       00001090
      KP = MOD(K,NP) + 1                                                00001100
      CIP = CI(KP)                                                      00001110
      IF(CI(K)-CI(KP).GT.IFCUT)  ICUT(K) = 1                            00001120
      IF(CI(KP)-CI(K).GT.IFCUT)  ICUT(K) = 2                            00001130
      VERT(K) = .FALSE.                                                 00001140
      IF(CI(K).EQ.CI(KP)) VERT(K) = .TRUE.                              00001150
      IF(ICUT(K).EQ.1) CIP = CIP + IMAX                                 00001160
      IF(ICUT(K).EQ.2) CIP = CIP - IMAX                                 00001170
      IF(VERT(K)) GO TO 3                                               00001180
      S(K) = (CJ(KP) - CJ(K))/(CIP - CI(K))                             00001190
    3 CONTINUE                                                          00001200
C                                                                       00001210
C      SCAN FOR ICE BY MID POINT OF CLOUMN                              00001220
C                                                                       00001230
      DO 8 I = 1,IMAX                                                   00001240
      INSIDE= .FALSE.                                                   00001250
      AI = I                                                            00001260
C                                                                       00001270
C      CALCULATE J INTERSECTION FOR I VALUE                             00001280
C                                                                       00001290
      DO 4 K = 1,NP                                                     00001300
      KP = MOD(K,NP) + 1                                                00001310
      XI = AI                                                           00001320
      IF(ICUT(K).EQ.1.AND.AI.LE.CI(KP)) XI = AI + IMAX                  00001330
      IF(ICUT(K).EQ.2.AND.AI.GE.CI(KP)) XI = AI - IMAX                  00001340
      IF(VERT(K)) GO TO 4                                               00001350
      AJL(K) = CJ(K) + (XI-CI(K))*S(K)                                  00001360
    4 CONTINUE                                                          00001370
      DO 7 J=1,JMAX                                                     00001380
      L = J                                                             00001390
      AJ = J                                                            00001400
      AJP = AJ + 1.                                                     00001410
C                                                                       00001420
      DO 5 K = 1,NP                                                     00001430
      KP = MOD(K,NP) + 1                                                00001440
C     XI = XI + .0001                                                   00001450
      CIP = CI(KP)                                                      00001460
      IF(VERT(K)) GO TO 5                                               00001470
      IF(ICUT(K).EQ.1) GO TO 14                                         00001480
      IF(ICUT(K).EQ.2) GO TO 15                                         00001490
      GO TO 6                                                           00001500
   14 CIP = CIP + IMAX                                                  00001510
      IF(AI.LE.CI(KP)) XI = AI + IMAX                                   00001520
      GO TO 6                                                           00001530
   15 CIP = CIP - IMAX                                                  00001540
      IF(AI.GE.CI(KP)) XI = AI - IMAX                                   00001550
C                                                                       00001560
    6 CONTINUE                                                          00001570
      IF(XI.EQ.CI(K))  XI = XI + .0001                                  00001580
      IF(AJ.EQ.AJL(K)) AJ = AJ + .0001                                  00001590
      IF((AJ-AJL(K))*(AJP-AJL(K)).LE.0.0                                00001600
     1      .AND.(XI-CI(K))*(XI-CIP).LT.0.0)  GOTO 10                   00001610
      GO TO 5                                                           00001620
   10 INSIDE = .NOT.INSIDE                                              00001630
      IF((AJL(K)-AJ).GT.0.)  GO TO 11                                   00001640
      GO TO 5                                                           00001650
   11 IF(INSIDE) L = J + 1                                              00001660
      IF(.NOT. INSIDE) ICE(I,J) = 1.0                                   00001670
    5 CONTINUE                                                          00001680
      IF(INSIDE) ICE(I,L)= 1.0                                          00001690
    7 CONTINUE                                                          00001700
    8 CONTINUE                                                          00001710
      DO 9 J = 1,JMAX                                                   00001720
C     ICE(IMAX,J) = ICE(1,J)                                            00001730
      ICE(1,J) = ICE(IMAX,J)                                            00001740
    9 CONTINUE                                                          00001750
C                                                                       00001760
      IF (XMESHL.LT.0) THEN                                             00001770
        DO 106 J = 1,JMAX                                               00001780
          DO 107 I = 1,IMAX                                             00001790
            IF (ICE(I,J).EQ.0) THEN                                     00001800
              ICE(I,J) = 1.                                             00001810
            ELSE                                                        00001820
              ICE(I,J) = 0.                                             00001830
            ENDIF                                                       00001840
  107     CONTINUE                                                      00001850
  106   CONTINUE                                                        00001860
      ENDIF                                                             00001870
C                                                                       00001880
      RETURN                                                            00001890
C                                                                       00001900
  103 FORMAT(18F7.2)                                                    00001910
      END                                                               00001920
