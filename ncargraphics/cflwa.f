      SUBROUTINE CFLWA(ZBB,UBB,ZSS,USS)                                 00010042
C       UM (INPUT - FPS) WIND SPEED                                     00020000
C       ZM (INPUT - FT ) HEIGHT AT WHICH WIND IS INPUT                  00030000
C       TD (INPUT - DEG,C) AIR-SEA TEMPERATURE DIFFERENCE               00040000
C       VST (INPUT - FPS) FRICTION VELOCITY                             00050000
C       Z0 (OUTPUT - FT) ROUGHNESS LENGTH                               00060000
      DATA A/1.641E-04/, B/4.474E-04/, C/1.217E-03/, CF/54.3478/        00080037
C                                                                       00090000
C      WRITE(6,601) ZBB,UBB,ZSS                                         00091042
c  601 FORMAT(1H0,'CFL INPUT - ZBB',F5.1,' UBB',F5.1,'  ZSS',F5.1)      00092042
C                                                                       00092138
      ICNT = 0                                                          00092219
      USS = 0.0                                                         00092448
      IF(UBB.EQ.0.0) USS = 0.0                                          00092647
      IF(UBB.EQ.0.0) RETURN                                             00092747
      UM = UBB/0.3048                                                   00093045
      ZM = ZBB/0.3048                                                   00094045
      ZS = ZSS/0.3048                                                   00095048
      IF(UM.LT.0.0) UM = 0.0                                            00100000
      VST = .04*UM                                                      00110000
C                                                                       00110138
C     WRITE(6,603) ICNT,VST                                             00111042
C 603 FORMAT(1H ,'ICNT',I5,'   VST ',F9.4,'F/S')                        00112042
C                                                                       00113038
      VSTN = VST                                                        00120000
      IF(VST.LE.0.0) GO TO 16                                           00130000
      FFF = ZM/(A/VST+B*VST**2+C)                                       00150000
      IF(FFF.LT.0.0) GO TO 12                                           00160011
C                                                                       00170038
C ... MAIN CONVERGENCE LOOP ... ... ... ... ... ...                     00171038
C                                                                       00172038
 1000 VSTN = (0.4*UM)/(ALOG(ZM/(A/VST+B*VST**2+C)))                     00180007
C                                                                       00180138
C     WRITE(6,604) ICNT,VSTN                                            00181042
C 604 FORMAT(1H ,'ICNT',I5,'   VSTN ',F9.4,'F/S')                       00183042
C                                                                       00184038
      IF(ABS(VSTN-VST).LT.0.005) GO TO 1400                             00190000
      VST = VSTN                                                        00200000
      ICNT = ICNT + 1                                                   00210000
      IF(ICNT.LT.25) GO TO 1000                                         00220038
C                                                                       00221038
C ... END END END ... ... ... ... ... ... ... ... ...                   00222038
C                                                                       00223038
      IF(ICNT.GE.25) WRITE(6,699)                                       00230005
  699 FORMAT(1H ,'CFL SOLUTION DOES NOT CONVERGE')                      00240005
C                                                                       00241038
 1400 CONTINUE                                                          00250012
C                                                                       00401038
   12 Z0 = A/VSTN + B*VSTN**2 + C                                       00410000
      USS = (VST/0.4)*ALOG(ZS/Z0)                                       00410349
      USS = USS*0.3048                                                  00411148
      VST = VSTN*0.3048                                                 00411334
      Z0 = Z0*0.3048                                                    00411427
C                                                                       00411627
C      WRITE(6,608) VST,Z0,ZSS,USS                                      00412048
C  608 FORMAT(1H ,'CFL METRIC - VST',F6.2,'  Z0',F7.4,                  00413042
C     *'   UMM',F6.1,'   U20',F6.1)                                     00414046
      IF(VST.GT.0.0) RETURN                                             00420034
   16 Z0 = 0.0                                                          00430000
      IF(VSTN.EQ.0.0) RETURN                                            00450000
      VST = 0.4*UM                                                      00460004
      VSTN = VST                                                        00470000
      GO TO 12                                                          00480000
      END                                                               00500000
