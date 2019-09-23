      PROGRAM ABC                                                       0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
      EQUIVALENCE ( DELT,     STA ( 13 ) ),                             0000000
     *            ( STPTIM,   STA ( 15 ) ),                             0000000
     *            ( FCSTL,    STA ( 16 ) )                              0000000
C                                                                       0000000
      SAVE                                                              0000000
C                                                                       0000000
C====================================================================   0000000
C                                                                       0000000
      PRINT 101                                                         0000000
C                                                                       0000000
C     LOAD ARRAYS WITH DUMMY VALUES FOR BENCHMARK TIMING                0000000
C                                                                       0000000
      CALL SETUP                                                        0000000
C                                                                       0000000
      ITPLT = 1800                                                      0000000
      IFSLP = 0                                                         0000000
      ITSM  = 999 * 3600                                                0000000
      IFSLS = 0                                                         0000000
C                                                                       0000000
      CALL ENERGY                                                       0000000
C                                                                       0000000
      CALL DIAG(PRM,WRK,LEN,NX,NY,NZA)                                  0000000
C                                                                       0000000
      I = 0                                                             0000000
C                                                                       0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
         I = I + 1                                                      0000000
         CALL DYN                                                       0000000
         FCSTL = FCSTL + DELT                                           0000000
C                                                                       0000000
         IFSLS = IFSLS + NINT ( DELT )                                  0000000
         IF ( IFSLS .GE. ITSM ) THEN                                    0000000
             CALL SM                                                    0000000
             IFSLS = 0                                                  0000000
         END IF                                                         0000000
C                                                                       0000000
         IFSLP = IFSLP + NINT ( DELT )                                  0000000
         IF ( IFSLP .GE. ITPLT ) THEN                                   0000000
             CALL DIAG(PRM,WRK,LEN,NX,NY,NZA)                           0000000
             IFSLP = 0                                                  0000000
         END IF                                                         0000000
C                                                                       0000000
         PRINT 103, I                                                   0000000
         CALL ENERGY                                                    0000000
C                                                                       0000000
         IF ( FCSTL .GE. STPTIM ) GOTO 2                                0000000
C                                                                       0000000
         GOTO 1                                                         0000000
2     CONTINUE                                                          0000000
C                                                                       0000000
      PRINT 102                                                         0000000
C                                                                       0000000
101   FORMAT(' *** SPLIT-EXPLICIT MODEL STARTING ***')                  0000000
102   FORMAT(' *** RAN TO PLANNED COMPLETION ***')                      0000000
103   FORMAT(' I=',2X,I10)                                              0000000
C                                                                       0000000
C                                                                       0000000
      STOP                                                              0000000
      END                                                               0000000
      SUBROUTINE ENERGY                                                 0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
C     ADDRESSES FOR PRM                                                 0000000
C                                                                       0000000
      IADU   = IGADR ( 'U MOMENTUM' )                                   0000000
      IADV   = IGADR ( 'V MOMENTUM' )                                   0000000
      IADT   = IGADR ( 'POT TEMP  ' )                                   0000000
      IADQ   = IGADR ( 'SPEC HUMID' )                                   0000000
      IADH   = IGADR ( 'SURF PRES ' )                                   0000000
      IADSP  = IGADR ( 'SINPHI    ' )                                   0000000
      IADTER = IGADR ( 'TERRAIN   ' )                                   0000000
C                                                                       0000000
C     COMPUTE TOTAL MASS                                                0000000
C                                                                       0000000
      SUM = 0.                                                          0000000
      DO 1 J = 1, NY                                                    0000000
      DO 1 I = 1, NX - 3                                                0000000
         II = ( J - 1 ) * NX + I                                        0000000
         SUM = SUM + PRM ( II, IADH )                                   0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
      PRINT 100, SUM                                                    0000000
100   FORMAT(' TOTAL MASS =',2X,E15.5)                                  0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE DIAG(PRM,WRK,LEN,NX,NY,NZ)                             0000000
C                                                                       0000000
      DIMENSION PRM(LEN,1)                                              0000000
      DIMENSION WRK(LEN,1)                                              0000000
      DIMENSION SPV(2)                                                  0000000
C                                                                       0000000
      DATA SPV/2*32767./                                                0000000
C                                                                       0000000
C     GET ADDRESSES                                                     0000000
C                                                                       0000000
      IADU = IGADR ( 'U MOMENTUM' )                                     0000000
      IADV = IGADR ( 'V MOMENTUM' )                                     0000000
      IADT = IGADR ( 'POT TEMP  ' )                                     0000000
      IADH = IGADR ( 'SURF PRES ' )                                     0000000
C                                                                       0000000
C     SURFACE PRESSURE                                                  0000000
C                                                                       0000000
      DO 1 I = 1, LEN                                                   0000000
         WRK(I,1) = PRM(I,IADH) * 1000.                                 0000000
1     CONTINUE                                                          0000000
C      FINC = 2.                                                         0000000
C      CALL CONREC(WRK(1,1),NX,NX,NY,0.,0.,FINC,0,0,1)                   0000000
C      CALL FRAME                                                        0000000
C                                                                       0000000
C     U AND V MOMENTUM AND THETA                                        0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      DO 3 K = 1, NZ                                                    0000000
         IADUX = IADUX + 1                                              0000000
         IADVX = IADVX + 1                                              0000000
         IADTX = IADTX + 1                                              0000000
         DO 2 I = 1, LEN                                                0000000
            WRK(I,1) = SQRT( PRM(I, IADUX)**2 + PRM(I, IADVX)**2 )      0000000
2        CONTINUE                                                       0000000
C         CALL VELVCT(PRM(1,IADUX),NX,PRM(1,IADVX),NX,NX-1,NY-1,         0000000
C     *               0.,0.,0,0,4,SPV)                                   0000000
C         FINC = 5.                                                      0000000
C         CALL CONREC(WRK(1,1),NX,NX-1,NY-1,0.,0.,FINC,0,0,682)          0000000
C         CALL FRAME                                                     0000000
C         FINC = 2.                                                      0000000
C         CALL CONREC(PRM(1,IADTX),NX,NX,NY,0.,0.,FINC,0,0,1)            0000000
C         CALL FRAME                                                     0000000
3     CONTINUE                                                          0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE DISP ( X, NX, NY, NXP, NYP, IFMT )                     0000000
      DIMENSION X ( NX, NY )                                            0000000
      PRINT 90                                                          0000000
90    FORMAT(/)                                                         0000000
      DO 1 J = NYP, 1, -1                                               0000000
         IF ( IFMT .EQ. 1 )PRINT 100, ( X ( I, J ), I = 1, NXP )        0000000
         IF ( IFMT .EQ. 2 )PRINT 200, ( X ( I, J ), I = 1, NXP )        0000000
         IF ( IFMT .EQ. 3 )PRINT 300, ( X ( I, J ), I = 1, NXP )        0000000
1     CONTINUE                                                          0000000
100   FORMAT(' ',9F6.3)                                                 0000000
200   FORMAT(' ',9F8.1)                                                 0000000
300   FORMAT(' ',9E9.2)                                                 0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE SETUP                                                  0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      PARAMETER ( ONE    = 1.,                                          0000000
     *            ZERO   = 0.,                                          0000000
     *            P25    = 0.25,                                        0000000
     *            P5     = 0.5,                                         0000000
     *            TWOMEG = 2. * 7.29E-05,                               0000000
     *            P609   = 0.609,                                       0000000
     *            G      = 9.8,                                         0000000
     *            R      = 287.04,                                      0000000
     *            CP     = 1004.64 )                                    0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
      DIMENSION SL   ( NZA ),                                           0000000
     *          DSIG ( NZA ),                                           0000000
     *          SKAP ( NZA )                                            0000000
C                                                                       0000000
      DIMENSION AMULT ( LEN ),                                          0000000
     *          WRK1  ( LEN ),                                          0000000
     *          WRK2  ( LEN ),                                          0000000
     *          AMF   ( LEN ),                                          0000000
     *          PI    ( LEN ),                                          0000000
     *          PIX   ( LEN ),                                          0000000
     *          PIY   ( LEN ),                                          0000000
     *          PGC   ( LEN ),                                          0000000
     *          THOLD ( LEN ),                                          0000000
     *          GEOP  ( LEN )                                           0000000
C                                                                       0000000
      EQUIVALENCE ( SL     ( 1 ), STA ( 101   ) ),                      0000000
     *            ( DSIG   ( 1 ), STA ( 201   ) ),                      0000000
     *            ( SKAP   ( 1 ), STA ( 301   ) ),                      0000000
     *            ( SP0,          STA ( 8     ) ),                      0000000
     *            ( DELX,         STA ( 9     ) ),                      0000000
     *            ( DELT,         STA ( 13    ) ),                      0000000
     *            ( AITADJ,       STA ( 14    ) ),                      0000000
     *            ( STPTIM,       STA ( 15    ) ),                      0000000
     *            ( FCSTL,        STA ( 16    ) )                       0000000
C                                                                       0000000
      EQUIVALENCE ( THS,          STA ( 51    ) ),                      0000000
     *            ( DTHS,         STA ( 52    ) ),                      0000000
     *            ( GAMT,         STA ( 53    ) ),                      0000000
     *            ( DGAMT,        STA ( 54    ) ),                      0000000
     *            ( GAMS,         STA ( 55    ) ),                      0000000
     *            ( DGAMS,        STA ( 56    ) ),                      0000000
     *            ( STARTS,       STA ( 57    ) ),                      0000000
     *            ( A,            STA ( 58    ) ),                      0000000
     *            ( B,            STA ( 59    ) ),                      0000000
     *            ( YSTAR,        STA ( 60    ) )                       0000000
C                                                                       0000000
      EQUIVALENCE ( WRK1  ( 1 ), WRK ( 1,  1 ) ),                       0000000
     *            ( WRK2  ( 1 ), WRK ( 1,  2 ) ),                       0000000
     *            ( AMULT ( 1 ), WRK ( 1,  3 ) ),                       0000000
     *            ( AMF   ( 1 ), WRK ( 1,  4 ) ),                       0000000
     *            ( PI    ( 1 ), WRK ( 1,  5 ) ),                       0000000
     *            ( PIX   ( 1 ), WRK ( 1,  6 ) ),                       0000000
     *            ( PIY   ( 1 ), WRK ( 1,  7 ) ),                       0000000
     *            ( PGC   ( 1 ), WRK ( 1,  8 ) ),                       0000000
     *            ( THOLD ( 1 ), WRK ( 1,  9 ) ),                       0000000
     *            ( GEOP  ( 1 ), WRK ( 1, 10 ) )                        0000000
C                                                                       0000000
      PARAMETER ( LENHB = LEN - 1 * NX - 1 )                            0000000
C                                                                       0000000
      SAVE                                                              0000000
C                                                                       0000000
C     ADDRESSES FOR PRM                                                 0000000
C                                                                       0000000
      IADU   = IGADR ( 'U MOMENTUM' )                                   0000000
      IADV   = IGADR ( 'V MOMENTUM' )                                   0000000
      IADT   = IGADR ( 'POT TEMP  ' )                                   0000000
      IADQ   = IGADR ( 'SPEC HUMID' )                                   0000000
      IADH   = IGADR ( 'SURF PRES ' )                                   0000000
      IADSP  = IGADR ( 'SINPHI    ' )                                   0000000
      IADTER = IGADR ( 'TERRAIN   ' )                                   0000000
C                                                                       0000000
C     INITIALIZE U, V, T, Q TO ZERO                                     0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      IADQX = IADQ - 1                                                  0000000
C                                                                       0000000
      DO 2 K = 1, NZA                                                   0000000
C                                                                       0000000
         IADUX = IADUX + 1                                              0000000
         IADVX = IADVX + 1                                              0000000
         IADTX = IADTX + 1                                              0000000
         IADQX = IADQX + 1                                              0000000
C                                                                       0000000
         DO 1 I = 1, LEN                                                0000000
            PRM ( I, IADUX ) = ZERO                                     0000000
            PRM ( I, IADVX ) = ZERO                                     0000000
            PRM ( I, IADTX ) = ZERO                                     0000000
            PRM ( I, IADQX ) = ZERO                                     0000000
1        CONTINUE                                                       0000000
C                                                                       0000000
2     CONTINUE                                                          0000000
C                                                                       0000000
C     SETUP REASONABLE VALUES FOR DSIG, SIGMA AND SIG TO THE KAPPA      0000000
C                                                                       0000000
      DO 3 K = 1, NZA                                                   0000000
         DSIG ( K ) = - ONE / REAL ( NZA )                              0000000
3     CONTINUE                                                          0000000
C                                                                       0000000
      SLO = ONE                                                         0000000
      DO 4 K = 1, NZA                                                   0000000
         SHI = SLO + DSIG ( K )                                         0000000
         SL ( K ) = ( SLO + SHI ) * P5                                  0000000
         SKAP ( K ) = SL ( K ) ** ( R / CP )                            0000000
         SLO = SHI                                                      0000000
         PRINT 832, K, DSIG(K), SL(K), SKAP(K)                          0000000
832      FORMAT(' K, DSIG,SL,SKAP = ',2X,I5,2X,3(E15.5,2X))             0000000
4     CONTINUE                                                          0000000
C                                                                       0000000
C     SETUP THE SIN OF THE LAT DELX IS TRUE AT,                         0000000
C     DELX                                                              0000000
C     DELT                                                              0000000
C     HOW MANY GRAVITY TIMESTEPS PER DELT                               0000000
C     HOW LONG TO FORECAST FOR IN SECS                                  0000000
C     LENGTH OF FORECAST SO FAR IN SECS                                 0000000
C                                                                       0000000
      SP0    = 1.                                                       0000000
      DELT   = 1800.                                                    0000000
      AITADJ = 2.                                                       0000000
      GWS    = 380. + 120.                                              0000000
      DELX   = ( GWS * SQRT ( 2. ) * DELT * 0.5 ) / AITADJ              0000000
      DELX   = DELX * 1.02                                              0000000
C                                                                       0000000
C     READ DELX, DELT, AITADJ, STPTIM                                   0000000
C
      DELX = 400000.0
      DELT = 1800.0
      AITADJ = 2.
      STPTIM = 72.0
C                                                                       0000000
C     READ(5,7211) DELX, DELT, AITADJ, STPTIM                           0000000
7211  FORMAT(F8.1,1X,F8.1,1X,F8.1,1X,F8.1)                              0000000
C                                                                       0000000
      PRINT 773, DELX, DELT, AITADJ, STPTIM                             0000000
773   FORMAT(' *** DELX, DELT, AITADJ, STPTIM =',2X,4(F8.1,2X))         0000000
      STPTIM = STPTIM * 3600.                                           0000000
      FCSTL  = ZERO                                                     0000000
C                                                                       0000000
C     SETUP REASONABLE VALUES FOR H, SIN OF THE LATITUDE AND TERRAIN    0000000
C                                                                       0000000
      DO 5 I = 1, LEN                                                   0000000
         PRM ( I, IADH   ) = ONE                                        0000000
         PRM ( I, IADSP  ) = ONE                                        0000000
         PRM ( I, IADTER ) = ZERO * G                                   0000000
5     CONTINUE                                                          0000000
C                                                                       0000000
C     CREATE JET                                                        0000000
C                                                                       0000000
      THS    = 300.                                                     0000000
      DTHS   = 4.                                                       0000000
      GAMT   = 200.                                                     0000000
      DGAMT  = 100.                                                     0000000
      GAMS   = 400.                                                     0000000
      DGAMS  = -200.                                                    0000000
      STARTS = 7.                                                       0000000
      A      = 5.                                                       0000000
      B      = 2.                                                       0000000
      YSTAR  = 20.                                                      0000000
C                                                                       0000000
C     SETUP MULTIPLIER FOR GENERATING INITIAL DATA                      0000000
C     INITIALIZE FIRST SIGMA LAYER THETA                                0000000
C                                                                       0000000
      XCON  = ATAN ( 1. ) * 8. / FLOAT ( NX - 3 )                       0000000
      AJ0   = FLOAT ( (NY / 2) + 1 )                                    0000000
      ALATN = 65.                                                       0000000
      ALATS = 25.                                                       0000000
      SPN   = SIN ( ALATN * 3.14159 / 180. )                            0000000
      SPS   = SIN ( ALATS * 3.14159 / 180. )                            0000000
      DSP   = ( SPN - SPS ) / FLOAT ( NY - 1 )                          0000000
C                                                                       0000000
      IADTX = IADT                                                      0000000
C                                                                       0000000
      DO 802 J = 1, NY                                                  0000000
         YCON = ( AJ0 - FLOAT ( J ) ) * ( ONE / YSTAR )                 0000000
         II = ( J - 1 ) * NX                                            0000000
         SPA = SPS + FLOAT ( J - 1 ) * DSP                              0000000
         DO 801 I = 1, NX                                               0000000
            II = II + 1                                                 0000000
            AMULT ( II ) = TANH(YCON*(A+B*SIN(XCON*FLOAT(I-1))))        0000000
            PRM (II, IADTX)= THS + DTHS * AMULT ( II )                  0000000
CCCC        PRM (II, IADSP)= SPA                                        0000000
801      CONTINUE                                                       0000000
802   CONTINUE                                                          0000000
C                                                                       0000000
C      CALL DISP ( AMULT, NX, NY, NX, NY, 1 )                            0000000
C                                                                       0000000
C      CALL DISP ( PRM(1,IADTX), NX, NY, NX, NY, 2 )                     0000000
C                                                                       0000000
C     INITIALIZE REMAINING SIGMA LAYER THETAS                           0000000
C                                                                       0000000
      DO 804 K = 2, NZA                                                 0000000
         IADTX = IADTX + 1                                              0000000
         IF ( K .LT. NINT ( STARTS ) ) THEN                             0000000
            GAM  = GAMT                                                 0000000
            DGAM = DGAMT                                                0000000
         ELSE                                                           0000000
            GAM  = GAMS                                                 0000000
            DGAM = DGAMS                                                0000000
         END IF                                                         0000000
C                                                                       0000000
C        DSIG IS POSITIVE                                               0000000
C                                                                       0000000
         DSL = SL ( K - 1 ) - SL ( K )                                  0000000
C                                                                       0000000
         DO 803 I = 1, LEN                                              0000000
            DTH = DSL * ( GAM + DGAM * AMULT ( I ) )                    0000000
            PRM ( I, IADTX ) = PRM ( I, IADTX - 1 ) + DTH               0000000
803      CONTINUE                                                       0000000
C                                                                       0000000
CCC      CALL DISP ( PRM(1,IADTX), NX, NY, NX, NY, 2 )                  0000000
C                                                                       0000000
804   CONTINUE                                                          0000000
C                                                                       0000000
C     APPLY BOUNDARY CONDITIONS                                         0000000
C                                                                       0000000
      CALL BC                                                           0000000
C                                                                       0000000
C     INITIALIZE U AND V WINDS                                          0000000
C                                                                       0000000
C     COMPUTE 0.5 * M / ( DX * F )                                      0000000
C                                                                       0000000
      DO 501 I = 1, LENHB + 1                                           0000000
         WRK1 ( I ) = PRM ( I, IADSP ) + PRM ( I + NX, IADSP )          0000000
501   CONTINUE                                                          0000000
      TEMP1 =  P5 * ( ONE + SP0 ) / DELX                                0000000
      DO 502 I = 1, LENHB                                               0000000
         TEMP3 = ( WRK1 ( I ) + WRK1 ( I + 1 ) ) * P25                  0000000
         AMF ( I ) = ( TEMP1 / ( ONE + TEMP3 ) ) / ( TWOMEG * TEMP3 )   0000000
502   CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE PI, PIX AND PIY                                           0000000
C                                                                       0000000
      CALL P2KAP ( PRM ( 1, IADH ), PI ( 1 ), LEN )                     0000000
      DO 503 I = 1, LENHB + 1                                           0000000
         PIX ( I ) = PI ( I + NX ) + PI ( I )                           0000000
         PIY ( I ) = PI ( I + NX ) - PI ( I )                           0000000
503   CONTINUE                                                          0000000
      DO 504 I = 1, LENHB                                               0000000
         PIX ( I ) = PIX ( I + 1 ) - PIX ( I )                          0000000
         PIY ( I ) = PIY ( I + 1 ) + PIY ( I )                          0000000
504   CONTINUE                                                          0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      IADQX = IADQ - 1                                                  0000000
C                                                                       0000000
      DO 601 K = 1, NZA                                                 0000000
C                                                                       0000000
         IADUX = IADUX + 1                                              0000000
         IADVX = IADVX + 1                                              0000000
         IADTX = IADTX + 1                                              0000000
         IADQX = IADQX + 1                                              0000000
C                                                                       0000000
C        COMPUTE VIRTUAL TEMPERATURE                                    0000000
C                                                                       0000000
         DO 602 I = 1, LEN                                              0000000
            WRK1 ( I ) = ( ONE + P609 * PRM ( I, IADQX ) ) *            0000000
     *      PRM ( I, IADTX )                                            0000000
602      CONTINUE                                                       0000000
C                                                                       0000000
C        COMPUTE GEOPOTENTIAL                                           0000000
C                                                                       0000000
         IF ( K .EQ. 1 ) THEN                                           0000000
            TEMP1 = ( ONE - SKAP ( 1 ) ) * CP                           0000000
            DO 603 I = 1, LEN                                           0000000
               GEOP ( I ) = PRM ( I, IADTER ) + TEMP1 * WRK1 ( I ) *    0000000
     *         PI ( I )                                                 0000000
603         CONTINUE                                                    0000000
         ELSE                                                           0000000
            TEMP1 = ( SKAP ( K - 1 ) - SKAP ( K ) ) * ( CP * P5 )       0000000
            DO 604 I = 1, LEN                                           0000000
               GEOP ( I ) = GEOP ( I ) + TEMP1 *                        0000000
     *         ( WRK1 ( I ) + THOLD ( I ) ) * PI ( I )                  0000000
604         CONTINUE                                                    0000000
         END IF                                                         0000000
C                                                                       0000000
CC       CALL DISP(GEOP, NX, NY, NX-1, NY, 2 )                          0000000
C                                                                       0000000
C        SAVE VIRTUAL TEMPERATURE                                       0000000
C                                                                       0000000
         IF ( K .NE. NZA ) THEN                                         0000000
            DO 605 I = 1, LEN                                           0000000
               THOLD ( I ) = WRK1 ( I )                                 0000000
 605        CONTINUE                                                    0000000
         END IF                                                         0000000
C                                                                       0000000
C        COMPUTE CP * THETAV * SKAP                                     0000000
C                                                                       0000000
         DO 606 I = 1, LENHB + 1                                        0000000
            PGC ( I ) = WRK1 ( I ) + WRK1 ( I + NX )                    0000000
606      CONTINUE                                                       0000000
         TEMP1 = ( P25 * CP ) * SKAP ( K )                              0000000
         DO 607 I = 1, LENHB                                            0000000
            PGC ( I ) = TEMP1 * ( PGC ( I ) + PGC ( I + 1 ) )           0000000
607      CONTINUE                                                       0000000
C                                                                       0000000
C        COMPUTE U AND V GEO                                            0000000
C                                                                       0000000
         DO 608 I = 1, LENHB + 1                                        0000000
            WRK1 ( I ) = GEOP ( I + NX ) + GEOP ( I )                   0000000
            WRK2 ( I ) = GEOP ( I ) - GEOP ( I + NX )                   0000000
608      CONTINUE                                                       0000000
         DO 609  I = 1, LENHB                                           0000000
            PRM ( I, IADVX ) = ( WRK1 ( I + 1 ) - WRK1 ( I ) +          0000000
     *      PGC ( I ) * PIX ( I ) ) * AMF ( I )                         0000000
            PRM ( I, IADUX ) = ( WRK2 ( I + 1 ) + WRK2 ( I ) -          0000000
     *      PGC ( I ) * PIY ( I ) ) * AMF ( I )                         0000000
609      CONTINUE                                                       0000000
C                                                                       0000000
C         PRINT 6773                                                     0000000
c6773     FORMAT(' U WIND')                                              0000000
C         CALL DISP(PRM(1,IADUX),NX,NY,NX,NY,2)                          0000000
C         PRINT 6774                                                     0000000
6774     FORMAT(' V WIND')                                              0000000
C         CALL DISP(PRM(1,IADVX),NX,NY,NX,NY,2)                          0000000
C                                                                       0000000
601   CONTINUE                                                          0000000
C                                                                       0000000
C     APPLY B.C.                                                        0000000
C                                                                       0000000
      CALL BC                                                           0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE DYN                                                    0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
      LOGICAL LTOP,                                                     0000000
     *        LBOT                                                      0000000
C                                                                       0000000
      DIMENSION SL     ( NZA ),                                         0000000
     *          DSIG   ( NZA ),                                         0000000
     *          SKAP   ( NZA )                                          0000000
C                                                                       0000000
      DIMENSION WRK1   ( LEN ),                                         0000000
     *          WRK2   ( LEN ),                                         0000000
     *          WRK3   ( LEN ),                                         0000000
     *          WRK4   ( LEN ),                                         0000000
     *          AMAP   ( LEN ),                                         0000000
     *          FDT    ( LEN ),                                         0000000
     *          F2DT2I ( LEN ),                                         0000000
     *          HOVM   ( LEN ),                                         0000000
     *          AMMINV ( LEN ),                                         0000000
     *          AMM    ( LEN ),                                         0000000
     *          AMH2   ( LEN ),                                         0000000
     *          AM2OVH ( LEN ),                                         0000000
     *          DHDT   ( LEN ),                                         0000000
     *          PI     ( LEN ),                                         0000000
     *          PIX    ( LEN ),                                         0000000
     *          PIY    ( LEN )                                          0000000
      DIMENSION SIGD   ( LEN ),                                         0000000
     *          SIGDB  ( LEN ),                                         0000000
     *          PGC    ( LEN ),                                         0000000
     *          THOLD  ( LEN ),                                         0000000
     *          GEOP   ( LEN ),                                         0000000
     *          UADV   ( LEN ),                                         0000000
     *          VADV   ( LEN ),                                         0000000
     *          TADV   ( LEN ),                                         0000000
     *          QADV   ( LEN ),                                         0000000
     *          AMMP   ( LEN ),                                         0000000
     *          AMMC   ( LEN ),                                         0000000
     *          AMHP   ( LEN ),                                         0000000
     *          AMHC   ( LEN )                                          0000000
C                                                                       0000000
      EQUIVALENCE ( SL     ( 1 ), STA ( 101   ) ),                      0000000
     *            ( DSIG   ( 1 ), STA ( 201   ) ),                      0000000
     *            ( SKAP   ( 1 ), STA ( 301   ) ),                      0000000
     *            ( SP0,          STA ( 8     ) ),                      0000000
     *            ( DELX,         STA ( 9     ) ),                      0000000
     *            ( DELT,         STA ( 13    ) ),                      0000000
     *            ( AITADJ,       STA ( 14    ) )                       0000000
C                                                                       0000000
      EQUIVALENCE ( WRK1   ( 1 ), WRK ( 1,  1 ) ),                      0000000
     *            ( WRK2   ( 1 ), WRK ( 1,  2 ) ),                      0000000
     *            ( WRK3   ( 1 ), WRK ( 1,  3 ) ),                      0000000
     *            ( WRK4   ( 1 ), WRK ( 1,  4 ) ),                      0000000
     *            ( AMAP   ( 1 ), WRK ( 1,  5 ) ),                      0000000
     *            ( FDT    ( 1 ), AMMP ( 1 ), WRK ( 1,  6 ) ),          0000000
     *            ( F2DT2I ( 1 ), AMMC ( 1 ), WRK ( 1,  7 ) ),          0000000
     *            ( HOVM   ( 1 ), AMHP ( 1 ), WRK ( 1,  8 ) ),          0000000
     *            ( AMMINV ( 1 ), AMHC ( 1 ), WRK ( 1,  9 ) ),          0000000
     *            ( AMM    ( 1 ), WRK ( 1, 10 ) ),                      0000000
     *            ( AMH2   ( 1 ), WRK ( 1, 11 ) ),                      0000000
     *            ( AM2OVH ( 1 ), WRK ( 1, 12 ) ),                      0000000
     *            ( DHDT   ( 1 ), WRK ( 1, 13 ) ),                      0000000
     *            ( PI     ( 1 ), WRK ( 1, 14 ) ),                      0000000
     *            ( PIX    ( 1 ), WRK ( 1, 15 ) ),                      0000000
     *            ( PIY    ( 1 ), WRK ( 1, 16 ) )                       0000000
      EQUIVALENCE ( SIGD   ( 1 ), WRK ( 1, 17 ) ),                      0000000
     *            ( SIGDB  ( 1 ), WRK ( 1, 18 ) ),                      0000000
     *            ( PGC    ( 1 ), WRK ( 1, 19 ) ),                      0000000
     *            ( THOLD  ( 1 ), WRK ( 1, 20 ) ),                      0000000
     *            ( GEOP   ( 1 ), WRK ( 1, 21 ) ),                      0000000
     *            ( UADV   ( 1 ), WRK ( 1, 22 ) ),                      0000000
     *            ( VADV   ( 1 ), WRK ( 1, 23 ) ),                      0000000
     *            ( TADV   ( 1 ), WRK ( 1, 24 ) ),                      0000000
     *            ( QADV   ( 1 ), WRK ( 1, 25 ) )                       0000000
C                                                                       0000000
C     SET UP LENGTHS USED LOCALLY                                       0000000
C                                                                       0000000
      PARAMETER ( LENHB  = LEN - 1 * NX - 1,                            0000000
     *            LENHT  = LEN - 2 * NX - 2,                            0000000
     *            LENMT  = LEN - 3 * NX - 3,                            0000000
     *            NXP1   = NX + 1,                                      0000000
     *            NXP2   = NX + 2          )                            0000000
C                                                                       0000000
C     SET UP CONSTANTS USED LOCALLY                                     0000000
C                                                                       0000000
      PARAMETER ( P5     = 0.5,                                         0000000
     *            P25    = 0.25,                                        0000000
     *            P609   = 0.609,                                       0000000
     *            TWOMEG = 2. * 7.29E-05,                               0000000
     *            CP     = 1004.64,                                     0000000
     *            ZERO   = 0.0,                                         0000000
     *            ONE    = 1.0,                                         0000000
     *            TWO    = 2.0 )                                        0000000
C                                                                       0000000
      SAVE                                                              0000000
C                                                                       0000000
C     SETUP INTEGER POINTERS TO PRM                                     0000000
C                                                                       0000000
      IADU   = IGADR ( 'U MOMENTUM' )                                   0000000
      IADV   = IGADR ( 'V MOMENTUM' )                                   0000000
      IADT   = IGADR ( 'POT TEMP  ' )                                   0000000
      IADQ   = IGADR ( 'SPEC HUMID' )                                   0000000
      IADH   = IGADR ( 'SURF PRES ' )                                   0000000
      IADTER = IGADR ( 'TERRAIN   ' )                                   0000000
      IADSP  = IGADR ( 'SINPHI    ' )                                   0000000
C                                                                       0000000
C     TIMESTEP AND GRID DISTANCE                                        0000000
C                                                                       0000000
      DT = DELT / AITADJ                                                0000000
      DX = DELX                                                         0000000
C                                                                       0000000
C     COMPUTE M * DT / DX AT ALL H POINTS, CALL THIS MAPCONST           0000000
C                                                                       0000000
      TEMP1 = ( DT / DX ) * ( ONE + SP0 )                               0000000
      DO 2000 I = 1, LEN                                                0000000
         AMAP ( I ) = TEMP1 / ( ONE + PRM ( I, IADSP ) )                0000000
2000  CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE MAPCONST WITH P5 AND INVERSE MAPCONST WITHOUT P25 FACTOR  0000000
C                                                                       0000000
      DO 2001 I = 1, LENHB + 1                                          0000000
         AMM ( I ) = AMAP ( I + NX ) + AMAP ( I )                       0000000
2001  CONTINUE                                                          0000000
      DO 2002 I = 1, LENHB                                              0000000
         TEMP1 = AMM ( I + 1 ) + AMM ( I )                              0000000
         AMM    ( I ) = TEMP1 * ( P25 * P5 )                            0000000
         AMMINV ( I ) = ONE / TEMP1                                     0000000
2002  CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE MAPCONST AT H POINTS WITH P5                              0000000
C                                                                       0000000
      DO 2003 I = 1, LENHT                                              0000000
         AMH2 ( I ) = AMAP ( I + NXP1 ) * AMAP ( I + NXP1 ) * P5        0000000
2003  CONTINUE                                                          0000000
C                                                                       0000000
C     CORIOLIS ACCELERATION TERMS                                       0000000
C                                                                       0000000
      DO 3001 I = 1, LENHB + 1                                          0000000
         WRK1 ( I ) = PRM ( I + NX, IADSP ) + PRM ( I, IADSP )          0000000
3001  CONTINUE                                                          0000000
      DO 3002 I = 1, LENHB                                              0000000
         TEMP1 = ( WRK1 ( I + 1 ) + WRK1 ( I ) ) * ( TWOMEG * P25 )     0000000
         FDT ( I )   = TEMP1 * DT                                       0000000
         F2DT2I ( I ) = ONE / ( ONE + TEMP1 * TEMP1 * ( DT * DT ) )     0000000
3002  CONTINUE                                                          0000000
C                                                                       0000000
C     SOLVE GRAVITY WAVE TERMS                                          0000000
C                                                                       0000000
      ITADJ = NINT ( AITADJ )                                           0000000
C                                                                       0000000
      DO 99999 IT = 1, ITADJ                                            0000000
C                                                                       0000000
C     COMPUTE AM2OVH, CONTAINS P5 FACTOR                                0000000
C                                                                       0000000
      DO 1 I = 1, LENHT                                                 0000000
         AM2OVH ( I ) = AMH2 ( I ) / PRM ( I + NXP1, IADH )             0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE HBAR / MAPFACTOR, P25 FACTORS FOR AVERAGE CANCEL          0000000
C                                                                       0000000
      DO 2 I = 1, LENHB + 1                                             0000000
         HOVM ( I ) = PRM ( I + NX, IADH  ) + PRM ( I, IADH  )          0000000
2     CONTINUE                                                          0000000
      DO 3 I = 1, LENHB                                                 0000000
         HOVM( I ) =  ( HOVM ( I + 1 ) + HOVM ( I ) ) * AMMINV ( I )    0000000
3     CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE VERTICAL SUM OF DELSIG WEIGHTED MOMENTUM                  0000000
C     DELSIG IS NEGATIVE                                                0000000
C                                                                       0000000
      IADUX = IADU                                                      0000000
      IADVX = IADV                                                      0000000
      TEMP1 = DSIG ( 1 )                                                0000000
      DO 4 I = 1, LENHB                                                 0000000
         WRK1 ( I ) = PRM ( I, IADUX ) * TEMP1                          0000000
         WRK2 ( I ) = PRM ( I, IADVX ) * TEMP1                          0000000
4     CONTINUE                                                          0000000
      DO 6 K = 2, NZA                                                   0000000
         IADUX = IADUX + 1                                              0000000
         IADVX = IADVX + 1                                              0000000
         TEMP1 = DSIG ( K )                                             0000000
         DO 5 I = 1, LENHB                                              0000000
            WRK1 ( I ) = WRK1 ( I ) + PRM ( I, IADUX ) * TEMP1          0000000
            WRK2 ( I ) = WRK2 ( I ) + PRM ( I, IADVX ) * TEMP1          0000000
5        CONTINUE                                                       0000000
6     CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE PRESSURE WEIGHTED VERTICAL SUM                            0000000
C                                                                       0000000
      DO 7 I = 1, LENHB                                                 0000000
         WRK1 ( I ) = WRK1 ( I ) * HOVM( I )                            0000000
         WRK2 ( I ) = WRK2 ( I ) * HOVM( I )                            0000000
7     CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE DHDT AND UPDATE H                                         0000000
C                                                                       0000000
      DO 9 I = 1, LENHT + 1                                             0000000
         WRK1 ( I ) = WRK1 ( I + NX ) + WRK1 ( I )                      0000000
         WRK2 ( I ) = WRK2 ( I + NX ) - WRK2 ( I )                      0000000
9     CONTINUE                                                          0000000
      DO 10 I = 1, LENHT                                                0000000
         TEMP1      = WRK1 ( I + 1 ) - WRK1 ( I ) +                     0000000
     *                WRK2 ( I + 1 ) + WRK2 ( I )                       0000000
         DHDT ( I ) = TEMP1                                             0000000
         PRM ( I + NXP1, IADH ) = PRM ( I + NXP1, IADH ) +              0000000
     *                            TEMP1 * AMH2 ( I )                    0000000
10    CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE PSTAR TO THE KAPPA, CALL IT PI                            0000000
C                                                                       0000000
      CALL P2KAP ( PRM ( NXP2, IADH ), PI ( 1 ), LENHT )                0000000
C                                                                       0000000
C     COMPUTE PIX AND PIY WITHOUT P5 FACTOR                             0000000
C     INITIALIZE SOME QUANTITIES TO ZERO                                0000000
C                                                                       0000000
      DO 15 I = 1, LENMT + 1                                            0000000
         PIX ( I ) = PI ( I + NX ) + PI ( I )                           0000000
         PIY ( I ) = PI ( I + NX ) - PI ( I )                           0000000
15    CONTINUE                                                          0000000
      DO 16 I = 1, LENMT                                                0000000
         PIX ( I )  = PIX ( I + 1 ) - PIX ( I )                         0000000
         PIY ( I )  = PIY ( I + 1 ) + PIY ( I )                         0000000
         UADV ( I ) = ZERO                                              0000000
         VADV ( I ) = ZERO                                              0000000
16    CONTINUE                                                          0000000
      DO 17 I = 1, LENHT                                                0000000
         SIGD ( I ) = ZERO                                              0000000
         TADV ( I ) = ZERO                                              0000000
         QADV ( I ) = ZERO                                              0000000
17    CONTINUE                                                          0000000
C                                                                       0000000
C     INITIALIZE POINTERS TO U, V, TH, Q                                0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      IADQX = IADQ - 1                                                  0000000
C                                                                       0000000
      DO 99998 K = 1, NZA                                               0000000
C                                                                       0000000
      IADUX = IADUX + 1                                                 0000000
      IADVX = IADVX + 1                                                 0000000
      IADTX = IADTX + 1                                                 0000000
      IADQX = IADQX + 1                                                 0000000
C                                                                       0000000
C     SETUP LOGICALS FOR WHICH LAYER WE ARE AT                          0000000
C                                                                       0000000
      IF ( K .EQ. 1 ) THEN                                              0000000
         LBOT = .TRUE.                                                  0000000
      ELSE                                                              0000000
         LBOT = .FALSE.                                                 0000000
      END IF                                                            0000000
      IF ( K .EQ. NZA ) THEN                                            0000000
         LTOP = .TRUE.                                                  0000000
      ELSE                                                              0000000
         LTOP = .FALSE.                                                 0000000
      END IF                                                            0000000
C                                                                       0000000
      IF ( .NOT. LTOP ) THEN                                            0000000
C                                                                       0000000
C     COMPUTE SIGMA DOT AT H POINTS                                     0000000
C                                                                       0000000
         DO 19 I = 1, LENHB                                             0000000
            WRK1 ( I ) = HOVM ( I ) * PRM ( I, IADUX )                  0000000
            WRK2 ( I ) = HOVM ( I ) * PRM ( I, IADVX )                  0000000
19       CONTINUE                                                       0000000
         DO 21 I = 1, LENHT + 1                                         0000000
            WRK1 ( I ) = WRK1 ( I + NX ) + WRK1 ( I )                   0000000
            WRK2 ( I ) = WRK2 ( I + NX ) - WRK2 ( I )                   0000000
21       CONTINUE                                                       0000000
         TEMP1 = DSIG ( K )                                             0000000
         DO 23 I = 1, LENHT                                             0000000
            SIGD ( I ) = SIGD ( I ) - TEMP1 * AM2OVH ( I ) *            0000000
     *                   ( WRK1 ( I + 1 ) - WRK1 ( I ) +                0000000
     *                     WRK2 ( I + 1 ) + WRK2 ( I ) +                0000000
     *                     DHDT ( I ) )                                 0000000
23       CONTINUE                                                       0000000
C                                                                       0000000
C        COMPUTE SIGMA DOT AT MOMENTUM POINTS WITHOUT P25 FACTOR        0000000
C                                                                       0000000
         DO 24 I = 1, LENMT + 1                                         0000000
            SIGDB ( I ) = SIGD ( I + NX ) + SIGD ( I )                  0000000
24       CONTINUE                                                       0000000
         DO 25 I = 1, LENMT                                             0000000
            SIGDB ( I ) = SIGDB ( I + 1 ) + SIGDB ( I )                 0000000
25       CONTINUE                                                       0000000
C                                                                       0000000
C        COMPUTE DSIG FOR VERTICAL ADVECTION                            0000000
C                                                                       0000000
         DSIGL = P5 / ( SL ( K ) - SL ( K + 1 ) )                       0000000
C                                                                       0000000
C        SOLVE THERMODYNAMIC AND MOISTURE CONSERVATION EQUATION         0000000
C                                                                       0000000
         DO 26 I = 1, LENHT                                             0000000
            TEMP1 = DSIGL * SIGD ( I )                                  0000000
            TEMP2 = TEMP1 *                                             0000000
     *      ( PRM ( I + NXP1, IADTX + 1 ) - PRM ( I + NXP1, IADTX ) )   0000000
            TEMP3 = TEMP1 *                                             0000000
     *      ( PRM ( I + NXP1, IADQX + 1 ) - PRM ( I + NXP1, IADQX ) )   0000000
            PRM ( I + NXP1, IADTX ) = PRM ( I + NXP1, IADTX ) +         0000000
     *      TEMP2 + TADV ( I )                                          0000000
            PRM ( I + NXP1, IADQX ) = PRM ( I + NXP1, IADQX ) +         0000000
     *      TEMP3 + QADV ( I )                                          0000000
            TADV ( I ) = TEMP2                                          0000000
            QADV ( I ) = TEMP3                                          0000000
26       CONTINUE                                                       0000000
C                                                                       0000000
C        SPECIAL CASE FOR TOP LAYER                                     0000000
C                                                                       0000000
      ELSE                                                              0000000
C                                                                       0000000
         DO 29 I = 1, LENHT                                             0000000
            PRM ( I + NXP1, IADTX ) = PRM ( I + NXP1, IADTX ) +         0000000
     *      TADV ( I )                                                  0000000
            PRM ( I + NXP1, IADQX ) = PRM ( I + NXP1, IADQX ) +         0000000
     *      QADV ( I )                                                  0000000
29       CONTINUE                                                       0000000
C                                                                       0000000
      END IF                                                            0000000
C                                                                       0000000
C                 SOLVE MOMENTUM EQUATIONS                              0000000
C                                                                       0000000
C     COMPUTE VIRTUAL THETA, HOLD IN WRK1 FOR A LITTLE WHILE            0000000
C                                                                       0000000
      DO 31 I = 1, LENHT                                                0000000
         WRK1 ( I ) = ( ONE + P609 * PRM ( I + NXP1, IADQX ) ) *        0000000
     *                PRM ( I + NXP1, IADTX )                           0000000
31    CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE GEOPOTENTIAL                                              0000000
C                                                                       0000000
C     SPECIAL CASE FOR BOTTOM LAYER                                     0000000
C                                                                       0000000
      IF ( LBOT ) THEN                                                  0000000
         TEMP1 = ( ONE - SKAP ( 1 ) ) * CP                              0000000
         DO 32 I = 1, LENHT                                             0000000
            GEOP ( I ) = PRM ( I + NXP1,  IADTER ) +                    0000000
     *                   TEMP1 * WRK1 ( I ) * PI ( I )                  0000000
32       CONTINUE                                                       0000000
      ELSE                                                              0000000
         TEMP1 = ( SKAP ( K - 1 ) - SKAP ( K ) ) * ( CP * P5 )          0000000
         DO 33 I = 1, LENHT                                             0000000
            GEOP ( I ) = GEOP ( I ) + TEMP1 *                           0000000
     *                   ( WRK1 ( I ) + THOLD ( I ) ) * PI ( I )        0000000
33       CONTINUE                                                       0000000
      END IF                                                            0000000
C                                                                       0000000
C     SAVE VIRTUAL TEMPERATURE FOR NEXT LAYER                           0000000
C                                                                       0000000
      IF ( .NOT. LTOP ) THEN                                            0000000
         DO 333 I = 1, LENHT                                            0000000
            THOLD ( I ) = WRK1 ( I )                                    0000000
333      CONTINUE                                                       0000000
      END IF                                                            0000000
C                                                                       0000000
C     COMPUTE CP * THETAV * SKAP AT MOMENTUM POINTS COMPLETE            0000000
C                                                                       0000000
      DO 34 I = 1, LENMT + 1                                            0000000
         PGC ( I ) = WRK1 ( I ) + WRK1 ( I + NX )                       0000000
34    CONTINUE                                                          0000000
      TEMP1 = ( P25 * CP ) * SKAP ( K )                                 0000000
      DO 35 I = 1, LENMT                                                0000000
         PGC ( I ) = TEMP1 * ( PGC ( I ) + PGC ( I + 1 ) )              0000000
35    CONTINUE                                                          0000000
C                                                                       0000000
C     COMPUTE VERTICAL ADVECTION WITH NEGATIVE BUILT IN                 0000000
C                                                                       0000000
      IF ( .NOT. LTOP ) THEN                                            0000000
C                                                                       0000000
         DSIGL = DSIGL * P25                                            0000000
C                                                                       0000000
         DO 81 I = 1, LENMT                                             0000000
            TEMP1 = DSIGL * SIGDB ( I )                                 0000000
            TEMP2 = TEMP1 *                                             0000000
     *      ( PRM ( I + NXP1, IADUX + 1 ) - PRM ( I + NXP1, IADUX ) )   0000000
            TEMP3 = TEMP1 *                                             0000000
     *      ( PRM ( I + NXP1, IADVX + 1 ) - PRM ( I + NXP1, IADVX ) )   0000000
            WRK3 ( I ) = TEMP2 + UADV ( I )                             0000000
            WRK4 ( I ) = TEMP3 + VADV ( I )                             0000000
            UADV ( I ) = TEMP2                                          0000000
            VADV ( I ) = TEMP3                                          0000000
81    CONTINUE                                                          0000000
C                                                                       0000000
C        SPECIAL CASE FOR TOP LAYER                                     0000000
C                                                                       0000000
      ELSE                                                              0000000
C                                                                       0000000
         DO 84 I = 1, LENMT                                             0000000
            WRK3 ( I ) = UADV ( I )                                     0000000
            WRK4 ( I ) = VADV ( I )                                     0000000
84       CONTINUE                                                       0000000
C                                                                       0000000
      END IF                                                            0000000
C                                                                       0000000
C     UPDATE MOMENTUM                                                   0000000
C                                                                       0000000
      DO 37 I = 1, LENMT + 1                                            0000000
         WRK1 ( I ) = GEOP ( I ) + GEOP ( I + NX )                      0000000
         WRK2 ( I ) = GEOP ( I ) - GEOP ( I + NX )                      0000000
37    CONTINUE                                                          0000000
      DO 38 I = 1, LENMT                                                0000000
         TEMP1 = ( WRK1 ( I ) - WRK1 ( I + 1 ) - PGC ( I ) *            0000000
     *   PIX ( I ) ) * AMM ( I + NXP1 ) + WRK3 ( I )                    0000000
         TEMP2 = ( WRK2 ( I + 1 ) + WRK2 ( I ) - PGC ( I ) *            0000000
     *   PIY ( I ) ) * AMM ( I + NXP1 ) + WRK4 ( I )                    0000000
         TEMP3 = PRM ( I + NXP1, IADUX )                                0000000
         TEMP4 = PRM ( I + NXP1, IADVX )                                0000000
         PRM ( I + NXP1, IADUX ) = F2DT2I ( I + NXP1 ) *                0000000
     *   ( TEMP3 + TEMP1 + FDT ( I + NXP1 ) * ( TEMP4 + TEMP2 ) )       0000000
         PRM ( I + NXP1, IADVX ) = F2DT2I ( I + NXP1 ) *                0000000
     *   ( TEMP4 + TEMP2 - FDT ( I + NXP1 ) * ( TEMP3 + TEMP1 ) )       0000000
38    CONTINUE                                                          0000000
C                                                                       0000000
99998 CONTINUE                                                          0000000
C                                                                       0000000
C     APPLY BOUNDARY CONDITIONS                                         0000000
C                                                                       0000000
      CALL BC                                                           0000000
C                                                                       0000000
99999 CONTINUE                                                          0000000
C                                                                       0000000
C     REDEFINE MAPSCONST FOR LAX-WENDROFF ADVECTION                     0000000
C                                                                       0000000
C     P5 FOR HALF DT                                                    0000000
      DO 302 I = 1, LENHB                                               0000000
         AMMP ( I ) = AMM ( I ) * ( P5 * AITADJ )                       0000000
302   CONTINUE                                                          0000000
      DO 303 I = 1, LENMT                                               0000000
         AMMC ( I ) = AMM ( I + NXP1 ) * ( AITADJ )                     0000000
303   CONTINUE                                                          0000000
C     P5 FOR HALF DT, P5 FOR AVERAGE OF GRAD                            0000000
      DO 304 I = 1, LENHT                                               0000000
         AMHP ( I ) = AMAP ( I + NXP1 ) * ( P25 * AITADJ )              0000000
         AMHC ( I ) = AMAP ( I + NXP1 ) * ( P5  * AITADJ )              0000000
304   CONTINUE                                                          0000000
C                                                                       0000000
C     ADVECTION SECTION                                                 0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      IADQX = IADQ - 1                                                  0000000
C                                                                       0000000
      DO 99997 K = 1, NZA                                               0000000
C                                                                       0000000
      IADUX = IADUX + 1                                                 0000000
      IADVX = IADVX + 1                                                 0000000
      IADTX = IADTX + 1                                                 0000000
      IADQX = IADQX + 1                                                 0000000
C                                                                       0000000
C     THETA AND Q PREDICTION                                            0000000
C                                                                       0000000
      DO 101 I = 1, LENHB + 1                                           0000000
         WRK1 ( I ) = PRM ( I + NX, IADTX ) - PRM ( I, IADTX )          0000000
         WRK2 ( I ) = PRM ( I + NX, IADQX ) - PRM ( I, IADQX )          0000000
         TADV ( I ) = PRM ( I + NX, IADTX ) + PRM ( I, IADTX )          0000000
         QADV ( I ) = PRM ( I + NX, IADQX ) + PRM ( I, IADQX )          0000000
101   CONTINUE                                                          0000000
      DO 102 I = 1, LENHB                                               0000000
         TADV ( I ) = ( TADV ( I + 1 ) + TADV ( I ) ) * P25 -           0000000
     *                ( ( TADV ( I + 1 ) - TADV ( I ) ) *               0000000
     *                PRM ( I, IADUX ) +                                0000000
     *                ( WRK1 ( I + 1 ) + WRK1 ( I ) ) *                 0000000
     *                PRM ( I, IADVX ) ) * AMMP ( I )                   0000000
         QADV ( I ) = ( QADV ( I + 1 ) + QADV ( I ) ) * P25 -           0000000
     *                ( ( QADV ( I + 1 ) - QADV ( I ) ) *               0000000
     *                PRM ( I, IADUX ) +                                0000000
     *                ( WRK2 ( I + 1 ) + WRK2 ( I ) ) *                 0000000
     *                PRM ( I, IADVX ) ) * AMMP ( I )                   0000000
102   CONTINUE                                                          0000000
C                                                                       0000000
C     U AND V PREDICTION                                                0000000
C                                                                       0000000
      DO 103 I = 1, LENHT + 1                                           0000000
         WRK1 ( I ) = PRM ( I + NX, IADUX ) - PRM ( I, IADUX )          0000000
         WRK2 ( I ) = PRM ( I + NX, IADVX ) - PRM ( I, IADVX )          0000000
         UADV ( I ) = PRM ( I + NX, IADUX ) + PRM ( I, IADUX )          0000000
         VADV ( I ) = PRM ( I + NX, IADVX ) + PRM ( I, IADVX )          0000000
103   CONTINUE                                                          0000000
      DO 104 I = 1, LENHT                                               0000000
         TEMP1 = ( UADV ( I + 1 ) + UADV ( I ) ) * P25                  0000000
         TEMP2 = ( VADV ( I + 1 ) + VADV ( I ) ) * P25                  0000000
         UADV ( I ) = TEMP1 -                                           0000000
     *   ( TEMP1 * ( UADV ( I + 1 ) - UADV ( I ) ) +                    0000000
     *     TEMP2 * ( WRK1 ( I + 1 ) + WRK1 ( I ) ) ) *                  0000000
     *     AMHP ( I )                                                   0000000
         VADV ( I ) = TEMP2 -                                           0000000
     *   ( TEMP1 * ( VADV ( I + 1 ) - VADV ( I ) ) +                    0000000
     *     TEMP2 * ( WRK2 ( I + 1 ) + WRK2 ( I ) ) ) *                  0000000
     *     AMHP ( I )                                                   0000000
104   CONTINUE                                                          0000000
C                                                                       0000000
C     THETA AND Q CORRECTION                                            0000000
C                                                                       0000000
      DO 105 I = 1, LENHT + 1                                           0000000
         WRK1 ( I ) = TADV ( I + NX ) - TADV ( I )                      0000000
         WRK2 ( I ) = QADV ( I + NX ) - QADV ( I )                      0000000
         WRK3 ( I ) = TADV ( I + NX ) + TADV ( I )                      0000000
         WRK4 ( I ) = QADV ( I + NX ) + QADV ( I )                      0000000
105   CONTINUE                                                          0000000
      DO 106 I = 1, LENHT                                               0000000
         PRM ( I + NXP1, IADTX ) = PRM ( I + NXP1, IADTX ) -            0000000
     *                ( UADV ( I ) * ( WRK3 ( I + 1 ) - WRK3 ( I ) ) +  0000000
     *                  VADV ( I ) * ( WRK1 ( I + 1 ) + WRK1 ( I ) ) ) *0000000
     *                  AMHC ( I )                                      0000000
         PRM ( I + NXP1, IADQX ) = PRM ( I + NXP1, IADQX ) -            0000000
     *                ( UADV ( I ) * ( WRK4 ( I + 1 ) - WRK4 ( I ) ) +  0000000
     *                  VADV ( I ) * ( WRK2 ( I + 1 ) + WRK2 ( I ) ) ) *0000000
     *                  AMHC ( I )                                      0000000
106   CONTINUE                                                          0000000
C                                                                       0000000
C     U AND V CORRECTION                                                0000000
C                                                                       0000000
      DO 107 I = 1, LENMT + 1                                           0000000
         WRK1 ( I ) = UADV ( I + NX ) - UADV ( I )                      0000000
         WRK2 ( I ) = VADV ( I + NX ) - VADV ( I )                      0000000
         WRK3 ( I ) = UADV ( I + NX ) + UADV ( I )                      0000000
         WRK4 ( I ) = VADV ( I + NX ) + VADV ( I )                      0000000
107   CONTINUE                                                          0000000
      DO 108 I = 1, LENMT                                               0000000
         TEMP1 = ( WRK3 ( I + 1 ) + WRK3 ( I ) ) * P25                  0000000
         TEMP2 = ( WRK4 ( I + 1 ) + WRK4 ( I ) ) * P25                  0000000
         PRM ( I + NXP1, IADUX ) = PRM ( I + NXP1, IADUX ) -            0000000
     *                ( TEMP1 * ( WRK3 ( I + 1 ) - WRK3 ( I ) ) +       0000000
     *                  TEMP2 * ( WRK1 ( I + 1 ) + WRK1 ( I ) ) ) *     0000000
     *                  AMMC ( I )                                      0000000
         PRM ( I + NXP1, IADVX ) = PRM ( I + NXP1, IADVX ) -            0000000
     *                ( TEMP1 * ( WRK4 ( I + 1 ) - WRK4 ( I ) ) +       0000000
     *                  TEMP2 * ( WRK2 ( I + 1 ) + WRK2 ( I ) ) ) *     0000000
     *                  AMMC ( I )                                      0000000
108   CONTINUE                                                          0000000
C                                                                       0000000
C                                                                       0000000
99997 CONTINUE                                                          0000000
C                                                                       0000000
C     APPLY BOUNDARY CONDITIONS                                         0000000
C                                                                       0000000
      CALL BC                                                           0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE BC                                                     0000000
      CALL BCLON                                                        0000000
CCC   CALL BCLAT                                                        0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE BCLON                                                  0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
      PARAMETER ( INUMM = 2 * NZA,                                      0000000
     *            INUMH = 2 * NZA + 1 )                                 0000000
C                                                                       0000000
      DIMENSION IND1H ( NY ),                                           0000000
     *          IND2H ( NY ),                                           0000000
     *          IND3H ( NY ),                                           0000000
     *          IND4H ( NY ),                                           0000000
     *          IND1M ( NY ),                                           0000000
     *          IND2M ( NY ),                                           0000000
     *          IND3M ( NY ),                                           0000000
     *          IND4M ( NY )                                            0000000
C                                                                       0000000
      DIMENSION AG1 ( NY ),                                             0000000
     *          AG2 ( NY )                                              0000000
C                                                                       0000000
      LOGICAL LFRST                                                     0000000
C                                                                       0000000
      SAVE                                                              0000000
C                                                                       0000000
      DATA LFRST / .TRUE. /                                             0000000
C                                                                       0000000
C     SETUP GATHER/SCATTER INDICES                                      0000000
C                                                                       0000000
      IF ( LFRST ) THEN                                                 0000000
C                                                                       0000000
      IND1M ( 1 ) = 1                                                   0000000
      IND2M ( 1 ) = 2                                                   0000000
      IND3M ( 1 ) = NX - 2                                              0000000
      IND4M ( 1 ) = NX - 1                                              0000000
      IND1H ( 1 ) = 1                                                   0000000
      IND2H ( 1 ) = 3                                                   0000000
      IND3H ( 1 ) = NX - 2                                              0000000
      IND4H ( 1 ) = NX                                                  0000000
      DO 1 I = 2, NY                                                    0000000
         IND1M ( I ) = IND1M ( I - 1 ) + NX                             0000000
         IND2M ( I ) = IND2M ( I - 1 ) + NX                             0000000
         IND3M ( I ) = IND3M ( I - 1 ) + NX                             0000000
         IND4M ( I ) = IND4M ( I - 1 ) + NX                             0000000
         IND1H ( I ) = IND1H ( I - 1 ) + NX                             0000000
         IND2H ( I ) = IND2H ( I - 1 ) + NX                             0000000
         IND3H ( I ) = IND3H ( I - 1 ) + NX                             0000000
         IND4H ( I ) = IND4H ( I - 1 ) + NX                             0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
      LFRST = .FALSE.                                                   0000000
C                                                                       0000000
      END IF                                                            0000000
C                                                                       0000000
C     APPLY CYCLIC BOUNDARY CONDITIONS FOR M POINTS                     0000000
C                                                                       0000000
      IADM = IGADR ( 'U MOMENTUM' ) - 1                                 0000000
                                                                        0000000
      DO 104 K = 1, INUMM                                               0000000
         IADM = IADM + 1                                                0000000
         DO 101 I = 1, NY                                               0000000
            AG1 ( I ) = PRM ( IND2M ( I ), IADM )                       0000000
            AG2 ( I ) = PRM ( IND3M ( I ), IADM )                       0000000
101      CONTINUE                                                       0000000
         DO 102 I = 1, NY                                               0000000
            PRM ( IND4M ( I ), IADM ) = AG1 ( I )                       0000000
102      CONTINUE                                                       0000000
         DO 103 I = 1, NY                                               0000000
            PRM ( IND1M ( I ), IADM ) = AG2 ( I )                       0000000
103      CONTINUE                                                       0000000
104   CONTINUE                                                          0000000
C                                                                       0000000
C     APPLY CYCLIC BOUNDARY CONDITIONS FOR H POINTS                     0000000
C                                                                       0000000
      IADH = IGADR ( 'POT TEMP  ' ) - 1                                 0000000
C                                                                       0000000
      DO 110 K = 1, INUMH                                               0000000
         IADH = IADH + 1                                                0000000
         DO 105 I = 1, NY                                               0000000
            AG1 ( I ) = PRM ( IND2H ( I ), IADH )                       0000000
            AG2 ( I ) = PRM ( IND3H ( I ), IADH )                       0000000
105      CONTINUE                                                       0000000
         DO 106 I = 1, NY                                               0000000
            PRM ( IND4H ( I ), IADH ) = AG1 ( I )                       0000000
106      CONTINUE                                                       0000000
         DO 107 I = 1, NY                                               0000000
            PRM ( IND1H ( I ), IADH ) = AG2 ( I )                       0000000
107      CONTINUE                                                       0000000
110   CONTINUE                                                          0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE BCLAT                                                  0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
      IADU = IGADR ( 'U MOMENTUM' )                                     0000000
      IADV = IGADR ( 'V MOMENTUM' )                                     0000000
      IADT = IGADR ( 'POT TEMP  ' )                                     0000000
      IADQ = IGADR ( 'SPEC HUMID' )                                     0000000
      IADH = IGADR ( 'SURF PRES ' )                                     0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      IADQX = IADQ - 1                                                  0000000
C                                                                       0000000
      DO 1 K = 1, NZA                                                   0000000
C                                                                       0000000
         IADUX = IADUX + 1                                              0000000
         IADVX = IADVX + 1                                              0000000
         IADTX = IADTX + 1                                              0000000
         IADQX = IADQX + 1                                              0000000
C                                                                       0000000
         DO 2 I = 1, NX                                                 0000000
C                                                                       0000000
C           U AND V LOWER BOUNDARY                                      0000000
C                                                                       0000000
CCC         PRM ( I, IADUX ) =   PRM ( I + NX, IADUX )                  0000000
C                                                                       0000000
CCC         PRM ( I, IADVX ) = - PRM ( I + NX, IADVX )                  0000000
CCC         PRM ( I, IADVX ) =   PRM ( I + NX, IADVX )                  0000000
C                                                                       0000000
C           T AND Q LOWER BOUNDARY                                      0000000
C                                                                       0000000
CCC         PRM ( I, IADTX ) = 2. * PRM( I + NX, IADTX ) -              0000000
CCC  *          PRM ( I + 2*NX, IADTX )                                 0000000
CCC         PRM ( I, IADQX ) = 2. * PRM( I + NX, IADQX ) -              0000000
CCC  *          PRM ( I + 2*NX, IADQX )                                 0000000
C                                                                       0000000
CCC         PRM ( I, IADTX ) =  PRM( I + 2*NX, IADTX )                  0000000
CCC         PRM ( I, IADQX ) =  PRM( I + 2*NX, IADQX )                  0000000
C                                                                       0000000
            PRM ( I, IADTX ) =  PRM( I + 1*NX, IADTX )                  0000000
            PRM ( I, IADQX ) =  PRM( I + 1*NX, IADQX )                  0000000
C                                                                       0000000
C           U AND V UPPER BOUNDARY                                      0000000
C                                                                       0000000
CCC         PRM ( I + (NY-2)*NX, IADUX ) =                              0000000
CCC  *            PRM ( I + (NY-3)*NX, IADUX )                          0000000
C                                                                       0000000
CCC         PRM ( I + (NY-2)*NX, IADVX ) =                              0000000
CCC  *          - PRM ( I + (NY-3)*NX, IADVX )                          0000000
CCC         PRM ( I + (NY-2)*NX, IADVX ) =                              0000000
CCC  *            PRM ( I + (NY-3)*NX, IADVX )                          0000000
C                                                                       0000000
C           T AND Q UPPER BOUNDARY                                      0000000
C                                                                       0000000
CCC         PRM ( I + (NY-1)*NX, IADTX ) =                              0000000
CCC  *              2. * PRM ( I + (NY-2)*NX, IADTX ) -                 0000000
CCC  *              PRM ( I + (NY-3)*NX, IADTX )                        0000000
CCC         PRM ( I + (NY-1)*NX, IADQX ) =                              0000000
CCC  *              2. * PRM ( I + (NY-2)*NX, IADQX ) -                 0000000
CCC  *              PRM ( I + (NY-3)*NX, IADQX )                        0000000
C                                                                       0000000
CCC         PRM ( I + (NY-1)*NX, IADTX ) =                              0000000
CCC  *              PRM ( I + (NY-3)*NX, IADTX )                        0000000
CCC         PRM ( I + (NY-1)*NX, IADQX ) =                              0000000
CCC  *              PRM ( I + (NY-3)*NX, IADQX )                        0000000
C                                                                       0000000
            PRM ( I + (NY-1)*NX, IADTX ) =                              0000000
     *              PRM ( I + (NY-2)*NX, IADTX )                        0000000
            PRM ( I + (NY-1)*NX, IADQX ) =                              0000000
     *              PRM ( I + (NY-2)*NX, IADQX )                        0000000
C                                                                       0000000
2        CONTINUE                                                       0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
C                                                                       0000000
      DO 3 I = 1, NX                                                    0000000
C                                                                       0000000
C        H LOWER BOUNDARY                                               0000000
C                                                                       0000000
CCC      PRM ( I, IADH ) = 2. * PRM( I + NX, IADH ) -                   0000000
CCC  *          PRM ( I + 2*NX, IADH )                                  0000000
C                                                                       0000000
CCC      PRM ( I, IADH ) =  PRM( I + 2*NX, IADH )                       0000000
C                                                                       0000000
         PRM ( I, IADH ) =  PRM( I + 1*NX, IADH )                       0000000
C                                                                       0000000
C        H UPPER BOUNDARY                                               0000000
C                                                                       0000000
CCC      PRM ( I + (NY-1)*NX, IADH ) =                                  0000000
CCC  *          2. * PRM ( I + (NY-2)*NX, IADH ) -                      0000000
CCC  *          PRM ( I + (NY-3)*NX, IADH )                             0000000
C                                                                       0000000
CCC      PRM ( I + (NY-1)*NX, IADH ) = PRM ( I + (NY-3)*NX, IADH )      0000000
C                                                                       0000000
         PRM ( I + (NY-1)*NX, IADH ) = PRM ( I + (NY-2)*NX, IADH )      0000000
C                                                                       0000000
3     CONTINUE                                                          0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE SM                                                     0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
      IADU = IGADR ( 'U MOMENTUM' )                                     0000000
      IADV = IGADR ( 'V MOMENTUM' )                                     0000000
      IADT = IGADR ( 'POT TEMP  ' )                                     0000000
      IADQ = IGADR ( 'SPEC HUMID' )                                     0000000
      IADH = IGADR ( 'SURF PRES ' )                                     0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      IADQX = IADQ - 1                                                  0000000
C                                                                       0000000
      DO 1 K = 1, NZA                                                   0000000
         IADUX = IADUX + 1                                              0000000
         IADVX = IADVX + 1                                              0000000
         IADTX = IADTX + 1                                              0000000
         IADQX = IADQX + 1                                              0000000
         CALL SMOOTH(PRM(1,IADTX),WRK(1,1),NX,NY,1)                     0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
      CALL BC                                                           0000000
C                                                                       0000000
      IADUX = IADU - 1                                                  0000000
      IADVX = IADV - 1                                                  0000000
      IADTX = IADT - 1                                                  0000000
      IADQX = IADQ - 1                                                  0000000
C                                                                       0000000
      DO 2 K = 1, NZA                                                   0000000
         IADUX = IADUX + 1                                              0000000
         IADVX = IADVX + 1                                              0000000
         IADTX = IADTX + 1                                              0000000
         IADQX = IADQX + 1                                              0000000
         CALL SMOOTH(PRM(1,IADTX),WRK(1,1),NX,NY,2)                     0000000
2     CONTINUE                                                          0000000
C                                                                       0000000
      CALL BC                                                           0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      FUNCTION IGADR ( NAME )                                           0000000
C                                                                       0000000
      PARAMETER ( NX     = 30,                                          0000000
     *            NY     = 31,                                          0000000
     *            NZA    = 10,                                          0000000
     *            NZG    = 2,                                           0000000
     *            NZANUM = 4,                                           0000000
     *            NZGNUM = 0,                                           0000000
     *            NZONE  = 3,                                           0000000
     *            LENSTA = 1000,                                        0000000
     *            LVLWRK = 25,                                          0000000
     *            LEN    = NX * NY,                                     0000000
     *            LVLPRM = NZANUM * NZA + NZGNUM * NZG + NZONE,         0000000
     *            LENIAD = NZANUM       + NZGNUM       + NZONE)         0000000
C                                                                       0000000
      COMMON/B1/PRM ( LEN, LVLPRM ), STA ( LENSTA )                     0000000
C                                                                       0000000
      COMMON/B2/WRK ( LEN, LVLWRK )                                     0000000
C                                                                       0000000
      CHARACTER*10 NAME,                                                0000000
     *             TYPES ( LENIAD )                                     0000000
C                                                                       0000000
      DIMENSION IAD ( LENIAD )                                          0000000
C                                                                       0000000
      LOGICAL LFRST                                                     0000000
C                                                                       0000000
      SAVE                                                              0000000
C                                                                       0000000
      DATA LFRST / .TRUE./                                              0000000
C                                                                       0000000
      DATA TYPES / 'U MOMENTUM',                                        0000000
     *             'V MOMENTUM',                                        0000000
     *             'POT TEMP  ',                                        0000000
     *             'SPEC HUMID',                                        0000000
     *             'SURF PRES ',                                        0000000
     *             'TERRAIN   ',                                        0000000
     *             'SINPHI    '/                                        0000000
C                                                                       0000000
      IF ( LFRST ) THEN                                                 0000000
C                                                                       0000000
         LFRST = .FALSE.                                                0000000
C                                                                       0000000
         IAD ( 1 ) = 1                                                  0000000
         IAD ( 2 ) = IAD ( 1 ) + NZA                                    0000000
         IAD ( 3 ) = IAD ( 2 ) + NZA                                    0000000
         IAD ( 4 ) = IAD ( 3 ) + NZA                                    0000000
         IAD ( 5 ) = IAD ( 4 ) + NZA                                    0000000
         IAD ( 6 ) = IAD ( 5 ) + 1                                      0000000
         IAD ( 7 ) = IAD ( 6 ) + 1                                      0000000
C                                                                       0000000
      END IF                                                            0000000
C                                                                       0000000
      DO 1 I = 1, LENIAD                                                0000000
         IF ( NAME .EQ. TYPES ( I ) ) THEN                              0000000
            IGADR = IAD ( I )                                           0000000
            RETURN                                                      0000000
         END IF                                                         0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
      PRINT 100, NAME                                                   0000000
100   FORMAT(' *** UNKNOWN VARIABLE IN IGADR = ',A10)                   0000000
C                                                                       0000000
      STOP                                                              0000000
      END                                                               0000000
      SUBROUTINE P2KAP( P, PI, N )                                      0000000
C                                                                       0000000
C     FOR INPUT IN THE RANGE 0.5-1.1, KAPPA=0.2856219                   0000000
C                                                                       0000000
      DIMENSION P  ( N ),                                               0000000
     *          PI ( N )                                                0000000
C                                                                       0000000
      DATA        COFD1 /  1. /,                                        0000000
     *            COFD2 /  5.44053037 /,                                0000000
     *            COFD3 /  2.27693825 /,                                0000000
     *            COFD4 / -0.0869930591 /                               0000000
C                                                                       0000000
      DATA        COFN1 /  0.34757549 /,                                0000000
     *            COFN2 /  4.36732956 /,                                0000000
     *            COFN3 /  3.91557032 /                                 0000000
C                                                                       0000000
      DO 1 I = 1, N                                                     0000000
         PL = P ( I )                                                   0000000
         PI ( I ) =                                                     0000000
     *       ( COFN1 + PL * ( COFN2 + PL * COFN3 ) ) /                  0000000
     *       ( COFD1 + PL * ( COFD2 + PL * ( COFD3 + PL * COFD4 ) ) )   0000000
1     CONTINUE                                                          0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
      SUBROUTINE SMOOTH(FIELDS,TEMP,ICOL,JROW,ISW)                      0000000
C                                                                       0000000
      DIMENSION FIELDS(ICOL,JROW),TEMP(ICOL,JROW)                       0000000
C                                                                       0000000
      JROWM1=JROW-1                                                     0000000
      ICOLM1=ICOL-1                                                     0000000
C                                                                       0000000
      IF ( ISW .EQ. 1 ) THEN                                            0000000
         S = 0.5                                                        0000000
      ELSE                                                              0000000
         S = -0.5                                                       0000000
      END IF                                                            0000000
C                                                                       0000000
      ACOEF=(1.-S)*(1.-S)                                               0000000
      CCOEF=S*S                                                         0000000
      BCOEF=S-CCOEF                                                     0000000
      BCOEF=BCOEF*0.5                                                   0000000
      CCOEF=CCOEF*0.25  
C                                                                        0000000
      DO 761 J=2,JROWM1                                                 0000000
      DO 761 I=2,ICOLM1                                                 0000000
      TEMP(I,J)=ACOEF*FIELDS(I,J)+BCOEF*(FIELDS(I+1,J)+FIELDS(I-1,J)+   0000000
     *FIELDS(I,J+1)+FIELDS(I,J-1))+CCOEF*(FIELDS(I+1,J+1)+FIELDS(I+1,   0000000
     *J-1)+FIELDS(I-1,J-1)+FIELDS(I-1,J+1))                             0000000
761   CONTINUE                                                          0000000
      DO 763 J=2,JROWM1                                                 0000000
      DO 763 I=2,ICOLM1                                                 0000000
763   FIELDS(I,J)=TEMP(I,J)                                             0000000
C                                                                       0000000
C                                                                       0000000
      RETURN                                                            0000000
      END                                                               0000000
