      PROGRAM CSHELF                                                    MOD00010
C     MODEL BOUYANCY (AND PERHAPS WIND) FORCED CONTINENTAL SHELF        MOD00020
C       MOTIONS AND CONDITIONS FOR THE POLAR REGIONS.                   MOD00030
                                                                        MOD00040
      INTEGER NX, NY                                                    MOD00050
      PARAMETER (NX = 36, NY = 36)                                      MOD00060
                                                                        MOD00070
C     DATA ARRAYS                                                       MOD00080
      REAL UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)               MOD00090
      REAL SS(NX, NY), SD(NX, NY), QSS(NX, NY), QSD(NX, NY)             MOD00100
      REAL WE(NX, NY), H(NX, NY)                                        MOD00110
                                                                        MOD00120
C     PHYSICAL PARAMETERS                                               MOD00130
      REAL AHM, AVM, AHS, AVS                                           MOD00140
      REAL SDREF, SSREF, RHOREF                                         MOD00150
      REAL G, F, BETA                                                   MOD00160
      INTEGER XMIN, XMAX, YMIN, YMAX, STRSPR, STRSUM, STRFLL, STRWIN    MOD00170
      REAL QSFMAX, QSFREF, QSM                                          MOD00180
                                                                        MOD00190
C     NUMERICAL PARAMETERS                                              MOD00200
      REAL DELX, DELY, DELT                                             MOD00210
      INTEGER NOUT, NFLOUT, NTOT, LOY, TAV                              MOD00220
      REAL SCRIT                                                        MOD00230
                                                                        MOD00240
C     LOCAL VARIABLES                                                   MOD00250
      INTEGER I                                                         MOD00260
      CHARACTER*60 FNAME                                                MOD00270
C     CONSERVATION TEST VARIABLES                                       MOD00280
      DOUBLE PRECISION S1, S2, S3, S4, S5, S6, S7, S8, S1OLD            MOD00290
                                                                        MOD00300
C***********************************************************----------!!MOD00310
C     BEGIN EXECUTION                                                   MOD00320
                                                                        MOD00330
C     INITIALIZE THE VARIABLES                                          MOD00340
      CALL INIT (UC, VC, UT, VT, SS, SD, WE, H, NX, NY,                 MOD00350
     2           AHM, AVM, AHS, AVS,                                    MOD00360
     3           SDREF, SSREF, RHOREF, G, F,                            MOD00370
     4           DELX, DELY, DELT,                                      MOD00380
     5           NOUT, NFLOUT, NTOT, SCRIT, TAV,                        MOD00390
     6           XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,           MOD00400
     7           STRSPR, STRSUM, STRFLL, STRWIN, LOY,                   MOD00410
     8           BETA                                         )         MOD00420
      I = 0                                                             MOD00430
      S1OLD = 0.D0                                                      MOD00440
                                                                        MOD00450
C     OPEN THE OUTPUT FILES                                             MOD00460
CD    OPEN (1, FILE='QOUT', FORM='UNFORMATTED', STATUS='NEW')           MOD00470
      CALL OUTSTR(UC, VC, UT, VT, SS, SD, H, DELX, NX, NY)              MOD00480
      CALL UVTROP(UT, VT, WE, H, NX, NY, DELX, DELY, F, BETA, AHM)      MOD00490
      CALL HEADER(WE, UT, VT, H, NX, NY,                                MOD00500
     1                AHM, AVM, AHS, AVS,                               MOD00510
     2                SDREF, SSREF, RHOREF, G, F,                       MOD00520
     3                DELX, DELY, DELT,                                 MOD00530
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,                   MOD00540
     4                XMIN, XMAX, YMIN, YMAX, QSFMAX, QSFREF, QSM,      MOD00550
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )    MOD00560
CM      OPEN (1, FILE='TIMEINFO', FORM='FORMATTED', STATUS='NEW')       MOD00570
CM      PRINT *,LONG(362)                                               MOD00580
CM      WRITE(1,9001) 0, LONG(362)                                      MOD00590
CM 9001 FORMAT (2I12)                                                   MOD00600
C     EXTRAPOLATION LOOP                                                MOD00610
      DO 1000 I = 1, NTOT                                               MOD00620
CM        WRITE(*,9001) I, LONG(362)                                    MOD00630
CM        WRITE(1,9001) I, LONG(362)                                    MOD00640
        CALL UVEXT (UC, VC, SS, SD, H, NX, NY,                          MOD00650
     2              RHOREF, G, F, AHM, AVM, DELX, DELY, DELT    )       MOD00660
                                                                        MOD00670
        CALL QSEXT (QSS, QSD, H, WE, NX, NY, I, LOY,                    MOD00680
     1              XMIN, XMAX, YMIN, YMAX,                             MOD00690
     2              QSFMAX, QSFREF, QSM, DELX, DELY,                    MOD00700
     3              STRSPR, STRSUM, STRFLL, STRWIN              )       MOD00710
CD      WRITE (1) QSS                                                   MOD00720
                                                                        MOD00730
CD      CALL SCONV (UC, VC, UT, VT, WE, SS, SD, QSS, QSD, H,            MOD00740
CD   1              NX, NY, DELX, DELY, DELT, SDREF, SSREF,             MOD00750
CD   2              AHS, AVS, I, S1, S2,                                MOD00760
CD   3              S3, S4, S5, S6, S7, S8                      )       MOD00770
CD      WRITE (*,9015) I, (S1-S1OLD)/DELT, S2, S1,                      MOD00780
CD   1                    S3, S4, S5, S6, S7, S8                        MOD00790
CD      S1OLD = S1                                                      MOD00800
                                                                        MOD00810
        CALL STEXT (UC, VC, UT, VT, WE, SS, SD, QSS, QSD, H,            MOD00820
     1              NX, NY, DELX, DELY, DELT, SDREF, SSREF,             MOD00830
     2              AHS, AVS, I                                 )       MOD00840
                                                                        MOD00850
        CALL CONVEC(SS, SD, NX, NY, I)                                  MOD00860
        CALL TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, I,            MOD00870
     1                     NX, NY, DELX, DELY                    )      MOD00880
                                                                        MOD00890
        IF (MOD(I,NOUT) .EQ. 0) THEN                                    MOD00900
           CALL OUTDAT (UC, VC, UT, VT, SS, SD, H, DELX, NX, NY  )      MOD00910
           PRINT *,'TSTEP=',I                                           MOD00920
        ENDIF                                                           MOD00930
        IF (MOD(I,NFLOUT) .EQ. 0)                                       MOD00940
     1      CALL OUTFL (UC, VC, UT, VT, SS, SD, H, DELX, NX, NY  )      MOD00950
 1000 CONTINUE                                                          MOD00960
                                                                        MOD00970
      CALL OUTEND (UC, VC, UT, VT, SS, SD, H, DELX, NX, NY       )      MOD00980
                                                                        MOD00990
 9015 FORMAT (I4,3D16.10,/,6D12.6)                                      MOD01000
                                                                        MOD01010
      END                                                               MOD01020
C***********************************************************----------!!MOD01030
      SUBROUTINE HEADER(WE, UT, VT, H, NX, NY,                          MOD01040
     1                AHM, AVM, AHS, AVS,                               MOD01050
     2                SDREF, SSREF, RHOREF, G, F,                       MOD01060
     3                DELX, DELY, DELT,                                 MOD01070
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,                   MOD01080
     4                XCEN, XLEN, YCEN, YLEN, QFMAX, QFREF, QM,         MOD01090
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )    MOD01100
C     COMPUTE THE NONDIMENSIONAL PARAMETERS USED IN DESCRIBING THE      MOD01110
C        EXPERIMENTS.  2-1-90. BG                                       MOD01120
                                                                        MOD01130
      REAL SECPYR                                                       MOD01140
      PARAMETER (SECPYR = 3.1556908E7)                                  MOD01150
      REAL PI                                                           MOD01160
      PARAMETER (PI = 3.141592654)                                      MOD01170
                                                                        MOD01180
      INTEGER NX, NY                                                    MOD01190
      REAL WE(NX, NY), H(NX, NY)                                        MOD01200
      REAL UT(NX, NY), VT(NX, NY)                                       MOD01210
      REAL AHM, AVM, AHS, AVS                                           MOD01220
      REAL SREF, SDREF, SSREF, RHOREF                                   MOD01230
      REAL G, F                                                         MOD01240
      REAL DELX, DELY, DELT                                             MOD01250
      INTEGER NOUT, NFLOUT, NTOT, TAV                                   MOD01260
      REAL SCRIT                                                        MOD01270
                                                                        MOD01280
C     PARAMETERS FOR BUOYANCY FORCING                                   MOD01290
      INTEGER XCEN, XLEN, YCEN, YLEN                                    MOD01300
      REAL QFMAX, QFREF, QM                                             MOD01310
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY                       MOD01320
                                                                        MOD01330
C     PARMS FOR BAROTROPIC FLOW                                         MOD01340
      REAL BETA                                                         MOD01350
                                                                        MOD01360
      REAL GAMMA, ETA(18), DELS, CFLUX, TFLUX(18)                       MOD01370
      REAL C, TOTQ, SUMW                                                MOD01380
      INTEGER NSCRIT, I, J, K                                           MOD01390
      REAL XNOT, SIGX, YNOT, SIGY, DX, DY

      XNOT = FLOAT(XCEN)
      SIGX = FLOAT(XLEN)
      YNOT = FLOAT(YCEN)
      SIGY = FLOAT(YLEN)
      C = 0.809                                                         MOD01400
CD    PRINT *,QFMAX, QFREF, XNOT, SIGX, YNOT, SIGY
CD    PRINT *, H(1,1), WE(9,9)

      NSCRIT = 12                                                       MOD01410
      DELS = QFMAX*STRSPR*DELT*EXP(-1.4**2/2-0.675**2/2)
     1            /H(NX/2,NY/2)
      CFLUX = G*H(1,1)**2*C/8/RHOREF/F * DELS                           MOD01430
C***********************************************************----------!!MOD01440
      TOTQ = 0.0                                                        MOD01450
      DO 100 J = 2, NY                                                  MOD01460
        DO 110 I = 1, NX                                                MOD01470
          TOTQ = TOTQ+QFREF+  EXP(-(I*DELX-XNOT)**2/2./SIGX**2          MOD01480
     1                            -(J*DELY-YNOT)**2/2./SIGY**2 )*QFMAX  MOD01490
 110    CONTINUE                                                        MOD01500
 100  CONTINUE                                                          MOD01510
      TOTQ = TOTQ * DELX*DELX
      WRITE (56, 9001) DELS, CFLUX, TOTQ                                MOD01520
      DO 1000 I = 1, NSCRIT                                             MOD01530
        GAMMA = STRSPR*DELT*(QFMAX+QFREF)/H(1,1)                        MOD01540
     1          /(SCRIT+0.005*(I-1))                                    MOD01550
        SUMW = 0.0                                                      MOD01560
        DO 1100 J = 2, NY/2                                             MOD01570
          SUMW = 0.5*(MAX(0.0,VT(1,J))+MAX(0.0,VT(NX,J)))               MOD01580
          DO 1200 K = 2, NX-1                                           MOD01590
            SUMW = SUMW+MAX(0.0,VT(K,J))                                MOD01600
 1200     CONTINUE                                                      MOD01610
C         ETA(J) = (STRSPR/LOY)*(BETA/F)/((SCRIT+0.005*(I-1)))          MOD01620
C    1            *TOTQ /SUMW/DELX                                      MOD01630
          TFLUX(J) = SUMW*DELX*H(NX/2,NY/2)                             MOD01640
          ETA(J) = STRSPR*TOTQ/(LOY*TFLUX(J)*
     1                (SCRIT+0.005*(I-1))                    )
 1100   CONTINUE                                                        MOD01650
        IF (I.EQ.1) WRITE (56, 9001)(ABS(TFLUX(J)),J=2,NY/2)            MOD01660
        WRITE (56, 9002) GAMMA,(ABS(ETA(J)),J=2,NY/2)                   MOD01670
 1000 CONTINUE                                                          MOD01680
                                                                        MOD01690
 9001 FORMAT (6E12.3)                                                   MOD01700
 9002 FORMAT (10F7.3)                                                   MOD01710
      RETURN                                                            MOD01720
      END                                                               MOD01730
      SUBROUTINE TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, TSTEP,    MOD01740
     1                     NX, NY, DELX, DELY)                          MOD01750
C     PROGRAM TO COMPUTE AVERAGED QUANTITIES FROM MODEL OUTPUT          MOD01760
C     VERSION MODIFIED TO WORK 'ON THE FLY'.  ORIGINAL                  MOD01770
C       ASSUMED THAT THE FILES WERE ALREADY WRITTEN AND MERELY          MOD01780
C       NEEDED AVERAGING.  NOW WORK WITH DATA AS IT IS BEING GENERATED. MOD01790
C       BG 9-29-89                                                      MOD01800
      INTEGER NX, NY, NNX, NNY                                          MOD01810
      PARAMETER (NNX = 36, NNY = 36)                                    MOD01820
                                                                        MOD01830
      REAL UC(NX, NY), VC(NX, NY), SS(NX, NY), SD(NX, NY)               MOD01840
      REAL UT(NX, NY), VT(NX, NY), H(NX, NY)                            MOD01850
      REAL SCRIT, DELX, DELY                                            MOD01860
                                                                        MOD01870
      INTEGER NSCRIT                                                    MOD01880
      PARAMETER (NSCRIT = 12)                                           MOD01890
      REAL DELSCRIT                                                     MOD01900
      PARAMETER (DELSCRIT = 0.005)                                      MOD01910
      REAL FTAV                                                         MOD01920
      INTEGER I, J, K, L                                                MOD01930
      INTEGER TSTEP, TAV                                                MOD01940
      CHARACTER*60 FNAME                                                MOD01950
      REAL R1(NNX, NNY), R2(NNX, NNY), R3(NNX, NNY), R4(NNX, NNY)       MOD01960
      REAL R5(NNX, NNY), R6(NNX, NNY)                                   MOD01970
      REAL F1(NNY,NSCRIT), F2(NNY,NSCRIT)                               MOD01980
      REAL F1T(NNY,NSCRIT), F2T(NNY,NSCRIT)                             MOD01990
      SAVE R1, R2, R3, R4, R5, R6, F1, F2, F1T, F2T                     MOD02000
                                                                        MOD02010
C***********************************************************----------!!MOD02020
                                                                        MOD02030
C     NUMBER OF STEPS TO AVERAGE OVER IS TAV.                           MOD02040
      FTAV = FLOAT(TAV)                                                 MOD02050
      IF (TSTEP .EQ. 1) THEN                                            MOD02060
        DO 1040 K = 1, NY                                               MOD02070
          DO 1050 L = 1, NX                                             MOD02080
            R1(L,K) = 0.0                                               MOD02090
            R2(L,K) = 0.0                                               MOD02100
            R3(L,K) = 0.0                                               MOD02110
            R4(L,K) = 0.0                                               MOD02120
            R5(L,K) = 0.0                                               MOD02130
            R6(L,K) = 0.0                                               MOD02140
 1050     CONTINUE                                                      MOD02150
 1040   CONTINUE                                                        MOD02160
        DO 1060 K = 1, NY                                               MOD02170
          DO 1070 L = 1, NSCRIT                                         MOD02180
            F1(K,L) = 0.0                                               MOD02190
            F2(K,L) = 0.0                                               MOD02200
 1070     CONTINUE                                                      MOD02210
 1060   CONTINUE                                                        MOD02220
                                                                        MOD02230
       ELSE                                                             MOD02240
                                                                        MOD02250
        DO 1900 I = 1, NSCRIT                                           MOD02260
         CALL REFLUX(VT, VC, SS, SD, H, F1T(1,I), F2T(1,I),             MOD02270
     1                SCRIT+(I-1)*DELSCRIT, NX, NY, DELX, DELY)         MOD02280
          DO 1910 K = 1, NY                                             MOD02290
             F1(K,I) = F1(K,I)+F1T(K,I)                                 MOD02300
             F2(K,I) = F2(K,I)+F2T(K,I)                                 MOD02310
 1910     CONTINUE                                                      MOD02320
 1900   CONTINUE                                                        MOD02330
                                                                        MOD02340
        DO 2000 K = 1, NY                                               MOD02350
          DO 2010 L = 1, NX                                             MOD02360
            R1(L,K) = R1(L,K) + UC(L,K)                                 MOD02370
            R2(L,K) = R2(L,K) + VC(L,K)                                 MOD02380
            R3(L,K) = R3(L,K) + UT(L,K)                                 MOD02390
            R4(L,K) = R4(L,K) + VT(L,K)                                 MOD02400
            R5(L,K) = R5(L,K) + SS(L,K)                                 MOD02410
            R6(L,K) = R6(L,K) + SD(L,K)                                 MOD02420
 2010     CONTINUE                                                      MOD02430
 2000   CONTINUE                                                        MOD02440
                                                                        MOD02450
        IF ( MOD(TSTEP,TAV) .EQ. 0) THEN                                MOD02460
          DO 2020 K = 1, NY                                             MOD02470
            DO 2030 L = 1, NX                                           MOD02480
              R1(L,K) = R1(L,K) / FTAV                                  MOD02490
              R2(L,K) = R2(L,K) / FTAV                                  MOD02500
              R3(L,K) = R3(L,K) / FTAV                                  MOD02510
              R4(L,K) = R4(L,K) / FTAV                                  MOD02520
              R5(L,K) = R5(L,K) / FTAV                                  MOD02530
              R6(L,K) = R6(L,K) / FTAV                                  MOD02540
 2030       CONTINUE                                                    MOD02550
            DO 2040 I = 1, NSCRIT                                       MOD02560
       F1(K,I) = F1(K,I) / FTAV / 1.E5                                  MOD02570
              F2(K,I) = F2(K,I) / FTAV / 1.E5                           MOD02580
 2040       CONTINUE                                                    MOD02590
 2020     CONTINUE                                                      MOD02600
          WRITE (50) R1                                                 MOD02610
          WRITE (51) R2                                                 MOD02620
          WRITE (52) R3                                                 MOD02630
          WRITE (53) R4                                                 MOD02640
          WRITE (54) R5                                                 MOD02650
          WRITE (55) R6                                                 MOD02660
          DO 2100 J = 1, NSCRIT                                         MOD02670
           WRITE (56,9009) (F1(I,J),I=1,NY)                             MOD02680
           WRITE (57,9009) (F2(I,J),I=1,NY)                             MOD02690
 2100   CONTINUE                                                        MOD02700
         WRITE (56,9010)                                                MOD02710
          WRITE (57,9010)                                               MOD02720
          DO 2060 K = 1, NY                                             MOD02730
            DO 2050 L = 1, NX                                           MOD02740
              R1(L,K) = 0.0                                             MOD02750
              R2(L,K) = 0.0                                             MOD02760
              R3(L,K) = 0.0                                             MOD02770
              R4(L,K) = 0.0                                             MOD02780
              R5(L,K) = 0.0                                             MOD02790
              R6(L,K) = 0.0                                             MOD02800
 2050       CONTINUE                                                    MOD02810
      DO 2070 I = 1, NSCRIT                                             MOD02820
              F1(K,I) = 0.0                                             MOD02830
              F2(K,I) = 0.0                                             MOD02840
 2070       CONTINUE                                                    MOD02850
 2060     CONTINUE                                                      MOD02860
        ENDIF                                                           MOD02870
                                                                        MOD02880
      ENDIF                                                             MOD02890
                                                                        MOD02900
 9001 FORMAT (BN, I5)                                                   MOD02910
                                                                        MOD02920
 9002 FORMAT (A60)                                                      MOD02930
                                                                        MOD02940
 9009 FORMAT (16F5.2)                                                   MOD02950
                                                                        MOD02960
 9010 FORMAT (' END OF STEP')                                           MOD02970
                                                                        MOD02980
      RETURN                                                            MOD02990
      END                                                               MOD03000
      SUBROUTINE REFLUX(VT, VC, SS, SD, H, FLM, FLS,                    MOD03010
     1                          SCRIT, NX, NY, DX, DY)                  MOD03020
C     RECOMPUTE FLUXES USING A BOTTOM WATER DEFINITION                  MOD03030
                                                                        MOD03040
      INTEGER NX, NY                                                    MOD03050
      REAL VT(NX, NY), VC(NX, NY), SS(NX, NY), SD(NX, NY)               MOD03060
      REAL H(NX, NY), FLM(NY), FLS(NY)                                  MOD03070
      REAL DX, DY, SCRIT                                                MOD03080
      INTEGER I, J                                                      MOD03090
                                                                        MOD03100
      DO 1000 J = 1, NY                                                 MOD03110
        FLM(J) = 0.0                                                    MOD03120
        FLS(J) = 0.0                                                    MOD03130
        DO 1010 I = 1, NX                                               MOD03140
C         LOWER LAYER                                                   MOD03150
          IF (VT(I,J)-VC(I,J) .GT. 0.0) THEN                            MOD03160
            IF (SS(I,J)-SD(I,J) .GT. SCRIT) THEN                        MOD03170
              FLM(J) = FLM(J) + H(I,J)*0.5*(VT(I,J) - VC(I,J))          MOD03180
              FLS(J) = FLS(J) +H(I,J)*0.5*                              MOD03190
     1                    (VT(I,J) - VC(I,J))*(SS(I,J)-SD(I,J))         MOD03200
            ENDIF                                                       MOD03210
          ENDIF                                                         MOD03220
C         UPPER LAYER                                                   MOD03230
          IF (VT(I,J)+VC(I,J) .GT. 0.0) THEN                            MOD03240
            IF (SS(I,J)+SD(I,J) .GT. SCRIT) THEN                        MOD03250
              FLM(J) = FLM(J) + H(I,J)*0.5*(VT(I,J) + VC(I,J))          MOD03260
              FLS(J) = FLS(J) +H(I,J)*0.5*                              MOD03270
     1                    (VT(I,J) + VC(I,J))*(SS(I,J)+SD(I,J))         MOD03280
            ENDIF                                                       MOD03290
          ENDIF                                                         MOD03300
 1010   CONTINUE                                                        MOD03310
        FLM(J) = FLM(J)*DX                                              MOD03320
        FLS(J) = FLS(J)*DX                                              MOD03330
 1000 CONTINUE                                                          MOD03340
                                                                        MOD03350
      RETURN                                                            MOD03360
      END                                                               MOD03370
C***********************************************************----------!!MOD03380
      SUBROUTINE UVTROP(UT, VT, WE, H, NX, NY, DX, DY, F, BETA, AM)     MOD03390
C     BAROTROPIC SOLUTION.                                              MOD03400
      INTEGER NX, NY                                                    MOD03410
      REAL UT(NX, NY), VT(NX, NY), WE(NX, NY), H(NX, NY)                MOD03420
      REAL DX, DY, F, BETA, AM                                          MOD03430
                                                                        MOD03440
      INTEGER NNX, NNY                                                  MOD03450
      PARAMETER (NNX = 36)                                              MOD03460
      PARAMETER (NNY = 36)                                              MOD03470
      REAL PSI(NNX, NNY)                                                MOD03480
      REAL NUM, LB, WAVE                                                MOD03490
      INTEGER I, J                                                      MOD03500
                                                                        MOD03510
C     COMPUTE THE INTERIOR SOLUTION                                     MOD03520
      DO 1000 J = 1, NY                                                 MOD03530
        PSI(NX, J) = WE(NX, J)                                          MOD03540
        DO 1010 I = NX-1, 1, -1                                         MOD03550
          PSI(I, J) = PSI(I+1,J) + WE(I+1,J) + WE(I,J)                  MOD03560
 1010   CONTINUE                                                        MOD03570
 1000 CONTINUE                                                          MOD03580
      NUM = F*DX/2./BETA/H(1,1)                                         MOD03590
      DO 1020 J = 1, NY                                                 MOD03600
        DO 1030 I= 1, NX                                                MOD03610
          PSI(I,J) = -PSI(I,J) * NUM                                    MOD03620
 1030   CONTINUE                                                        MOD03630
 1020 CONTINUE                                                          MOD03640
                                                                        MOD03650
C     APPLY THE BOUNDARY LAYER CORRECTIONS.                             MOD03660
      LB = (AM/BETA)**(1./3.)                                           MOD03670
      WAVE = SQRT(3.)*DX/2./LB                                          MOD03680
      DO 2000 J = 1, NY                                                 MOD03690
        NUM = LB*(PSI(NX,J)-PSI(NX-1,J))/DX                             MOD03700
        DO 2010 I = 1, NX                                               MOD03710
          PSI(I,J) = PSI(I,J)*(1.-EXP(-(I-1)*DX/2./LB)*                 MOD03720
     1                 (COS(WAVE*(I-1)) + SIN(WAVE*(I-1))/SQRT(3.) ))   MOD03730
     2              - NUM*EXP( (I-NX)*DX/LB)                            MOD03740
 2010   CONTINUE                                                        MOD03750
 2000 CONTINUE                                                          MOD03760
                                                                        MOD03770
C     NOW THAT WE HAVE PSI, COMPUTE UT, VT.                             MOD03780
      DO 3000 J = 2, NY-1                                               MOD03790
        DO 3010 I = 2, NX-1                                             MOD03800
          UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY                      MOD03810
          VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX                      MOD03820
 3010   CONTINUE                                                        MOD03830
 3000 CONTINUE                                                          MOD03840
      DO 3020 J = 2, NY-1                                               MOD03850
        I = 1                                                           MOD03860
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY                        MOD03870
CD      VT(I,J) =  (PSI(I+1,J)-PSI(I  ,J))   /DX                        MOD03880
        VT(I,J) = 0.0                                                   MOD03890
        I = NX                                                          MOD03900
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY                        MOD03910
        VT(I,J) =  (PSI(I  ,J)-PSI(I-1,J))   /DX                        MOD03920
 3020 CONTINUE                                                          MOD03930
      DO 3030 I = 2, NX-1                                               MOD03940
        J = 1                                                           MOD03950
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J))/DY                             MOD03960
        VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX                        MOD03970
        J = NY                                                          MOD03980
        UT(I,J) = -(PSI(I,J)-PSI(I,J-1))/DY                             MOD03990
        VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX                        MOD04000
 3030 CONTINUE                                                          MOD04010
                                                                        MOD04020
      RETURN                                                            MOD04030
      END                                                               MOD04040
C***********************************************************----------!!MOD04050
      SUBROUTINE INIT(UB, VB, UT, VT, SS, SD, WE, H, NX, NY,            MOD04060
     1                AHM, AVM, AHS, AVS,                               MOD04070
     2                SDREF, SSREF, RHOREF, G, F,                       MOD04080
     3                DELX, DELY, DELT,                                 MOD04090
     4                NOUT, NFLOUT, NTOT, SCRIT, TAV,                   MOD04100
     4                XMIN, XMAX, YMIN, YMAX, QFMAX, QFREF, QM,         MOD04110
     5                STRSPR, STRSUM, STRFLL, STRWIN, LOY, BETA    )    MOD04120
C     INITIALIZE THE DATA ARRAYS AND CONSTANTS                          MOD04130
                                                                        MOD04140
      REAL SECPYR                                                       MOD04150
      PARAMETER (SECPYR = 3.1556908E7)                                  MOD04160
      REAL PI                                                           MOD04170
      PARAMETER (PI = 3.141592654)                                      MOD04180
                                                                        MOD04190
      INTEGER NX, NY                                                    MOD04200
      REAL UB(NX, NY), VB(NX, NY), UT(NX, NY), VT(NX, NY)               MOD04210
      REAL SS(NX, NY), SD(NX, NY)                                       MOD04220
      REAL WE(NX, NY), H(NX, NY)                                        MOD04230
                                                                        MOD04240
      REAL AHM, AVM, AHS, AVS                                           MOD04250
      REAL SREF, SDREF, SSREF, RHOREF                                   MOD04260
      REAL G, F                                                         MOD04270
      REAL DELX, DELY, DELT                                             MOD04280
      INTEGER NOUT, NFLOUT, NTOT, TAV                                   MOD04290
      REAL SCRIT                                                        MOD04300
                                                                        MOD04310
C     PARAMETERS FOR BUOYANCY FORCING                                   MOD04320
      INTEGER XMIN, XMAX, YMIN, YMAX                                    MOD04330
      REAL QFMAX, QFREF, QM                                             MOD04340
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY                       MOD04350
                                                                        MOD04360
C     PARMS FOR BAROTROPIC FLOW                                         MOD04370
      REAL BETA                                                         MOD04380
                                                                        MOD04390
      REAL RHO, VALUE, A, B                                             MOD04400
      CHARACTER*60 FNAME                                                MOD04410
      INTEGER I, J, DUMMY                                               MOD04420
      LOGICAL YES                                                       MOD04430
                                                                        MOD04440
C***********************************************************----------**MOD04450
CI    PRINT *,'WHAT IS THE NAME OF THE PARAMETER FILE?'                 MOD04460
CI    READ (*,9001) FNAME                                               MOD04470
CI    WRITE (*,9001) FNAME                                              MOD04480
      OPEN (11, FILE='MODEL9IN', FORM='FORMATTED', STATUS='OLD')        MOD04490
C***********************************************************----------**MOD04500
C     GET ARRAY DATA:                                                   MOD04510
                                                                        MOD04520
      PRINT *,'WOULD YOU LIKE TO USE OUTPUT FROM AN OLD RUN?'           MOD04530
      IF (YES(.FALSE.)) THEN                                            MOD04540
        PRINT *,'WHAT IS THE FILE NAME FOR UB?'                         MOD04550
        READ (11,9001) FNAME                                            MOD04560
        OPEN (12, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')         MOD04570
        PRINT *,'WHAT IS THE FILE NAME FOR VB?'                         MOD04580
        READ (11,9001) FNAME                                            MOD04590
        OPEN (13, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')         MOD04600
        WRITE (*,9011) 'UT   '                                          MOD04610
        READ (11,9001) FNAME                                            MOD04620
        OPEN (14, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')         MOD04630
        WRITE (*,9011) 'VT   '                                          MOD04640
        READ (11,9001) FNAME                                            MOD04650
        OPEN (15, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')         MOD04660
        PRINT *,'WHAT IS THE FILE NAME FOR SS?'                         MOD04670
        READ (11,9001) FNAME                                            MOD04680
        OPEN (16, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')         MOD04690
        PRINT *,'WHAT IS THE FILE NAME FOR SD?'                         MOD04700
        READ (11,9001) FNAME                                            MOD04710
        OPEN (17, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD')         MOD04720
                                                                        MOD04730
        PRINT *,'AT WHAT TIME STEP DO YOU WANT THE DATA?'               MOD04740
        READ (11,9003) DUMMY                                            MOD04750
        DO 1000 I = 1, DUMMY                                            MOD04760
          READ (12) UB                                                  MOD04770
          READ (13) VB                                                  MOD04780
          READ (14) UT                                                  MOD04790
          READ (15) VT                                                  MOD04800
          READ (16) SS                                                  MOD04810
          READ (17) SD                                                  MOD04820
 1000   CONTINUE                                                        MOD04830
                                                                        MOD04840
       ELSE                                                             MOD04850
        WRITE (*,9011) 'UB   '                                          MOD04860
        READ (11,9002) VALUE                                            MOD04870
        CALL ARSET(UB, NX, NY, VALUE)                                   MOD04880
        WRITE (*,9011) 'VB   '                                          MOD04890
        READ (11,9002) VALUE                                            MOD04900
        CALL ARSET(VB, NX, NY, VALUE)                                   MOD04910
        WRITE (*,9011) 'UT   '                                          MOD04920
        READ (11,9002) VALUE                                            MOD04930
        CALL ARSET(UT, NX, NY, VALUE)                                   MOD04940
        WRITE (*,9011) 'VT   '                                          MOD04950
        READ (11,9002) VALUE                                            MOD04960
        CALL ARSET(VT, NX, NY, VALUE)                                   MOD04970
        WRITE (*,9011) 'SS   '                                          MOD04980
        READ (11,9002) VALUE                                            MOD04990
        CALL ARSET(SS, NX, NY, VALUE)                                   MOD05000
        WRITE (*,9011) 'SD   '                                          MOD05010
        READ (11,9002) VALUE                                            MOD05020
        CALL ARSET(SD, NX, NY, VALUE)                                   MOD05030
      ENDIF                                                             MOD05040
                                                                        MOD05050
CU    PRINT *,'WHAT IS THE NAME OF THE EKMAN PUMPING FILE?'             MOD05060
CU      READ (*,9001) FNAME                                             MOD05070
CU      CALL READ2(WE, NX, NY, 2, FNAME, .TRUE., .TRUE.)                MOD05080
      WRITE (*,9011) 'WE   '                                            MOD05090
      READ (11,9002) VALUE                                              MOD05100
      PRINT *,'WHAT IS THE WAVE LENGTH IN X?'                           MOD05110
      READ (11,9002) A                                                  MOD05120
      PRINT *,'WHAT IS THE WAVE LENGTH IN Y?'                           MOD05130
      READ (11,9002) B                                                  MOD05140
      DO 7000 J = 0, NY-1                                               MOD05150
        DO 7010 I = 0, NX-1                                             MOD05160
          WE(I+1,J+1) = VALUE*SIN(2.*PI*I/A)*SIN(2.*PI*J/B)             MOD05170
CD           WE(I+1,J+1) = VALUE*SIN(2.*PI*J/B)
 7010   CONTINUE                                                        MOD05180
 7000 CONTINUE                                                          MOD05190
                                                                        MOD05200
      WRITE (*,9011) 'H    '                                            MOD05210
      READ (11,9002) VALUE                                              MOD05220
      CALL ARSET(H, NX, NY, VALUE)                                      MOD05230
                                                                        MOD05240
 9011 FORMAT (' WHAT IS THE VALUE OF ',A5,'?')                          MOD05250
                                                                        MOD05260
C***********************************************************----------**MOD05270
CU    PRINT *,'WHAT IS THE NAME OF THE SCALAR PARAMETER FILE?'          MOD05280
CU    READ (*,9001) FNAME                                               MOD05290
CU    WRITE (*,9001) FNAME                                              MOD05300
CU    OPEN (11, FILE=FNAME, FORM='FORMATTED', STATUS='OLD')             MOD05310
C        FORCING                                                        MOD05320
        READ (11,9003) XMIN                                             MOD05330
        READ (11,9003) XMAX                                             MOD05340
        READ (11,9003) YMIN                                             MOD05350
        READ (11,9003) YMAX                                             MOD05360
        READ (11,9002) QFMAX                                            MOD05370
        READ (11,9002) QFREF                                            MOD05380
        READ (11,9002) QM                                               MOD05390
        READ (11,9003) STRSPR                                           MOD05400
        READ (11,9003) STRSUM                                           MOD05410
        READ (11,9003) STRFLL                                           MOD05420
        READ (11,9003) STRWIN                                           MOD05430
C        OUTPUT                                                         MOD05440
        READ (11,9003) NOUT                                             MOD05450
        READ (11,9003) NFLOUT                                           MOD05460
        READ (11,9003) NTOT                                             MOD05470
        READ (11,9002) SCRIT                                            MOD05480
        READ (11,9003) TAV                                              MOD05490
C        GRID                                                           MOD05500
        READ (11,9002) DELX                                             MOD05510
        READ (11,9002) DELY                                             MOD05520
        READ (11,9002) DELT                                             MOD05530
C        BOUNDARY CONDITION INFO.                                       MOD05540
        READ (11,9002) SREF                                             MOD05550
        READ (11,9002) SDREF                                            MOD05560
        READ (11,9002) SSREF                                            MOD05570
C        DIFFUSION                                                      MOD05580
        READ (11,9002) AHM                                              MOD05590
        READ (11,9002) AVM                                              MOD05600
        READ (11,9002) AHS                                              MOD05610
        READ (11,9002) AVS                                              MOD05620
C        PHYSICAL CONSTANTS                                             MOD05630
        READ (11,9002) F                                                MOD05640
        READ (11,9002) G                                                MOD05650
C        TERMS FOR BAROTROPIC SOLUTION.                                 MOD05660
        READ (11,9002) BETA                                             MOD05670
CU    CLOSE (11, STATUS='KEEP')                                         MOD05680
                                                                        MOD05690
C     COMPUTED PARAMETERS:                                              MOD05700
      RHOREF = RHO(SREF, 0.0, 0.)                                       MOD05710
      LOY    = INT(SECPYR/DELT)                                         MOD05720
                                                                        MOD05730
C     RESCALE INPUT VALUES OF QFMAX (M/DAY), QFREF,QM (M/SEASON) TO     MOD05740
C       KG/M**2/S.  MUST DIVIDE BY H IN UVEXT.  8-10-88.                MOD05750
        QFMAX = QFMAX / 2.88E3                                          MOD05760
        QFREF = QFREF * (30./FLOAT(STRSPR-STRWIN)/DELT)                 MOD05770
        QM    = QM    * (30./FLOAT(STRFLL-STRSUM)/DELT)                 MOD05780
C        FORCING                                                        MOD05790
        WRITE (*,9003) XMIN                                             MOD05800
        WRITE (*,9003) XMAX                                             MOD05810
        WRITE (*,9003) YMIN                                             MOD05820
        WRITE (*,9003) YMAX                                             MOD05830
        WRITE (*,9002) QFMAX                                            MOD05840
        WRITE (*,9002) QFREF                                            MOD05850
        WRITE (*,9002) QM                                               MOD05860
        WRITE (*,9003) STRSPR                                           MOD05870
        WRITE (*,9003) STRSUM                                           MOD05880
        WRITE (*,9003) STRFLL                                           MOD05890
        WRITE (*,9003) STRWIN                                           MOD05900
C        OUTPUT                                                         MOD05910
        WRITE (*,9003) NOUT                                             MOD05920
        WRITE (*,9003) NFLOUT                                           MOD05930
        WRITE (*,9003) NTOT                                             MOD05940
        WRITE (*,9002) SCRIT                                            MOD05950
        WRITE (*,9003) TAV                                              MOD05960
C        GRID                                                           MOD05970
        WRITE (*,9002) DELX                                             MOD05980
        WRITE (*,9002) DELY                                             MOD05990
        WRITE (*,9002) DELT                                             MOD06000
C        BOUNDARY CONDITION INFO.                                       MOD06010
        WRITE (*,9002) SREF                                             MOD06020
        WRITE (*,9002) SDREF                                            MOD06030
        WRITE (*,9002) SSREF                                            MOD06040
C        DIFFUSION                                                      MOD06050
        WRITE (*,9002) AHM                                              MOD06060
        WRITE (*,9002) AVM                                              MOD06070
        WRITE (*,9002) AHS                                              MOD06080
        WRITE (*,9002) AVS                                              MOD06090
C        PHYSICAL CONSTANTS                                             MOD06100
        WRITE (*,9002) F                                                MOD06110
        WRITE (*,9002) G                                                MOD06120
C        TERMS FOR BAROTROPIC SOLUTION.                                 MOD06130
        WRITE (*,9002) BETA                                             MOD06140
C        COMPUTED CONSTANTS                                             MOD06150
        WRITE (*,9003) LOY                                              MOD06160
                                                                        MOD06170
 9001 FORMAT (A60)                                                      MOD06180
                                                                        MOD06190
 9002 FORMAT (BN, E13.6)                                                MOD06200
                                                                        MOD06210
 9003 FORMAT (BN, I8)                                                   MOD06220
                                                                        MOD06230
      RETURN                                                            MOD06240
      END                                                               MOD06250
C***********************************************************----------!!MOD06260
      SUBROUTINE OUTDAT (UC, VC, UT, VT, SS, SD, H, DX, NX, NY       )  MOD06270
                                                                        MOD06280
C     WRITE OUT THE VELOCITY, SALINITY, AND TEMPERATURE.                MOD06290
      INTEGER NFIELD                                                    MOD06300
      PARAMETER (NFIELD = 6)                                            MOD06310
                                                                        MOD06320
      INTEGER NX, NY                                                    MOD06330
      REAL UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)               MOD06340
      REAL SS(NX, NY), SD(NX, NY), H(NX, NY)                            MOD06350
      REAL DX                                                           MOD06360
                                                                        MOD06370
      INTEGER I, J                                                      MOD06380
      REAL UCTEMP, VCTEMP                                               MOD06390
      INTEGER NFL                                                       MOD06400
      PARAMETER (NFL = 36)                                              MOD06410
      REAL FLM(NFL), FLS(NFL)                                           MOD06420
                                                                        MOD06430
      CHARACTER*60 FNAME(NFIELD)                                        MOD06440
      SAVE FLM, FLS, FNAME                                              MOD06450
                                                                        MOD06460
      UCTEMP  = UC(1,1)                                                 MOD06470
      VCTEMP  = VC(1,1)                                                 MOD06480
      UC(1,1) = 0.0125                                                  MOD06490
      VC(1,1) = 0.0125                                                  MOD06500
      WRITE (30) UC                                                     MOD06510
      WRITE (31) VC                                                     MOD06520
      WRITE (32) UT                                                     MOD06530
      WRITE (33) VT                                                     MOD06540
      WRITE (34) SS                                                     MOD06550
      WRITE (35) SD                                                     MOD06560
                                                                        MOD06570
      UC(1,1) = UCTEMP                                                  MOD06580
      VC(1,1) = VCTEMP                                                  MOD06590
                                                                        MOD06600
      RETURN                                                            MOD06610
                                                                        MOD06620
      ENTRY OUTFL(UC, VC, UT, VT, SS, SD, H, DX, NX, NY)                MOD06630
                                                                        MOD06640
      DO 1000 J = 1, NY                                                 MOD06650
        FLM(J) = 0.0                                                    MOD06660
        FLS(J) = 0.0                                                    MOD06670
        DO 1010 I = 1, NX                                               MOD06680
          FLM(J) = FLM(J) + 0.5*H(I,J)*DX*(VT(I,J) - VC(I,J))           MOD06690
          FLS(J) = FLS(J) + 0.5*H(I,J)*DX*                              MOD06700
     1                (VT(I,J) - VC(I,J))*(SS(I,J) - SD(I,J))           MOD06710
 1010   CONTINUE                                                        MOD06720
 1000 CONTINUE                                                          MOD06730
                                                                        MOD06740
      WRITE (40) FLM                                                    MOD06750
      WRITE (41) FLS                                                    MOD06760
                                                                        MOD06770
      RETURN                                                            MOD06780
                                                                        MOD06790
      ENTRY OUTSTR(UC, VC, UT, VT, SS, SD, H, DX, NX, NY)               MOD06800
                                                                        MOD06810
C     OPEN THE NECESSARY OUTPUT FILES                                   MOD06820
      DO 2000 I = 1, NFIELD                                             MOD06830
        PRINT *,'WHAT IS THE NAME OF INSTANTANEOUS OUTPUT FILE # ',I    MOD06840
        READ  (11,9002) FNAME(I)                                        MOD06850
        WRITE (*,9002) FNAME(I)                                         MOD06860
        OPEN (29+I, FILE=FNAME(I), FORM='UNFORMATTED', STATUS='NEW')    MOD06870
 2000 CONTINUE                                                          MOD06880
      PRINT *,'NAME FOR THE MASS FLUX FILE '                            MOD06890
      READ  (11, 9002) FNAME(1)                                         MOD06900
      WRITE (*, 9002) FNAME(1)                                          MOD06910
      OPEN (40, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')        MOD06920
      PRINT *,'NAME FOR THE SALT FLUX FILE '                            MOD06930
      READ  (11, 9002) FNAME(1)                                         MOD06940
      WRITE (*, 9002) FNAME(1)                                          MOD06950
      OPEN (41, FILE=FNAME(1), FORM='UNFORMATTED', STATUS='NEW')        MOD06960
C     AVERAGED OUTPUT FILES                                             MOD06970
      DO 2010 I = 1, NFIELD                                             MOD06980
        PRINT *,'WHAT IS THE NAME OF AVERAGED OUTPUT FILE # ',I         MOD06990
        READ  (11,9002) FNAME(I)                                        MOD07000
        WRITE (*,9002) FNAME(I)                                         MOD07010
        OPEN (49+I, FILE=FNAME(I), FORM='UNFORMATTED', STATUS='NEW')    MOD07020
 2010 CONTINUE                                                          MOD07030
      PRINT *,'NAME FOR THE MASS FLUX FILE '                            MOD07040
      READ  (11, 9002) FNAME(1)                                         MOD07050
      WRITE (*, 9002) FNAME(1)                                          MOD07060
      OPEN (56, FILE=FNAME(1), FORM='FORMATTED', STATUS='NEW')          MOD07070
      PRINT *,'NAME FOR THE SALT FLUX FILE '                            MOD07080
      READ  (11, 9002) FNAME(1)                                         MOD07090
      WRITE (*, 9002) FNAME(1)                                          MOD07100
      OPEN (57, FILE=FNAME(1), FORM='FORMATTED', STATUS='NEW')          MOD07110
 9002 FORMAT (A60)                                                      MOD07120
                                                                        MOD07130
      RETURN                                                            MOD07140
                                                                        MOD07150
      ENTRY OUTEND(UC, VC, UT, VT, SS, SD, H, DX, NX, NY)               MOD07160
                                                                        MOD07170
C     CLOSE THE DATA FILES:                                             MOD07180
      CLOSE (30, STATUS='KEEP')                                         MOD07190
      CLOSE (31, STATUS='KEEP')                                         MOD07200
      CLOSE (32, STATUS='KEEP')                                         MOD07210
      CLOSE (33, STATUS='KEEP')                                         MOD07220
      CLOSE (34, STATUS='KEEP')                                         MOD07230
      CLOSE (35, STATUS='KEEP')                                         MOD07240
      CLOSE (40, STATUS='KEEP')                                         MOD07250
      CLOSE (41, STATUS='KEEP')                                         MOD07260
      CLOSE (50, STATUS='KEEP')                                         MOD07270
      CLOSE (51, STATUS='KEEP')                                         MOD07280
      CLOSE (52, STATUS='KEEP')                                         MOD07290
      CLOSE (53, STATUS='KEEP')                                         MOD07300
      CLOSE (54, STATUS='KEEP')                                         MOD07310
      CLOSE (55, STATUS='KEEP')                                         MOD07320
      CLOSE (56, STATUS='KEEP')                                         MOD07330
      CLOSE (57, STATUS='KEEP')                                         MOD07340
                                                                        MOD07350
      RETURN                                                            MOD07360
      END                                                               MOD07370
C***********************************************************----------!!MOD07380
      SUBROUTINE UVEXT (UB, VB, SS, SD, H, NX, NY,                      MOD07390
     1                  RHOREF, G, F, AHM, AVM, DELX, DELY, DELT)       MOD07400
C     EXTRAPOLATE U, V TO THE NEXT TIME LEVEL                           MOD07410
C     COMPUTATION OF COMMON CONSTANTS ADDED PRIOR TO 5-26-88.           MOD07420
C     VERSION REWRITTEN FOR GEOSTROPHY, A LA DERIVATION. 4-5-89.        MOD07430
C       MUCH COMMENTED PROGRAM DELETED 4-5-89.                          MOD07440
                                                                        MOD07450
      INTEGER NX, NY                                                    MOD07460
      REAL UB(NX, NY), VB(NX, NY)                                       MOD07470
      REAL SS(NX, NY), SD(NX, NY)                                       MOD07480
      REAL H(NX, NY)                                                    MOD07490
      REAL RHOREF, F, G, AHM, AVM                                       MOD07500
      REAL DELX, DELY, DELT                                             MOD07510
                                                                        MOD07520
      INTEGER NNX, NNY                                                  MOD07530
      PARAMETER (NNX = 36)                                              MOD07540
      PARAMETER (NNY = 36)                                              MOD07550
      REAL RHOS1P, RHOS(NNX, NNY)                                       MOD07560
      INTEGER I, J, K, L                                                MOD07570
C     PARAMS FOR SPEEDIER NUMERICS:                                     MOD07580
      REAL DX2, DY2, G8RREF                                             MOD07590
                                                                        MOD07600
C     COMPUTE PARAMS FOR SPEEDIER NUMERICS:                             MOD07610
      DX2    = 2.*DELX                                                  MOD07620
      DY2    = 2.*DELY                                                  MOD07630
      G8RREF = G*H(1,1)/4./RHOREF/F                                     MOD07640
                                                                        MOD07650
C     COMPUTE THE DENSITY FIELD BEFORE ENTERING THE EXTRAPOLATION.      MOD07660
C     THIS REDUCES THE NUMBER OF CALLS TO THE DENSITY FUNCTION BY       MOD07670
C       ALMOST A FACTOR OF 4.  8-4-88.                                  MOD07680
      DO 900 L = 1, NY                                                  MOD07690
        DO 910 K = 1, NX                                                MOD07700
            RHOS(K,L) = RHOS1P( SS(K,L), 0.0, 0.0)                      MOD07710
  910   CONTINUE                                                        MOD07720
  900 CONTINUE                                                          MOD07730
                                                                        MOD07740
C     COMPUTE THE GEOSTROPHIC VELOCITY                                  MOD07750
      DO 1000 J = 2, NY-1                                               MOD07760
        DO 1010 I = 2, NX-1                                             MOD07770
                                                                        MOD07780
          UB(I,J) = +G8RREF*( RHOS(I,J+1) - RHOS(I,J-1) )/DY2           MOD07790
          VB(I,J) = -G8RREF*( RHOS(I+1,J) - RHOS(I-1,J) )/DX2           MOD07800
                                                                        MOD07810
 1010   CONTINUE                                                        MOD07820
 1000 CONTINUE                                                          MOD07830
                                                                        MOD07840
C     NOW CONSIDER THE BOUNDARY CONDITIONS:                             MOD07850
C       1-26-89:                                                        MOD07860
C         AT I = 1         U = V = 0.0                                  MOD07870
C         AT I = NX        U = V = 0.0                                  MOD07880
C         AT J = 1         U = V = 0.0                                  MOD07890
C         AT J = NY NORMAL DERIV = 0.0                                  MOD07900
                                                                        MOD07910
      DO 2000 I = 1, NX                                                 MOD07920
C       BC ON V AT THE Y BOUNDARIES                                     MOD07930
        VB(I,1)  = 0.0                                                  MOD07940
        VB(I,NY) = VB(I, NY-1)                                          MOD07950
C       BC ON U AT THE Y BOUNDARIES                                     MOD07960
        UB(I,1)  = 0.0                                                  MOD07970
        UB(I,NY) = UB(I, NY-1)                                          MOD07980
 2000 CONTINUE                                                          MOD07990
                                                                        MOD08000
      DO 2010 J = 1, NY                                                 MOD08010
C       V = 0.0 IMPLEMENTED 1-26-89                                     MOD08020
        VB(1,  J) = 0.0                                                 MOD08030
        VB(NX, J) = 0.0                                                 MOD08040
C       U = 0.0 IMPLEMENTED 1-26-89                                     MOD08050
        UB(1,  J) = 0.0                                                 MOD08060
        UB(NX, J) = 0.0                                                 MOD08070
 2010 CONTINUE                                                          MOD08080
                                                                        MOD08090
      RETURN                                                            MOD08100
      END                                                               MOD08110
C***********************************************************----------!!MOD08120
      FUNCTION RHO(S, T, P)                                             MOD08130
C     COMPUTE THE DENSITY OF WATER AS A FUNCTION OF SALINITY,           MOD08140
C       TEMPERATURE, AND PRESSURE.                                      MOD08150
C     USE THE ENTIRE EQUATION OF STATE GIVEN IN GILL, 1982.             MOD08160
C     S IN PSU, T IN DEGREES C, P IN BARS                               MOD08170
                                                                        MOD08180
      REAL RHO                                                          MOD08190
      REAL S, T, P                                                      MOD08200
                                                                        MOD08210
      DOUBLE PRECISION SIGMAW, SIGMA, SIGMAP                            MOD08220
      DOUBLE PRECISION DELTAW, DELTA, DELTAP                            MOD08230
                                                                        MOD08240
      SIGMAW = -.157406 + T*(6.793952D-2 + T*(-9.09529D-3               MOD08250
     1      + T*(1.001685D-4 +T*(-1.120083D-6 + 6.536332D-9*T))))       MOD08260
                                                                        MOD08270
      SIGMA  = SIGMAW +                                                 MOD08280
     1    S* ( .824493 + T*(-4.0899D-3 + T*(7.6438D-5                   MOD08290
     2                      + T*(-8.2467D-7 + T*5.3875D-9))) ) +        MOD08300
     3    S**1.5*( -5.72466D-3 + T*(1.0227D-4 - T*1.6546D-6)) +         MOD08310
     4    S*S   *( 4.8314D-4 )                                          MOD08320
                                                                        MOD08330
C     NOW COMPUTE THE COMPRESSIBILITY TERMS:                            MOD08340
      DELTAW = -347.79 + T*(148.4206 + T*(-2.327105 +                   MOD08350
     1                     T*(-1.360477D-2 - T*5.155288D-5 )))          MOD08360
                                                                        MOD08370
      DELTA  = DELTAW +                                                 MOD08380
     1    S  * (54.676 + T*(-.603459 + T*(1.09987D-2 - T*6.167D-5)) )   MOD08390
     2  + S**1.5*( 7.944D-2 + T*(1.6483D-2 - T*5.3009D-4) )             MOD08400
                                                                        MOD08410
      DELTAP = DELTA +                                                  MOD08420
     1   P *  (3.239908+T*(1.43713D-3+T*(1.16092D-4-T*5.77905D-7)) )    MOD08430
     2  +P*S* (2.2838D-3 + T*(-1.0981D-5*T - T*1.6078D-6) )             MOD08440
     3  +P*S**1.5*( 1.91075D-4 )                                        MOD08450
     4  +P*P* (8.50935D-5 + T*(-6.12293D-6 + T*5.2787D-8) )             MOD08460
     5  +P*P*S*(-9.9348D-7 +T*(2.0816D-8 + T*9.1697D-10) )              MOD08470
                                                                        MOD08480
C     NOW COMPUTE THE DENSITY:                                          MOD08490
      RHO = (1000. + SIGMA)/ (1 - P/(20000.+ DELTAP) )                  MOD08500
                                                                        MOD08510
      RETURN                                                            MOD08520
      END                                                               MOD08530
      FUNCTION RHOS1P(S, T, P)                                          MOD08540
C     THIS IS AN APPROXIMATE REPRESENTATION OF RHO, FITTED OVER A       MOD08550
C       CHARACTERISTIC RANGE OF T, S TO THE COMPLETE EQN. OF STATE.     MOD08560
C     COMPUTE THE DEVIATION FROM THE REFERENCE DENSITY.                 MOD08570
      REAL S, T, P, RHOS1P                                              MOD08580
      REAL ALPHA, BETA, GAMMA                                           MOD08590
      PARAMETER (ALPHA = -4.5795E-2)                                    MOD08600
      PARAMETER (BETA  = -6.8927E-3)                                    MOD08610
      PARAMETER (GAMMA =  0.80908  )                                    MOD08620
C     REFERENCE VALUES ARE T= -0.5, S=34.6, P = 0.0                     MOD08630
                                                                        MOD08640
      RHOS1P = T*(ALPHA + BETA*T) + GAMMA*S                             MOD08650
                                                                        MOD08660
      RETURN                                                            MOD08670
      END                                                               MOD08680
C***********************************************************----------!!MOD08690
      SUBROUTINE CONVEC(SS, SD, NX, NY, TSTEP)                          MOD08700
C     SUBROUTINE TO LOOK FOR CONVECTIVE OVERTURNING AND CABBELING.      MOD08710
C     OVERTURN ADDED 3-9-88.                                            MOD08720
C     USES SHORT FORM ENTRY POINT TO RHO 5-26-88.                       MOD08730
                                                                        MOD08740
      INTEGER NX, NY, TSTEP                                             MOD08750
      REAL SS(NX, NY), SD(NX, NY)                                       MOD08760
                                                                        MOD08770
      INTEGER I, J                                                      MOD08780
      LOGICAL OVRTRN, OVRLST                                            MOD08790
      SAVE    OVRLST                                                    MOD08800
                                                                        MOD08810
      OVRTRN = .FALSE.                                                  MOD08820
      DO 1000 J = 1, NY                                                 MOD08830
        DO 1010 I = 1, NX                                               MOD08840
CF        RHO1 = RHOS1P(SS(I,J)+SD(I,J), 0.0, 0.)                       MOD08850
CF        RHO2 = RHOS1P(SS(I,J)-SD(I,J), 0.0, 0.)                       MOD08860
          IF (SD(I,J) .GT. 0.0) THEN                                    MOD08870
            SD(I,J) = 0.0                                               MOD08880
            OVRTRN = .TRUE.                                             MOD08890
          ENDIF                                                         MOD08900
 1010   CONTINUE                                                        MOD08910
 1000 CONTINUE                                                          MOD08920
                                                                        MOD08930
      IF (TSTEP .EQ. 1) GO TO 9999                                      MOD08940
                                                                        MOD08950
      IF (OVRTRN .AND. (.NOT. OVRLST) ) THEN                            MOD08960
        PRINT *,'STARTED CONVECTION AT TSTEP',TSTEP                     MOD08970
       ELSE IF ((.NOT. OVRTRN)  .AND. OVRLST ) THEN                     MOD08980
        PRINT *,'STOPPED CONVECTION AT TSTEP',TSTEP                     MOD08990
      ENDIF                                                             MOD09000
                                                                        MOD09010
 9999 CONTINUE                                                          MOD09020
      OVRLST = OVRTRN                                                   MOD09030
                                                                        MOD09040
      RETURN                                                            MOD09050
      END                                                               MOD09060
C***********************************************************----------!!MOD09070
      SUBROUTINE READ2(X, NX, NY, UNIT, FNAME, OPE, CLOS)               MOD09080
C     READ IN A 2D, UNFORMATTED ARRAY, WITH EXTERNAL CONTROL ON         MOD09090
C       OPENING AND CLOSING.                                            MOD09100
                                                                        MOD09110
      INTEGER NX, NY, UNIT                                              MOD09120
      REAL X(NX, NY)                                                    MOD09130
      CHARACTER*60 FNAME                                                MOD09140
      LOGICAL OPE, CLOS                                                 MOD09150
                                                                        MOD09160
      IF (OPE) OPEN(UNIT, FILE=FNAME, FORM='UNFORMATTED', STATUS='OLD') MOD09170
      READ (UNIT) X                                                     MOD09180
      IF (CLOS) CLOSE(UNIT, STATUS='KEEP')                              MOD09190
                                                                        MOD09200
      RETURN                                                            MOD09210
      END                                                               MOD09220
      FUNCTION YES(DEFALT)                                              MOD09230
C     FUNCTION TO RETURN .TRUE. IF THE USER RESPONDS Y, .FALSE. IF HE   MOD09240
C       SAYS N, AND THE DEFAULT VALUE OTHERWISE.                        MOD09250
                                                                        MOD09260
      LOGICAL YES, DEFALT                                               MOD09270
      CHARACTER RESP                                                    MOD09280
                                                                        MOD09290
      READ (11,9001) RESP                                               MOD09300
 9001 FORMAT(A1)                                                        MOD09310
                                                                        MOD09320
      YES = (RESP.EQ.'Y') .OR. (DEFALT .AND. RESP.NE.'Y'                MOD09330
     1                                 .AND. RESP.NE.'N')               MOD09340
                                                                        MOD09350
      RETURN                                                            MOD09360
      END                                                               MOD09370
      SUBROUTINE ARSET (X, NX, NY, VALUE)                               MOD09380
C     SET ALL ELEMENTS OF ARRAY X EQUAL TO VALUE.                       MOD09390
                                                                        MOD09400
      INTEGER NX, NY                                                    MOD09410
      REAL X(NX, NY), VALUE                                             MOD09420
                                                                        MOD09430
      INTEGER I, J                                                      MOD09440
                                                                        MOD09450
      DO 1000 J = 1, NY                                                 MOD09460
        DO 1010 I = 1, NX                                               MOD09470
          X(I,J) = VALUE                                                MOD09480
 1010   CONTINUE                                                        MOD09490
 1000 CONTINUE                                                          MOD09500
                                                                        MOD09510
      RETURN                                                            MOD09520
      END                                                               MOD09530
      SUBROUTINE STEXT(UC, VC, UT, VT, WE, SS, SD, QS, QD, H,           MOD09540
     1                 NX, NY, DELX, DELY, DELT, DREF, SREF,            MOD09550
     2                 ASH, ASV, TSTEP)                                 MOD09560
C     SUBROUTINE TO EXTRAPOLATE THE SALINITY FIELD TO THE NEXT          MOD09570
C       TIME STEP.                                                      MOD09580
C     DEL(US) COMPUTED AS D(US)/DX + D(VS)/DY 3-30-88.                  MOD09590
                                                                        MOD09600
      INTEGER NX, NY, TSTEP                                             MOD09610
      REAL WE(NX, NY), UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)   MOD09620
      REAL QS(NX, NY), QD(NX, NY), SS(NX, NY), SD(NX, NY)               MOD09630
      REAL H(NX, NY)                                                    MOD09640
      REAL DELX, DELY, DELT, DREF, SREF, ASH, ASV                       MOD09650
                                                                        MOD09660
      INTEGER I, J, NNX, NNY                                            MOD09670
      PARAMETER (NNX = 36)                                              MOD09680
      PARAMETER (NNY = 36)                                              MOD09690
      REAL FSS(NNX, NNY), FSD(NNX, NNY)                                 MOD09700
      REAL DX2, DY2, DXTDX, DYTDY, HREF, PVDIF                          MOD09710
                                                                        MOD09720
      REAL PSI                                                          MOD09730
      INTEGER K, L                                                      MOD09740
      PSI(K,L) = 0.0                                                    MOD09750
CD    PSI(K,L) = SIGN(1., WE(K,L)                                       MOD09760
CD   1  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2) MOD09770
CD   2  +  UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2                              MOD09780
CD   3  +  VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2                              MOD09790
CD   4  -  UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2                              MOD09800
CD   5  -  VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2          )                   MOD09810
                                                                        MOD09820
      DX2 = DELX+DELX                                                   MOD09830
      DY2 = DELY+DELY                                                   MOD09840
      DXTDX = DELX*DELX                                                 MOD09850
      DYTDY = DELY*DELY                                                 MOD09860
      HREF  = H(NX/2, NY/2)                                             MOD09870
      PVDIF = 8.*ASV/HREF                                               MOD09880
                                                                        MOD09890
C     COMPUTE FORCING FOR THE INTERIOR POINTS:                          MOD09900
      DO 1000 J = 2, NY-1                                               MOD09910
        DO 1010 I = 2, NX-1                                             MOD09920
                                                                        MOD09930
          FSS(I,J) =                                                    MOD09940
     1     - (UT(I,J)*(SS(I+1,J)-SS(I-1,J))                             MOD09950
     2       +VT(I,J)*(SS(I,J+1)-SS(I,J-1))                             MOD09960
     3       +UC(I,J)*(SD(I+1,J)-SD(I-1,J))                             MOD09970
     4       +VC(I,J)*(SD(I,J+1)-SD(I,J-1))                             MOD09980
     5       +SD(I,J)*(4./3.)*(  (UC(I+1,J)-UC(I-1,J))                  MOD09990
     6                         + (VC(I,J+1)-VC(I,J-1)) )  )/DX2         MOD10000
     7     + (QS(I,J) - 2.*WE(I,J)*SD(I,J)) / HREF                      MOD10010
CT   8     - 2.*WE(I,J)*SD(I,J)/HREF                                    MOD10020
C          ADD DIFFUSIVE TERMS 3-30-88                                  MOD10030
CT   9     + ASH*( (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))/DXTDX               MOD10040
CT   1            +(SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))/DYTDY )             MOD10050
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.                     MOD10060
     2     + ( (ASH + UT(I,J)*UT(I,J)*DELT*0.5)                         MOD10070
     3           * (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))                     MOD10080
     4       + (ASH + VT(I,J)*VT(I,J)*DELT*0.5)                         MOD10090
     5           * (SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))  ) / DXTDX          MOD10100
                                                                        MOD10110
                                                                        MOD10120
          FSD(I,J) =                                                    MOD10130
     1     - (UT(I,J)*(SD(I+1,J)-SD(I-1,J))                             MOD10140
     2       +VT(I,J)*(SD(I,J+1)-SD(I,J-1))                             MOD10150
     3       +UC(I,J)*(SS(I+1,J)-SS(I-1,J))                             MOD10160
     4       +VC(I,J)*(SS(I,J+1)-SS(I,J-1))     ) / DX2                 MOD10170
     5     + (QD(I,J) - SD(I,J)*(PVDIF-WE(I,J)) ) / HREF                MOD10180
CT   6     - 8.*ASV*SD(I,J)/HREF/HREF                                   MOD10190
CT   9     - WE(I,J)*SD(I,J)/HREF                                       MOD10200
CT   7     + ASH*( (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))/DXTDX               MOD10210
CT   8            +(SD(I,J+1)-2.*SD(I,J)+SD(I,J-1))/DYTDY )             MOD10220
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.                     MOD10230
     2     + ((ASH + UT(I,J)*UT(I,J)*DELT*0.5 )                         MOD10240
     3           * (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))                     MOD10250
     4       +(ASH + VT(I,J)*VT(I,J)*DELT*0.5 )                         MOD10260
     5           * (SD(I,J+1)-2.*SD(I,J)+SD(I,J-1)) ) / DXTDX           MOD10270
                                                                        MOD10280
                                                                        MOD10290
 1010   CONTINUE                                                        MOD10300
 1000 CONTINUE                                                          MOD10310
                                                                        MOD10320
C     EXTRAPOLATE THE INTERIOR VALUES:                                  MOD10330
      DO 3000 J = 2, NY-1                                               MOD10340
        DO 3010 I = 2, NX-1                                             MOD10350
          SS(I,J) = SS(I,J) + DELT*FSS(I,J)                             MOD10360
          SD(I,J) = SD(I,J) + DELT*FSD(I,J)                             MOD10370
 3010   CONTINUE                                                        MOD10380
 3000 CONTINUE                                                          MOD10390
                                                                        MOD10400
C     NOW MUST APPLY THE BOUNDARY CONDITIONS.                           MOD10410
C     CONDITION FOR THE Y=0 BNDY, NO FLUX: S(X,1) = S(X,2)              MOD10420
C                     Y = YMAX  ,          S(X,NY) = S(X,NY-1)          MOD10430
C         X=0 BC CHANGED TO NO FLUX 5-26-88                             MOD10440
C         X = XMAX BC CHANGED TO ROBIN 5-26-88.                         MOD10450
C     BC:                                                               MOD10460
      DO 4000 I = 2, NX-1                                               MOD10470
        SS(I,1)  = SS(I,2)                                              MOD10480
        SD(I,1)  = SD(I,2)                                              MOD10490
        SS(I,NY) = SS(I,NY-1)                                           MOD10500
        SD(I,NY) = SD(I,NY-1)                                           MOD10510
 4000 CONTINUE                                                          MOD10520
      DO 4010 J = 2, NY-1                                               MOD10530
        SS(NX,J) = SS(NX-1,J)                                           MOD10540
        SD(NX,J) = SD(NX-1,J)                                           MOD10550
        SS(1,J)  = SS(2,J)                                              MOD10560
        SD(1,J)  = SD(2,J)                                              MOD10570
 4010 CONTINUE                                                          MOD10580
      SS(1,1)   = SS(2,2)                                               MOD10590
      SS(1,NY)  = SS(2,NY-1)                                            MOD10600
      SS(NX,1)  = SS(NX-1,2)                                            MOD10610
      SS(NX,NY) = SS(NX-1, NY-1)                                        MOD10620
      SD(1,1)   = SD(2,2)                                               MOD10630
      SD(1,NY)  = SD(2,NY-1)                                            MOD10640
      SD(NX,1)  = SD(NX-1,2)                                            MOD10650
      SD(NX,NY) = SD(NX-1, NY-1)                                        MOD10660
                                                                        MOD10670
      RETURN                                                            MOD10680
      END                                                               MOD10690
      SUBROUTINE QSEXT(QSS, QSD, H, WE, NX, NY, TSTEP, LOY,             MOD10700
     1                 XCEN, XLEN, YCEN, YLEN,                          MOD10710
     2                 QSFMAX, QSFREF, QSM, DELX, DELY,                 MOD10720
     3                 STRSPR, STRSUM, STRFLL, STRWIN            )      MOD10730
C     SUBROUTINE TO EXTRAPOLATE THE SALINIZATION FORCING TO THE NEXT    MOD10740
C       TIME STEP.  BG 3-25-88.                                         MOD10750
                                                                        MOD10760
      INTEGER NX, NY, TSTEP                                             MOD10770
      REAL QSS(NX, NY), QSD(NX, NY), H(NX, NY)
      REAL WE(NX, NY)                                                   MOD10780
      REAL DELX, DELY                                                   MOD10790
                                                                        MOD10800
      INTEGER I, J, T                                                   MOD10810
                                                                        MOD10820
      INTEGER XCEN, YCEN, XLEN, YLEN                                    MOD10830
      REAL QSFMAX, QSFREF                                               MOD10840
      REAL QSM                                                          MOD10850
      INTEGER STRSPR, STRSUM, STRFLL, STRWIN, LOY                       MOD10860
                                                                        MOD10870
      REAL PI                                                           MOD10880
      PARAMETER (PI = 3.141592654)                                      MOD10890
      REAL XREF, YREF, DX, DY, SIGX, SIGY                               MOD10900
      REAL A, SUMQ, SUMW                                                MOD10910
      REAL YTERM, XPART, QSSUM                                          MOD10920
      SAVE A                                                            MOD10930
                                                                        MOD10940
      T = MOD(TSTEP,LOY)                                                MOD10950
      XREF = FLOAT(XCEN)                                                MOD10960
      YREF = FLOAT(YCEN)                                                MOD10970
      SIGX = FLOAT(XLEN)                                                MOD10980
      SIGY = FLOAT(YLEN)                                                MOD10990
                                                                        MOD11000
      IF (T .EQ. 1) THEN                                                MOD11010
C       COMPUTE THE REQUIRED SLOPE FOR SALT CONSERVATION.               MOD11020
        SUMQ = 0.0 
        SUMW = 0.0                                                      MOD11030
        DO 100 J = 2, NY-1                                              MOD11040
          DY = DELY*FLOAT(J)                                            MOD11050
          DO 101 I = 2, NX-1                                            MOD11060
            DX = DELX*FLOAT(I)                                          MOD11070
            SUMQ = SUMQ + QSFREF + QSFMAX*                              MOD11080
     1      EXP((-1.)*(( DX-XREF )**2/2./SIGX**2                        MOD11090
     2                +( DY-YREF )**2/2./SIGY**2 ))  
            SUMW = SUMW + WE(I,J)                                       MOD11100
 101      CONTINUE                                                      MOD11110
 100    CONTINUE                                                        MOD11120
        SUMQ = SUMQ*FLOAT(STRSPR-STRWIN)/FLOAT(STRFLL-STRSUM)/          MOD11130
     1         FLOAT(NX*NY-2*NX-2*NY+4)                                 MOD11140
C       WARNING!! SUMW HAS THE MEAN STRATIFICATION AND LENGTH OF YEAR
C         HARD CODED  3-7-90.
        SUMW = SUMW*(-0.1*2.*FLOAT(LOY))/FLOAT(STRFLL-STRSUM)/
     1         FLOAT(NX*NY-2*NX-2*NY+4)                                 MOD11140
	SUMQ = SUMQ + SUMW + QSM                                        MOD11150
        A    = SUMQ*2./FLOAT(NY+1)/DELY                                 MOD11160
        PRINT *,'LINEAR MELTING PARAMETER = ',A                         MOD11170
      ENDIF                                                             MOD11180
                                                                        MOD11190
      XPART = 0.5/SIGX/SIGX                                             MOD11200
      IF ((T .GE. STRWIN) .AND. (T .LT. STRSPR)) THEN                   MOD11210
        DO 1000 J = 1, NY                                               MOD11220
          DY = DELY*FLOAT(J)                                            MOD11230
          YTERM = (DY - YREF)*(DY - YREF)*0.5/SIGY/SIGY                 MOD11240
          DO 1010 I = 1, NX                                             MOD11250
            DX = DELX*FLOAT(I)                                          MOD11260
            QSS(I,J) = QSFREF + QSFMAX*                                 MOD11270
     1        EXP( -(DX-XREF)*(DX-XREF)*XPART - YTERM )                 MOD11280
            QSD(I,J) = QSS(I,J)                                         MOD11290
 1010     CONTINUE                                                      MOD11300
 1000   CONTINUE                                                        MOD11310
       ELSEIF (T .GE. STRSPR .AND. T .LT. STRSUM) THEN                  MOD11320
C       SPRING, QSS = QSD = 0.0                                         MOD11330
        DO 1100 J = 1, NY                                               MOD11340
          DO 1110 I = 1, NX                                             MOD11350
            QSS(I,J) = 0.0                                              MOD11360
            QSD(I,J) = 0.0                                              MOD11370
 1110     CONTINUE                                                      MOD11380
 1100   CONTINUE                                                        MOD11390
       ELSEIF (T .GE. STRSUM .AND. T .LT. STRFLL) THEN                  MOD11400
        DO 2000 J = 1, NY                                               MOD11410
          QSSUM = QSM - A*FLOAT(J)*DELY                                 MOD11420
          DO 2010 I = 1, NX                                             MOD11430
            QSS(I,J) = QSSUM                                            MOD11440
            QSD(I,J) = QSSUM                                            MOD11450
 2010     CONTINUE                                                      MOD11460
 2000   CONTINUE                                                        MOD11470
       ELSE                                                             MOD11480
C       FALL, DO NOTHING                                                MOD11490
        DO 2100 J = 1, NY                                               MOD11500
          DO 2110 I = 1, NX                                             MOD11510
            QSS(I,J) = 0.0                                              MOD11520
            QSD(I,J) = 0.0                                              MOD11530
 2110     CONTINUE                                                      MOD11540
 2100   CONTINUE                                                        MOD11550
      ENDIF                                                             MOD11560
                                                                        MOD11570
      RETURN                                                            MOD11580
      END                                                               MOD11590
