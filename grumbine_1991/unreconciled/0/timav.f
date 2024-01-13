      SUBROUTINE TIMAV(UC, VC, UT, VT, SS, SD, H, SCRIT, TAV, TSTEP,
     1                     NX, NY, DELX, DELY)
C     PROGRAM TO COMPUTE AVERAGED QUANTITIES FROM MODEL OUTPUT
C     VERSION MODIFIED TO WORK 'ON THE FLY'.  ORIGINAL
C       ASSUMED THAT THE FILES WERE ALREADY WRITTEN AND MERELY
C       NEEDED AVERAGING.  NOW WORK WITH DATA AS IT IS BEING GENERATED.
C       BG 9-29-89
      INTEGER NX, NY, NNX, NNY
      PARAMETER (NNX = 36, NNY = 36)

      REAL UC(NX, NY), VC(NX, NY), SS(NX, NY), SD(NX, NY)
      REAL UT(NX, NY), VT(NX, NY), H(NX, NY)
      REAL SCRIT, DELX, DELY

      INTEGER NSCRIT
      PARAMETER (NSCRIT = 12)
      REAL DELSCRIT
      PARAMETER (DELSCRIT = 0.005)
      REAL FTAV
      INTEGER I, J, K, L
      INTEGER TSTEP, TAV
      CHARACTER*60 FNAME
      REAL R1(NNX, NNY), R2(NNX, NNY), R3(NNX, NNY), R4(NNX, NNY)
      REAL R5(NNX, NNY), R6(NNX, NNY)
      REAL F1(NNY,NSCRIT), F2(NNY,NSCRIT)
      REAL F1T(NNY,NSCRIT), F2T(NNY,NSCRIT)
      SAVE R1, R2, R3, R4, R5, R6, F1, F2, F1T, F2T

C***********************************************************----------!!

C     NUMBER OF STEPS TO AVERAGE OVER IS TAV.
      FTAV = FLOAT(TAV)
      IF (TSTEP .EQ. 1) THEN
        DO 1040 K = 1, NY
          DO 1050 L = 1, NX
            R1(L,K) = 0.0
            R2(L,K) = 0.0
            R3(L,K) = 0.0
            R4(L,K) = 0.0
            R5(L,K) = 0.0
            R6(L,K) = 0.0
 1050     CONTINUE
 1040   CONTINUE
        DO 1060 K = 1, NY
          DO 1070 L = 1, NSCRIT
            F1(K,L) = 0.0
            F2(K,L) = 0.0
 1070     CONTINUE
 1060   CONTINUE

       ELSE

        DO 1900 I = 1, NSCRIT
         CALL REFLUX(VT, VC, SS, SD, H, F1T(1,I), F2T(1,I),
     1                SCRIT+(I-1)*DELSCRIT, NX, NY, DELX, DELY)
          DO 1910 K = 1, NY
             F1(K,I) = F1(K,I)+F1T(K,I)
             F2(K,I) = F2(K,I)+F2T(K,I)
 1910     CONTINUE
 1900   CONTINUE

        DO 2000 K = 1, NY
          DO 2010 L = 1, NX
            R1(L,K) = R1(L,K) + UC(L,K)
            R2(L,K) = R2(L,K) + VC(L,K)
            R3(L,K) = R3(L,K) + UT(L,K)
            R4(L,K) = R4(L,K) + VT(L,K)
            R5(L,K) = R5(L,K) + SS(L,K)
            R6(L,K) = R6(L,K) + SD(L,K)
 2010     CONTINUE
 2000   CONTINUE

        IF ( MOD(TSTEP,TAV) .EQ. 0) THEN
          DO 2020 K = 1, NY
            DO 2030 L = 1, NX
              R1(L,K) = R1(L,K) / FTAV
              R2(L,K) = R2(L,K) / FTAV
              R3(L,K) = R3(L,K) / FTAV
              R4(L,K) = R4(L,K) / FTAV
              R5(L,K) = R5(L,K) / FTAV
              R6(L,K) = R6(L,K) / FTAV
 2030       CONTINUE
            DO 2040 I = 1, NSCRIT
       F1(K,I) = F1(K,I) / FTAV / 1.E5
              F2(K,I) = F2(K,I) / FTAV / 1.E5
 2040       CONTINUE
 2020     CONTINUE
          WRITE (50) R1
          WRITE (51) R2
          WRITE (52) R3
          WRITE (53) R4
          WRITE (54) R5
          WRITE (55) R6
          DO 2100 J = 1, NSCRIT
           WRITE (56,9009) (F1(I,J),I=1,NY)
           WRITE (57,9009) (F2(I,J),I=1,NY)
 2100   CONTINUE
         WRITE (56,9010)
          WRITE (57,9010)
          DO 2060 K = 1, NY
            DO 2050 L = 1, NX
              R1(L,K) = 0.0
              R2(L,K) = 0.0
              R3(L,K) = 0.0
              R4(L,K) = 0.0
              R5(L,K) = 0.0
              R6(L,K) = 0.0
 2050       CONTINUE
      DO 2070 I = 1, NSCRIT
              F1(K,I) = 0.0
              F2(K,I) = 0.0
 2070       CONTINUE
 2060     CONTINUE
        ENDIF

      ENDIF

 9001 FORMAT (BN, I5)

 9002 FORMAT (A60)

 9009 FORMAT (16F5.2)

 9010 FORMAT (' END OF STEP')

      RETURN
      END
