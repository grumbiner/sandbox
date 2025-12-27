C
C  PROGT2 IS THE SECOND PART OF THE SOIL MODEL THAT IS EXECUTED
C  AFTER PRECIPITATION FOR THE TIME STEP HAS BEEN CALCULATED
C
CFPP$ NOCONCUR R
CFPP$ EXPAND(FUNCDF,FUNCKT,THSAT)
      SUBROUTINE PROGT2(IM,KM,RHSCNPY,RHSMC,AI,BI,CI,SMC,SLIMSK,
     &       CANOPY,PRECIP,RUNOFF,SNOWMT,
     &       ZSOIL,SOILTYP,SIGMAF,DELT,LAT)
      PARAMETER (SCANOP=2.,RHOH2O=1000.)
      PARAMETER (CTFIL1=.5,CTFIL2=1.-CTFIL1)
      PARAMETER (RFFACT=.15)
      DIMENSION RHSCNPY(IM),RHSMC(IM,KM)
      DIMENSION AI(IM,KM),BI(IM,KM),CI(IM,KM)
      DIMENSION SMC(IM,KM),CANOPY(IM),PRECIP(IM),SOILTYP(IM)
      DIMENSION SIGMAF(IM),SLIMSK(IM),RUNOFF(IM)
      DIMENSION ZSOIL(IM,KM),DEW(IM),SNOWMT(IM)
      INTEGER SOILTYP
      REAL INF, INFMAX, KSAT
C     PARAMETER (LM= 384 ,MM= 2 )
      LOGICAL FLAG(IM)
C
      DIMENSION PRCP(IM),INF(IM),INFMAX(IM)
      DIMENSION TSAT(IM),DSAT(IM),KSAT(IM)
      DIMENSION SMSOIL(IM,KM),CNPY(IM)
      LATD = 44
      LOND = 353
      DELT2 = DELT * 2.
C
C  PRECIPITATION RATE IS NEEDED IN UNIT OF KG M-2 S-1
C
      DO I = 1, IM
        PRCP(I) = RHOH2O * (PRECIP(I)+SNOWMT(I)) / DELT
        RUNOFF(I) = 0.
        CNPY(I) = CANOPY(I)
      ENDDO
C      IF(LAT.EQ.LATD) THEN
C        I = LOND
C        PRINT *, ' BEFORE RUNOFF CAL, RHSMC =', RHSMC(I,1)
C      ENDIF
C
C  UPDATE CANOPY WATER CONTENT
C
      DO I = 1, IM
        IF(SLIMSK(I).EQ.1.) THEN
          RHSCNPY(I) = RHSCNPY(I) + SIGMAF(I) * PRCP(I)
          CANOPY(I) = CANOPY(I) + DELT2 * RHSCNPY(I)
          CANOPY(I) = MAX(CANOPY(I),0.)
          PRCP(I) = PRCP(I) * (1. - SIGMAF(I))
          IF(CANOPY(I).GT.SCANOP) THEN
            DRIP = CANOPY(I) - SCANOP
            CANOPY(I) = SCANOP
            PRCP(I) = PRCP(I) + DRIP / DELT2
          ENDIF
C
C  CALCULATE INFILTRATION RATE
C
          INF(I) = PRCP(I)
          TSAT(I) = THSAT(SOILTYP(I))
C         DSAT(I) = FUNCDF(TSAT(I),SOILTYP(I))
C         KSAT(I) = FUNCKT(TSAT(I),SOILTYP(I))
C         INFMAX(I) = -DSAT(I) * (TSAT(I) - SMC(I,1))
C    &                / (.5 * ZSOIL(I,1))
C    &                + KSAT(I)
          INFMAX(I) = (-ZSOIL(I,1)) *
     &                ((TSAT(I) - SMC(I,1)) / DELT2 - RHSMC(I,1))
     &                * RHOH2O
          INFMAX(I) = MAX(RFFACT*INFMAX(I),0.)
C         IF(SMC(I,1).GE.TSAT(I)) INFMAX(I) = KSAT(I)
C         IF(SMC(I,1).GE.TSAT(I)) INFMAX(I) = ZSOIL(I,1) * RHSMC(I,1)
          IF(INF(I).GT.INFMAX(I)) THEN
            RUNOFF(I) = INF(I) - INFMAX(I)
            INF(I) = INFMAX(I)
          ENDIF
          INF(I) = INF(I) / RHOH2O
          RHSMC(I,1) = RHSMC(I,1) - INF(I) / ZSOIL(I,1)
        ENDIF
      ENDDO
C      IF(LAT.EQ.LATD) THEN
C        I = LOND
C        PRINT *, ' PRCP, INFMAX, RUNOFF =', PRCP(I),INFMAX(I),RUNOFF(I)
C        PRINT *, ' SMSOIL =', SMC(I,1), SMC(I,2)
C        PRINT *, ' RHSMC =', RHSMC(I,1)
C      ENDIF
C
C  WE CURRENTLY IGNORE THE EFFECT OF RAIN ON SEA ICE
C
      DO I = 1, IM
        FLAG(I) = SLIMSK(I).EQ.1.
      ENDDO
C
C  SOLVE THE TRI-DIAGONAL MATRIX
C
      DO K = 1, KM
        DO I = 1, IM
          IF(FLAG(I))  THEN
            RHSMC(I,K) = RHSMC(I,K) * DELT2
            AI(I,K) = AI(I,K) * DELT2
            BI(I,K) = 1. + BI(I,K) * DELT2
            CI(I,K) = CI(I,K) * DELT2
          ENDIF
        ENDDO
      ENDDO
C  FORWARD ELIMINATION
      DO I = 1, IM
        IF(FLAG(I)) THEN
          CI(I,1) = -CI(I,1) / BI(I,1)
          RHSMC(I,1) = RHSMC(I,1) / BI(I,1)
        ENDIF
      ENDDO
      DO K = 2, KM
        DO I = 1, IM
          IF(FLAG(I)) THEN
            CC = 1. / (BI(I,K) + AI(I,K) * CI(I,K-1))
            CI(I,K) = -CI(I,K) * CC
            RHSMC(I,K) = (RHSMC(I,K) - AI(I,K) * RHSMC(I,K-1)) * CC
          ENDIF
        ENDDO
      ENDDO
C  BACKWARD SUBSTITUTTION
      DO I = 1, IM
        IF(FLAG(I)) THEN
          CI(I,KM) = RHSMC(I,KM)
        ENDIF
      ENDDO
      DO K = KM-1, 1
        DO I = 1, IM
          IF(FLAG(I)) THEN
            CI(I,K) = CI(I,K) * CI(I,K+1) + RHSMC(I,K)
          ENDIF
        ENDDO
      ENDDO
 100  CONTINUE
C
C  UPDATE SOIL MOISTURE
C
      DO K = 1, KM
        DO I = 1, IM
          IF(FLAG(I)) THEN
            SMSOIL(I,K) = SMC(I,K) + CI(I,K)
            SMSOIL(I,K) = MAX(SMSOIL(I,K),0.)
            TDIF = MAX(SMSOIL(I,K) - TSAT(I),0.)
            RUNOFF(I) = RUNOFF(I) -
     &                RHOH2O * TDIF * ZSOIL(I,K) / DELT2
            SMSOIL(I,K) = SMSOIL(I,K) - TDIF
          ENDIF
        ENDDO
      ENDDO
      DO K = 1, KM
        DO I = 1, IM
          IF(FLAG(I)) THEN
            SMC(I,K) = CTFIL1 * SMSOIL(I,K) + CTFIL2 * SMC(I,K)
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(FLAG(I)) THEN
          CANOPY(I) = CTFIL1 * CANOPY(I) + CTFIL2 * CNPY(I)
        ENDIF
      ENDDO
C     I = 1
C     PRINT *, ' SMC'
C     PRINT 6000, SMC(I,1), SMC(I,2)
 6000 FORMAT(2(F8.5,','))
      RETURN
      END
