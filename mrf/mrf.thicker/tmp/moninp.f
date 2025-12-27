CFPP$ NOCONCUR R
      SUBROUTINE MONINP(IM,IM2,KM,A,B,TAU,RTG,
     1     U1,V1,T1,Q1,
     2     PSTAR,RBSOIL,CD,CH,FM,FH,TSEA,QSS,DPHI,DLAM,
     3     SI,DEL,SL,SLK,RCL,DELTIM,LAT,KDT,THOUR,
     4     DUSFC,DVSFC,DTSFC,DQSFC,HPBL,HGAMT,HGAMQ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MONINp      COMPUTES VERTICAL DIFFUSION TERMS.
C   PRGMMRS: MRB PERSONNEL   ORG: W/NMC23    DATE: 88-04-29
C
C ABSTRACT: SR MONINp COMPUTES THE PARAMETERIZED EFFECTS
C           OF VERTICAL TURBULENT EDDY DIFFUSION OF MOMENTUM
C           WATER VAPOR AND SENSIBLE HEAT IN THE MRF MODEL.
C           USES STABILITY DEPENDENT MIXING COEFFICIENTS
C           SUGGESTED BY P. LONG AFTER ECMWF METHODS.
C           INCORPORATES LOWER BOUNDARY FLUXES USING DRAG
C           COEFFICIENTS BASED ON MONIN OBUKHOV FORMULAE WHICH
C           ARE CALCULATED IN SR PROGTN.  SPLIT IMPLICIT
C           INTEGRATION APPROXIMATIONS ARE USED FOLLOWING THE
C           GFDL TECHNIQUE.  THE SOLUTIONS OBTAINED ARE FOR THE
C           TIME TENDENCIES OF U V T AND Q, AND (BEFORE RETURN)
C           THESE ARE ADDED TO THE TENDENCIES PASSED.
C           ALSO RETURNED IS A QUANTITY
C           DLAM REQUIRED FOR SURFACE HYDROLOGY COMPUTATIONS.
C
C PROGRAM HISTORY LOG:
C   88-04-29  HUA-LU PAN
C   88-10-28  SELA
C   92-09-01  IREDELL
C
C USAGE:    CALL MONINP(IDIMT,IDIMT2,KDIM,A,B,TAU,RTG,
C    1           U1,V1,T1,Q1,TOV,
C    2           CD,PSTAR,CDQ,QSS,DPHI,DLAM,
C    3           SI,DEL,CL,SL,RCL,DELTIM,TSEA,LAT,KDT,THOUR,
C    4           DUSFC,DVSFC,DTSFC,DQSFC)
C   INPUT ARGUMENT LIST:
C     IDIMT    - NUMBER OF PROFILES TO COMPUTE
C     IDIMT2   - FIRST DIMENSION OF FIELD SLICES
C     KDIM     - NUMBER OF VERTICAL LEVELS
C     A        - (IDIMT,KDIM) NEGATIVE TENDENCY FOR V WIND IN M/S/S
C     B        - (IDIMT,KDIM) TENDENCY FOR U WIND IN M/S/S
C     TAU      - (IDIMT,KDIM) TENDENCY FOR TEMPERATURE IN K/S
C     RTG      - (IDIMT,KDIM) TENDENCY FOR SPECIFIC HUMIDITY IN KG/KG/S
C     U1       - (IDIMT2,KDIM) ZONAL WIND * COS(LAT) IN M/S
C     V1       - (IDIMT2,KDIM) MERID WIND * COS(LAT) IN M/S
C     T1       - (IDIMT2,KDIM) TEMPERATURE IN K
C     Q1       - (IDIMT2,KDIM) SPECIFIC HUMIDITY IN KG/KG
C     TOV      - (KDIM) BASE TEMPERATURE IN K
C     CD       - (IDIMT) 1/(FM*FM) MOMENTUM EXCHANGE COEFFICIENT
C     PSTAR    - (IDIMT) SURFACE PRESSURE IN KPA
C     CD       - (IDIMT) 1/(FM*FM) HEAT & MOISTURE EXCHANGE COEFFICIENT
C     QSS      - (IDIMT) SPECIFIC HUMIDITY AT SURFACE IN KG/KG
C     DPHI     - (IDIMT) COEFFICIENT MODULATING SFC EVAPORATION
C     SI       - (KDIM+1) P/PSFC AT BASE OF LAYER
C     DEL      - (KDIM) POSITIVE INCREMENT OF P/PSFC ACROSS LAYER
C     CL       - (KDIM) =1-SL
C     SL       - (KDIM) P/PSFC AT MIDDLE OF LAYER
C     RCL      - RECIPROCAL OF SQUARE OF COS(LAT)
C     DELTIM   - TIME STEP IN SECS
C     TSEA     - (IDIMT) SURFACE TEMPERATURE IN K
C     LAT      - LATITUDE NUMBER
C     KDT      - TIMESTEP NUMBER
C     THOUR    - FORECAST HOUR
C   OUTPUT ARGUMENT LIST:
C     A        - (IDIMT,KDIM) NEGATIVE TENDENCY FOR V WIND IN M/S/S
C     B        - (IDIMT,KDIM) TENDENCY FOR U WIND IN M/S/S
C     TAU      - (IDIMT,KDIM) TENDENCY FOR TEMPERATURE IN K/S
C     RTG      - (IDIMT,KDIM) TENDENCY FOR SPECIFIC HUMIDITY IN KG/KG/S
C     DUSFC    - (IDIMT) ZONAL STRESS ON SURFACE IN N/M**2
C     DVSFC    - (IDIMT) MERID STRESS ON SURFACE IN N/M**2
C     DTSFC    - (IDIMT) SENSIBLE HEAT FLUX ON SURFACE IN W/M**2
C     DQSFC    - (IDIMT) LATENT HEAT FLUX ON SURFACE IN W/M**2
C
C SUBPROGRAMS CALLED:
C     TRIDI2   - SOLVE TRIDIAGONAL MATRIX EQUATION
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      PARAMETER(CP= 1.0046E+3 ,G= 9.8000E+0 ,RD= 2.8705E+2 ,RV= 4.6150E+
     12 ,HVAP= 2.5000E+6 )
      PARAMETER(GOR=G/RD,GOCP=G/CP,ROCP=RD/CP,FV=RV/RD-1)
      PARAMETER(CONT=1000.*CP/G,CONQ=1000.*HVAP/G,CONW=1000./G)
      PARAMETER(RLAM=30.,VK=0.4,VK2=VK*VK,PRMIN=0.5,PRMAX=2.)
      PARAMETER(DW2MIN=0.0001,DKMIN=1.0,DKMAX=1000.,RIMIN=-100.)
      PARAMETER(RBCR=0.5,CFAC=7.8,PFAC=2.0,SFCFRAC=0.1)
      PARAMETER(QMIN=1.E-8,XKZO=1.,ZFMIN=1.E-8,APHI5=5.,APHI16=16.)
      PARAMETER(GAMCRT=3.,GAMCRQ=GAMCRT/200.)
      PARAMETER(IUN=84)
      DIMENSION DUSFC(IM),DVSFC(IM),DTSFC(IM),DQSFC(IM)
      DIMENSION A(IM,KM),B(IM,KM),TAU(IM,KM),RTG(IM,KM),
     1     U1(IM2,KM),V1(IM2,KM),T1(IM2,KM),Q1(IM2,KM),
     2     Z0M(IM),Z0H(IM),TSEA(IM),QSS(IM),PSTAR(IM),DPHI(IM),DLAM(IM),
     3     SI(KM+1),DEL(KM),CL(KM),SL(KM),SLK(KM),
     4     FM(IM),FH(IM),RBSOIL(IM),CD(IM),CH(IM)
      DIMENSION DZOT(KM-1),RDZT(KM-1),
     1     BETAW(IM),BETAQ(IM),BETAT(IM),ZI(IM,KM+1),ZL(IM,KM),ZL1(IM),
     2     DKU(IM,KM-1),DKT(IM,KM-1),
     3     AL(IM,KM-1),AD(IM,KM),AU(IM,KM-1),
     4     A1(IM,KM),A2(IM,KM),
     5     WSCALE(IM),HGAMT(IM),HGAMQ(IM),KPBL(IM),HPBL(IM),
     6     USTAR(IM),SPD1(IM),THE1V(IM),THERMAL(IM),RBDN(IM),RBUP(IM),
     7     HEAT(IM),EVAP(IM),THESV(IM),THE1(IM),WSTAR(IM),
     8     PHIM(IM),PHIH(IM)
      LOGICAL PBLFLG(IM),SFCFLG(IM),STABLE(IM)
C
C-----------------------------------------------------------------------
C
 601  FORMAT(1X,' MONINP LAT LON STEP HOUR ',3I6,F6.1)
 602      FORMAT(1X,'    K','        Z','        T','       TH',
     1     '      TVH','        Q','        U','        V',
     2     '       SP')
 603      FORMAT(1X,I5,8F9.1)
 604      FORMAT(1X,'  SFC',9X,F9.1,18X,F9.1)
 605      FORMAT(1X,'    K      ZL    SPD2   THEKV   THE1V'
     1         ,' THERMAL    RBUP')
 606      FORMAT(1X,I5,6F8.2)
 607      FORMAT(1X,' KPBL    HPBL      FM      FH   HGAMT',
     1         '   HGAMQ      WS   USTAR      CD      CH')
 608      FORMAT(1X,I5,9F8.2)
 609      FORMAT(1X,' K PR DKT DKU ',I5,3F8.2)
 610      FORMAT(1X,' K PR DKT DKU ',I5,3F8.2,' L2 RI T2',
     1         ' SR2  ',2F8.2,2E10.2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     COMPUTE PRELIMINARY VARIABLES
C
CCC   IF(IPRT.EQ.1) THEN
CCC   LATD = 48
CCC   LOND = 78
CCC   ELSE
CCC   LATD = 0
CCC         LOND = 0
CCC   ENDIF
CCC   IF(LAT.EQ.LATD) WRITE(IUN,601) LATD,LOND,KDT,THOUR
C
      DT    = 4. * DELTIM
      RDT   = 1. / DT
      RDZT1 = GOR * SL(1) / DEL(1)
      KMPBL = KM / 2
C
      DO K = 1,KM-1
         RDZT(K) = GOR * SI(K+1) / (SL(K) - SL(K+1))
         DZOT(K) = LOG(SI(K+1) / SI(K)) / GOR
      ENDDO
C
      DO I = 1,IM
         ZI(I,1) = 0.
      ENDDO
      DO K = 1, KM - 1
         DO I = 1, IM
            ZI(I,K+1) = ZI(I,K) - T1(I,K) * DZOT(K)
         ENDDO
      ENDDO
C
CCC   IF(LAT.EQ.LATD) THEN
CCC            I = LOND
CCC   WRITE(IUN,602)
CCC   DO K = KM-1,1,-1
CCC   ZLKP1 = (ZI(I,K) + ZI(I,K+1))/2.
CCC               THETA = T1(I,K)/SLK(K)
CCC   THETAV = T1(I,K)/SLK(K)*(1.+FV*MAX(Q1(I,K),QMIN))
CCC   SPEED = SQRT(MAX(RCL*(U1(I,K)**2+V1(I,K)**2),1.))
CCC   WRITE(IUN,603) K,ZLKP1,T1(I,K),THETA,THETAV,
CCC   1             Q1(I,K)*1000.,U1(I,K),V1(I,K),SPEED
CCC            ENDDO
CCC   WRITE(IUN,604) TSEA(I),QSS(I)*1000.
CCC   ENDIF
C
      DO I = 1,IM
         DUSFC(I) = 0.
         DVSFC(I) = 0.
         DTSFC(I) = 0.
         DQSFC(I) = 0.
         HGAMT(I) = 0.
         HGAMQ(I) = 0.
         WSCALE(I) = 0.
         KPBL(I) = 1
         HPBL(I) = ZI(I,2)
         PBLFLG(I) = .TRUE.
         SFCFLG(I) = .TRUE.
         IF(RBSOIL(I).GT.0.0) SFCFLG(I) = .FALSE.
      ENDDO
C
      DO I = 1,IM
         SPD1(I) = MAX(SQRT(RCL*(U1(I,1)**2+V1(I,1)**2)),1.)
         BET1 = DT*RDZT1*SPD1(I)/T1(I,1)
         BETAW(I) = BET1*CD(I)
         BETAT(I) = BET1*CH(I)
         BETAQ(I) = DPHI(I)*BETAT(I)
         DLAM(I) = RDT*BETAT(I)
      ENDDO
C
      DO I = 1,IM
         ZL1(I) = 0.-(T1(I,1)+TSEA(I))/2.*LOG(SL(1))/GOR
         USTAR(I) = SQRT(CD(I)*SPD1(I)**2)
      ENDDO
C
      DO I = 1,IM
         THESV(I)  = TSEA(I)*(1.+FV*MAX(QSS(I),QMIN))
         THE1(I)   = T1(I,1)/SLK(1)
         THE1V(I) = THE1(I)*(1.+FV*MAX(Q1(I,1),QMIN))
         DTHE1  = (THE1(I)-TSEA(I))
         DQ1    = (MAX(Q1(I,1),QMIN) - MAX(QSS(I),QMIN))
         HEAT(I) = -CH(I)*SPD1(I)*DTHE1
         EVAP(I) = -CH(I)*SPD1(I)*DQ1
      ENDDO
C
C
C     COMPUTE THE FIRST GUESS OF PBL HEIGHT
C
      DO I = 1, IM
         STABLE(I) = .FALSE.
         ZL(I,1) = ZL1(I)
         RBUP(I) = RBSOIL(I)
      ENDDO
      DO K = 2, KMPBL
         DO I = 1, IM
            IF(.NOT.STABLE(I)) THEN
               RBDN(I) = RBUP(I)
               ZL(I,K) = ZL(I,K-1) - (T1(I,K)+T1(I,K-1))/2 *
     &              LOG(SL(K)/SL(K-1)) / GOR
               THEKV = T1(I,K)/SLK(K)*(1.+FV*MAX(Q1(I,K),QMIN))
               SPDK2 = MAX(RCL*(U1(I,K)**2+V1(I,K)**2),1.)
               RBUP(I) = (THEKV-THE1V(I))*(G*ZL(I,K)/THE1V(I))/SPDK2
               KPBL(I) = K
               STABLE(I) = RBUP(I).GT.RBCR
            ENDIF
         ENDDO
      ENDDO
C
      DO I = 1,IM
         K = KPBL(I)
         if(rbdn(i).ge.rbcr) then
            rbint = 0.
         elseif(rbup(i).le.rbcr) then
            rbint = 1.
         else
            rbint = (RBCR-RBDN(I))/(RBUP(I)-RBDN(I))
         endif
         HPBL(I) = ZL(I,K-1) + rbint*(ZL(I,K)-ZL(I,K-1))
         IF(HPBL(I).LT.ZI(I,KPBL(I))) KPBL(I) = KPBL(I) - 1
      ENDDO
C
      DO I = 1, IM
         HOL = MAX(RBSOIL(I)*FM(I)*FM(I)/FH(I),RIMIN)
         IF(SFCFLG(I)) THEN
            HOL = MIN(HOL,-ZFMIN)
         ELSE
            HOL = MAX(HOL,ZFMIN)
         ENDIF
C
         HOL = HOL*HPBL(I)/ZL1(I)*SFCFRAC
         IF(SFCFLG(I)) THEN
            PHIM(I) = (1.-APHI16*HOL)**(-1./4.)
            PHIH(I) = (1.-APHI16*HOL)**(-1./2.)
         ELSE
            PHIM(I) = (1.+APHI5*HOL)
            PHIH(I) = PHIM(I)
         ENDIF
         WSCALE(I) = USTAR(I)/PHIM(I)
         WSCALE(I) = MIN(WSCALE(I),30.*USTAR(I))
         WSCALE(I) = MAX(WSCALE(I),USTAR(I)/30.)
      ENDDO
C
C     COMPUTE THE SURFACE VARIABLES FOR PBL HEIGHT ESTIMATION
C     UNDER UNSTABLE CONDITIONS
C
      DO I = 1,IM
         thermal(i) = the1v(i)
         hgamt(i) = 0.
         hgamq(i) = 0.
         IF(SFCFLG(I)) THEN
           DVTHE1 = (THE1V(I)-THESV(I))
           SFLUX  = HEAT(I) + EVAP(I)*FV*THE1(I)
           RBSFC  = G*ZL1(I)*(DVTHE1)/THESV(I)/SPD1(I)**2
           RBDN(I)   = RBSFC
           HGAMT(I) = min(CFAC*HEAT(I)/WSCALE(I),gamcrt)
           HGAMQ(I) = min(CFAC*EVAP(I)/WSCALE(I),gamcrq)
           VPERT = HGAMT(I) + FV*THE1(I)*HGAMQ(I)
           vpert = min(vpert,gamcrt)
           thermal(i) = thermal(i) + max(vpert,0.)
           IF(VPERT.LE.0.0) PBLFLG(I) = .FALSE.
           HGAMT(I) = MAX(HGAMT(I),0.0)
           HGAMQ(I) = MAX(HGAMQ(I),0.0)
         endif
      ENDDO
C
      DO I = 1,IM
         IF(PBLFLG(I)) THEN
            KPBL(I) = 1
            HPBL(I) = ZI(I,2)
         ENDIF
      ENDDO
C
C     ENHANCE THE PBL HEIGHT BY CONSIDERING THE THERMAL
C
      DO I = 1, IM
         IF(PBLFLG(I)) THEN
            STABLE(I) = .FALSE.
            RBUP(I) = RBSOIL(I)
         ENDIF
      ENDDO
      DO K = 2, KMPBL
         DO I = 1, IM
            IF(.NOT.STABLE(I).AND.PBLFLG(I)) THEN
               RBDN(I) = RBUP(I)
               ZL(I,K) = ZL(I,K-1) - (T1(I,K)+T1(I,K-1))/2 *
     &              LOG(SL(K)/SL(K-1)) / GOR
               THEKV = T1(I,K)/SLK(K)*(1.+FV*MAX(Q1(I,K),QMIN))
               SPDK2 = MAX(RCL*(U1(I,K)**2+V1(I,K)**2),1.)
               RBUP(I) = (THEKV-THERMAL(I))*(G*ZL(I,K)/THE1V(I))/SPDK2
               KPBL(I) = K
               STABLE(I) = RBUP(I).GT.RBCR
            ENDIF
         ENDDO
      ENDDO
C
      DO I = 1,IM
         IF(PBLFLG(I)) THEN
            K = KPBL(I)
            if(rbdn(i).ge.rbcr) then
               rbint = 0.
            elseif(rbup(i).le.rbcr) then
               rbint = 1.
            else
               rbint = (RBCR-RBDN(I))/(RBUP(I)-RBDN(I))
            endif
            HPBL(I) = ZL(I,K-1) + rbint*(ZL(I,K)-ZL(I,K-1))
            IF(HPBL(I).LT.ZI(I,KPBL(I))) KPBL(I) = KPBL(I) - 1
            IF(KPBL(I).LE.1) PBLFLG(I) = .FALSE.
         ENDIF
      ENDDO
C
C     COMPUTE DIFFUSION COEFFICIENTS BELOW PBL
C
      DO K = 1, KMPBL
         DO I = 1, IM
            IF(KPBL(I).GT.K) THEN
               PRNUM = (PHIH(I)/PHIM(I)+CFAC*VK*.1)
               PRNUM = MIN(PRNUM,PRMAX)
               PRNUM = MAX(PRNUM,PRMIN)
               ZFAC = MAX((1.-(ZI(I,K+1)-ZL1(I))/
     1              (HPBL(I)-ZL1(I))), ZFMIN)
               DKU(I,K) = XKZO + WSCALE(I)*VK*ZI(I,K+1)
     1              *ZFAC**PFAC
               DKT(I,K) = DKU(I,K)/PRNUM
               DKU(I,K) = MIN(DKU(I,K),DKMAX)
               DKU(I,K) = MAX(DKU(I,K),DKMIN)
               DKT(I,K) = MIN(DKT(I,K),DKMAX)
               DKT(I,K) = MAX(DKT(I,K),DKMIN)
            ENDIF
         ENDDO
      ENDDO
C
C     COMPUTE DIFFUSION COEFFICIENTS OVER PBL (FREE ATMOSPHERE)
C
      DO K = 1, KM-1
         DO I = 1, IM
            IF(K.GE.KPBL(I)) THEN
               TI   =0.5*(T1(I,K)+T1(I,K+1))
               RDZ  =RDZT(K)/TI
               DW2  =RCL*((U1(I,K)-U1(I,K+1))**2+(V1(I,K)-V1(I,K+1))**2)
               SHR2 =MAX(DW2,DW2MIN)*RDZ**2
               TVD  =T1(I,K)*(1.+FV*MAX(Q1(I,K),QMIN))
               TVU  =T1(I,K+1)*(1.+FV*MAX(Q1(I,K+1),QMIN))
               BVF2 =G*(GOCP+RDZ*(TVU-TVD))/TI
               RI   =MAX(BVF2/SHR2,RIMIN)
               ZK   =VK*ZI(I,K+1)
               RL2  =(ZK*RLAM/(RLAM+ZK))**2
               DK   =RL2*SQRT(SHR2)
               IF(RI.LT.0.) THEN ! UNSTABLE REGIME
                  DKU(I,K) = XKZO + DK*(1.-APHI16*RI)**(1./4.)
                  DKT(I,K) = XKZO + DK*(1.-APHI16*RI)**(1./2.)
               ELSE             ! STABLE REGIME
                  DKT(I,K)  = XKZO + DK*(EXP(-8.5*RI)+0.15/(RI+3.0))
                  PRNUM     = 1.5 + 3.08*RI
                  PRNUM     = MIN(PRNUM,PRMAX)
                  DKU(I,K)  = (DKT(I,K)-XKZO)*PRNUM + XKZO
               ENDIF
C
               DKU(I,K) = MIN(DKU(I,K),DKMAX)
               DKU(I,K) = MAX(DKU(I,K),DKMIN)
               DKT(I,K) = MIN(DKT(I,K),DKMAX)
               DKT(I,K) = MAX(DKT(I,K),DKMIN)
C
CCC   IF(I.EQ.LOND.AND.LAT.EQ.LATD) THEN
CCC   PRNUM = DKU(I,K)/DKT(I,K)
CCC   WRITE(IUN,610) K,PRNUM,DKT(I,K),DKU(I,K),RL2,RI,
CCC   1              BVF2,SHR2
CCC   ENDIF
C
            ENDIF
         ENDDO
      ENDDO
C
C     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR HEAT AND MOISTURE
C
      DO I=1,IM
         AD(I,1) = 1.
         A1(I,1) = T1(I,1)-BETAT(I)*(T1(I,1)/SLK(1)-TSEA(I))
         A2(I,1) = Q1(I,1)-BETAQ(I)*(MAX(Q1(I,1),QMIN)-MAX(QSS(I),QMIN))
      ENDDO
C
      DO K = 1,KM-1
         DTODSD = DT/DEL(K)
         DTODSU = DT/DEL(K+1)
         DSIG   = SL(K)-SL(K+1)
         DO I = 1,IM
            RDZ = RDZT(K)*2./(T1(I,K)+T1(I,K+1))
            IF(PBLFLG(I).AND.K.LT.KPBL(I)) THEN
               DSDZT = DSIG*DKT(I,K)*RDZ*(GOCP-HGAMT(I)/HPBL(I))
               DSDZQ = DSIG*DKT(I,K)*RDZ*(-HGAMQ(I)/HPBL(I))
               A2(I,K)   = A2(I,K)+DTODSD*DSDZQ
               A2(I,K+1) = Q1(I,K+1)-DTODSU*DSDZQ
            ELSE
               DSDZT = DSIG*DKT(I,K)*RDZ*(GOCP)
               A2(I,K+1) = Q1(I,K+1)
            ENDIF
            DSDZ2 = DSIG*DKT(I,K)*RDZ*RDZ
            AU(I,K)   = -DTODSD*DSDZ2
            AL(I,K)   = -DTODSU*DSDZ2
            AD(I,K)   = AD(I,K)-AU(I,K)
            AD(I,K+1) = 1.-AL(I,K)
            A1(I,K)   = A1(I,K)+DTODSD*DSDZT
            A1(I,K+1) = T1(I,K+1)-DTODSU*DSDZT
         ENDDO
      ENDDO
C
C     SOLVE TRIDIAGONAL PROBLEM FOR HEAT AND MOISTURE
C
      CALL TRIDI2(IM,KM,AL,AD,AU,A1,A2,AU,A1,A2)
C
C     RECOVER TENDENCIES OF HEAT AND MOISTURE
C
      DO  K = 1,KM
         DO I = 1,IM
            TTEND = (A1(I,K)-T1(I,K))*RDT
            QTEND = (A2(I,K)-Q1(I,K))*RDT
            TAU(I,K) = TAU(I,K)+TTEND
            RTG(I,K) = RTG(I,K)+QTEND
            DTSFC(I) = DTSFC(I)+CONT*DEL(K)*PSTAR(I)*TTEND
            DQSFC(I) = DQSFC(I)+CONQ*DEL(K)*PSTAR(I)*QTEND
         ENDDO
      ENDDO
C
C     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR MOMENTUM
C
      DO I = 1,IM
         AD(I,1) = 1.+BETAW(I)
         A1(I,1) = U1(I,1)
         A2(I,1) = V1(I,1)
      ENDDO
C
      DO K = 1,KM-1
         DTODSD = DT/DEL(K)
         DTODSU = DT/DEL(K+1)
         DSIG   = SL(K)-SL(K+1)
         DO I=1,IM
            RDZ   = RDZT(K)*2./(T1(I,K)+T1(I,K+1))
            DSDZ2 = DSIG*DKU(I,K)*RDZ*RDZ
            AU(I,K)  = -DTODSD*DSDZ2
            AL(I,K)  = -DTODSU*DSDZ2
            AD(I,K)  = AD(I,K)-AU(I,K)
            AD(I,K+1)= 1.-AL(I,K)
            A1(I,K+1)= U1(I,K+1)
            A2(I,K+1)= V1(I,K+1)
         ENDDO
      ENDDO
C
C     SOLVE TRIDIAGONAL PROBLEM FOR MOMENTUM
C
      CALL TRIDI2(IM,KM,AL,AD,AU,A1,A2,AU,A1,A2)
C
C     RECOVER TENDENCIES OF MOMENTUM
C
      CONWRC = CONW*SQRT(RCL)
      DO K = 1,KM
         DO I = 1,IM
            UTEND = (A1(I,K)-U1(I,K))*RDT
            VTEND = (A2(I,K)-V1(I,K))*RDT
            B(I,K)= B(I,K)+UTEND
            A(I,K)= A(I,K)+VTEND
            DUSFC(I) = DUSFC(I)+CONWRC*DEL(K)*PSTAR(I)*UTEND
            DVSFC(I) = DVSFC(I)+CONWRC*DEL(K)*PSTAR(I)*VTEND
         ENDDO
      ENDDO
C
      RETURN
      END
