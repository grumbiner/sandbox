      SUBROUTINE GETIJ (XLAT,XLON,SLMSK,BLAT,DX,
     1                  ILS,IDM,JDM,KI,KJ)
      DIMENSION BLAT(JDM),DIST(4),KENI(4),KENJ(4)
      DIMENSION IPSORT(4),IPSKP(4)
      DIMENSION SLMSK( 192 , 94 )
      JDM2 = JDM / 2
      JDMP1 = JDM2 + 1
      IF (ABS(XLAT).GT.BLAT(1)) GO TO 70
C----    GET UPPER LEFT GAUSSIAN POINT (IA,JA) ON GRIDBOX
C          SURROUNDING THE INPUT LAT/LON POINT........
        IA = XLON/DX + 1
        IB = IA + 1
        IF (IA.GE.IDM) IB = 1
        XI = XLON/DX + 1. - IA
        DO 10 JAK=2,JDMP1
         JB = JAK - 1
         IF(ABS(XLAT).GT.BLAT(JAK)) GO TO 15
  10    CONTINUE
  15    CONTINUE
        JA = JB
C----   NORMALIZED DISTANCE FROM UPPER LAT TO GAUSSIAN LAT
        XJ = (BLAT(JA) - ABS(XLAT)) / (BLAT(JA)-BLAT(JA+1))
C       XOUT(I,LAT)   = (1-XI)* XJ   *XIN(IA,JA+1) +
CC   1                 XI *  XJ  *XIN(IB,JA+1) +
CCC  2              (1-XI)*(1-XJ)*XIN(IA  ,JA  ) +
CCCC 3               XI   *(1-XJ)*XIN(IB,JA  )
C----    SOUTHERN HEMISPHERE
        IF (XLAT.LT.0.) THEN
         JA = JDM - JA
         XJ = 1. - XJ
        END IF
CCCC    XOUT(I,JOUT+1-LAT)=(1-XI)* XJ   *XIN(IA,JA+1) +
CCC  1                 XI *  XJ  *XIN(IB,JA+1) +
CC   2              (1-XI)*(1-XJ)*XIN(IA  ,JA  ) +
C    3               XI   *(1-XJ)*XIN(IB,JA  )
C...     UPPER LEFT POINT
      DIST(1)= SQRT(XI**2+XJ**2)
      KENI(1)= IA
      KENJ(1)= JA
C...     UPPER RIGHT
      DIST(2)= SQRT((1-XI)**2+XJ**2)
      KENI(2)= IB
      KENJ(2)= JA
C...     LOWER RIGHT
      DIST(3)= SQRT((1-XI)**2+(1-XJ)**2)
      KENI(3)= IB
      KENJ(3)= JA+1
C...     LOWER LEFT
      DIST(4)= SQRT(XI**2+(1-XJ)**2)
      KENI(4)= IA
      KENJ(4)= JA+1
C---     NOW SORT THE DISTANCES (BY INDEX, SHORTEST FIRST)
      NPT = 4
      NPT1 = NPT + 1
      DO 20 KD=1,NPT
        IPSKP(KD) = 0
   20 CONTINUE
      DO 40 KD=1,NPT
        DD=100.
C---     FIND SHORTEST DIST OF THE REMAINING UNSORTED DATA...
        DO 25 KK=1,NPT
         IF(IPSKP (KK).GT.0) GO TO 25
          IF(DIST(KK).LT.DD) THEN
            DD = DIST(KK)
            JX = KK
          END IF
   25   CONTINUE
C---   STORE SORTED INDEX
        IPSORT(KD) = JX
        IPSKP (JX) = 1
   40 CONTINUE
      PRINT 102,(DIST(KK),KK=1,NPT),(IPSORT(KK),KK=1,NPT)
  102 FORMAT(1H ,' DISTANCES=',4F8.4,' SORTED INDICES=',4I4)
C---   END OF DISTANCE SORT
      XILS = ILS
      IF (ILS.LT.0) THEN
       KI = KENI(IPSORT(1))
       KJ = KENJ(IPSORT(1))
       RETURN
      END IF
      IF (ILS.EQ.0) THEN
C....     FIND NEAREST SEA POINT
       DO 45 KD=1,NPT
        II = KENI(IPSORT(KD))
        JJ = KENJ(IPSORT(KD))
        IF (SLMSK(II,JJ).LE.XILS) GO TO 46
   45  CONTINUE
C....     NO SEA POINTS SO DEFAULT TO NEAREST POINT
       PRINT 49,XLAT,XLON
   49  FORMAT(1H ,' ASKED FOR SEA POINT BUT CAN T FIND ONE FOR',
     1            ' LAT LON=',2F9.2,'..SO DEFAULT TO NEAREST')
       KI = KENI(IPSORT(1))
       KJ = KENJ(IPSORT(1))
       RETURN
   46  CONTINUE
       KI = II
       KJ = JJ
       RETURN
      END IF
      IF (ILS.EQ.1) THEN
C....     FIND NEAREST LAND/ICE POINT
       DO 55 KD=1,NPT
        II = KENI(IPSORT(KD))
        JJ = KENJ(IPSORT(KD))
        IF (SLMSK(II,JJ).GE.XILS) GO TO 56
   55  CONTINUE
C....     NO LAND POINTS SO DEFAULT TO NEAREST POINT
       PRINT 59,XLAT,XLON
   59  FORMAT(1H ,' ASKED FOR LAND SEA POINT BUT CAN T FIND ONE FOR',
     1            ' LAT LON=',2F9.2,'..SO DEFAULT TO NEAREST')
       KI = KENI(IPSORT(1))
       KJ = KENJ(IPSORT(1))
       RETURN
   56  CONTINUE
       KI = II
       KJ = JJ
       RETURN
      END IF
C...   OUTSIDE LIMIT OF GAUSSIAN POLAR ROWS SO JUST TAKE NEAREST
C        POINT WITHOUT REGARD TO LAND AND SEA
   70 CONTINUE
      IA = XLON/DX + 1
      IB = IA + 1
      IF (IA.GE.IDM) IB = 1
      XI = XLON/DX + 1. - IA
      JA = 1
      IF (XLAT.LT.0.) JA = JDM
      IF (XI.GT.0.5) THEN
       KI = IB
       KJ = JA
      ELSE
       KI = IA
       KJ = JA
      END IF
      RETURN
      END
