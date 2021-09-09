      SUBROUTINE ISHEET ( TIME )
  
C------------------------------------------------------------------ 
  
C     ICE SHEET CONSTANTS 
  
      INTEGER MXSB
      REAL SECPYR, MTPDEG, RADPDG 
      REAL ICEFAC 
      REAL NBCLAT, MHPOIS 
      INTEGER TRIPS, HPCALL 
      REAL BNAUT, BWON, ALPHA, GAM
  
      PARAMETER (MXSB  = 160) 
  
C------------------------------------------------------------------ 
  
      COMMON /ISCOM/ HGT(0:MXSB,0:1), H, HP,
     1      ACMRAT, SLOPE, TIMELP, OSCARG, TIMSTP, GRDSTP,
     2      SNLAMP, SNLMPT, SNLXIN, TRIPS, RNETAC, POLFLX,
     3      MSNAUT, MAPRIM, OUTDAT, DELTAX, DELTAT, DHPDT(0:MXSB),
     4      HPCALL, ISSB
  
      COMMON /TEMCOM/ TEMP(0:MXSB)
      REAL TEMP 
  
C     VARIABLES FOR SLIDING AND TEMP DEPENDANT FLOW 
      REAL TB, TM, RHOI, GEE, GEOTH, CONDUC, SLIVEL 
      REAL DTDYB, SLIDE(0:MXSB), SLIPARM(0:MXSB), FLOPARM(0:MXSB) 
      REAL ACTEN, GASCON, TNAUT, QCDIVR, DMCSLI, DMPSLI 
  
C     ARGUMENTS 
      REAL TIME, TIMSTP, GRDSTP 
  
C     COMMON BLOCK HEIGHT DATA
      REAL HGT, DHPDT 
  
C     SNOW LINE DATA
      REAL ACMRAT, SLOPE, SNLXIN, TSTAR 
      REAL TIMELP, OSCARG, SNLAMP, SNLMPT 
      REAL MSNAUT, MAPRIM 
  
C     DATA ARRAYS 
      REAL FRCNG(0:MXSB),AHAT(0:MXSB),BHAT(0:MXSB)
      REAL GAMMA(0:MXSB), UPVCT(0:MXSB) 
  
C     LOOP VARIABLES, COUNTERS, FLAGS 
      INTEGER I, COUNT, MAXIT 
      REAL TIMCNT, TLIMIT 
      LOGICAL OUTDAT, TOLRNC
  
C     BOUNDARY MARKERS
      INTEGER ISNB, ISSB, ISSBOL
  
C     HEIGHT/DEPTH POINTERS 
      INTEGER H, HP 
  
C     TEMPORARY VALUE HOLDERS 
      REAL DELH, PERIOD 
      REAL DMC, DMP, DM 
      REAL G10, G11, G12, G20, G21, G22 
      REAL AI, BI, CI, FI 
      REAL HVIRT(0:1) 
  
C     COMPUTED CONSTANT ARRAYS
      REAL SINHLF(0:MXSB), DTDXSN(0:MXSB) 
  
C     COMPUTED CONSTANTS
      REAL RADSTP 
      REAL DELTAT, DELTAX 
      REAL DTAF2,MU1,MU2
  
C     SAVE LOCAL VARIABLES FOR MULTIPLE CALLS.
C     SAVE
  
C------------------------------------------------------------------ 
  
      IF(TRIPS.LT.0) GOTO 20000 
C-----MAIN TIME EXTRAPOLATION LOOP IN ISHEET
  
      DO 10000 TIMCNT = DELTAT, TIME*SECPYR, DELTAT 
  
C     CALCULATE THE ACCUMULATION RATE OVER THE ICE SHEET GRID 
  
C     CALCULATE POSITION OF PERIODICALLY VARYING SNOW LINE
        TIMELP = TIMELP + DELTAT
        SNLXIN = SNLAMP*SIN(OSCARG*TIMELP+0.0*PI) + SNLMPT
  
C       TEMPERATURE DEPENDANT ACCUMULATION RATE.
        DO 200 I=0,MXSB 
          TSTAR=GAM*(SLOPE*(I*DELTAX-SNLXIN)-HGT(I,H))
          IF(TSTAR.LE.0.0) THEN 
            AHAT(I)=ACMRAT*(1.+GAM*BNAUT*SLOPE*(I*DELTAX-SNLXIN)) 
            BHAT(I)=ACMRAT*GAM*BNAUT*BETA 
           ELSE 
            AHAT(I)=-ABLAT-ALPHA*BWON*GAM*SLOPE*(I*DELTAX-SNLXIN) 
            BHAT(I)=-ALPHA*BWON*GAM*BETA
          ENDIF 
        TB=TSTAR-DTDYB*(HGT(I,H)+HGT(I,HP)) 
        IF ((TB.LT.0.0).AND.(HGT(I,H)+HGT(I,HP)).GT.0.0) THEN 
          TEMP(I) = TB+TM 
          SLIDE(I)= 0.0 
         ELSE 
          TEMP(I) = TM
        IF(I.GT.0.AND.I.LT.MXSB) THEN 
C         IF THE SLOPE IS OF DIFFERING SIGN ON THE LEFT AND RIGHT 
C           SIDE OF THE POINT, OR IF IT IS ZERO, SET THE SLIDEING 
C           VELOCITY TO ZERO AT THAT POINT
          SLIDE(I)= SIGN(SLIPARM(I),-(HGT(I,H)-HGT(I-1,H))) 
C         IF ((HGT(I,H)-HGT(I-1,H))*(HGT(I+1,H)-HGT(I,H)).LE.0.0) 
C    1        SLIDE(I) = 0.0
           ELSE 
            SLIDE(I)=-SLIPARM(0)
          ENDIF 
        ENDIF 
C      IF THE SURFACE TEMP IS BELOW FREEZING, FORCE NO SLIDEING (EXPT)
      IF (TSTAR.LE.0.0) SLIDE(I)=0.0
        FLOPARM(I)= ICEFAC*EXP(QCDIVR*(1./TNAUT-1./TEMP(I)))
 200    CONTINUE
  
C       FIND THE FIRST PT THAT HAS ACCUMULATION.  GO FROM S-N.
        I=ISSB+10 
 210    IF (AHAT(I)-BHAT(I)*HGT(I,H).LT.0.0.AND.I.GT.1) THEN
          I=I-1 
          GO TO 210 
        ENDIF 
C       FIND THE EXACT INTERSECTION OF THE FIRN LINE. 
        DELI=(HGT(I,H)-SLOPE*(I*DELTAX-SNLXIN))/
     1        (HGT(I,H)-HGT(I+1,H)+SLOPE*DELTAX)
        IF (DELI.LT.0.0)  DELI=0.0
        IF (DELI.GT.1.0)  DELI=1.0
C       MODIFY THE ACCUMULATION RATES ACCORDINGLY.
        IF (DELI.LE.0.5) THEN 
          AHAT(I)=(.5+DELI)*AHAT(I)+(.5-DELI)*AHAT(I+1) 
          BHAT(I)=(.5+DELI)*BHAT(I)+(.5-DELI)*BHAT(I+1) 
         ELSE 
          AHAT(I+1)=(1.5-DELI)*AHAT(I+1)+(DELI-.5)*AHAT(I)
          BHAT(I+1)=(1.5-DELI)*BHAT(I+1)+(DELI-.5)*BHAT(I)
        ENDIF 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  
C-----FIND NORTH BOUNDARY GRID POINT OF ICE SHEET 
        ISNB = 1
 300    IF ((HGT(ISNB,H)+HGT(ISNB,HP).LE.0.0) 
     1     .AND.(AHAT(ISNB)-BHAT(ISNB)*HGT(ISNB,H).LT.0.0)
     2     .AND.(ISNB.LT.MXSB)) THEN
          ISNB = ISNB + 1 
          GOTO 300
        ENDIF 
  
C-----FIND SOUTH BOUNDARY GRID POINT OF ICE SHEET 
        ISSBOL=ISSB 
        ISSB = MIN(MXSB-1,ISSBOL+15)
C       THIS SPEEDS THE SEARCH FOR ISSB 
 400    IF ((HGT(ISSB,H)+HGT(ISSB,HP).LE.0.0) 
     1     .AND.(AHAT(ISSB)-BHAT(ISSB)*HGT(ISSB,H).LT.0.0)
     2     .AND.(ISSB.GT.1)) THEN 
          ISSB = ISSB - 1 
          GOTO 400
        ENDIF 
        IF (ISSB.NE.ISSBOL) THEN
          HVIRT(H)=-HGT(ISSB+1,HP)
        ENDIF 
C       IF THE ICE SHEET MOVES, NEED TO SET HVIRT TO A REAL HEIGHT. 
        IF (ISSB.EQ.MXSB-1) THEN
C       WE HAVE REACHED THE EDGE OF THE GRID. 
          WRITE (*,9007)
 9007     FORMAT (' WE HAVE RUN OUT OF SPACE IN THE GRID. ')
C         CALL PMDLOAD
          STOP
        ENDIF 
  
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C     DO THE BEDROCK EXTRAPOLATION
      TRIPS=TRIPS+1 
      IF (MOD(TRIPS,HPCALL).EQ.0) THEN
        CALL HPEXT
       ELSE 
        DO 700 I=0,MXSB 
          HGT(I,HP)=HGT(I,HP)+DHPDT(I)
 700    CONTINUE
      ENDIF 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  
C-----TEST FOR PRESENCE OF AN ICE SHEET 
  
        COUNT=0 
        IF (ISNB.GT.ISSB) THEN
C         NEITHER AN ICE SHEET OR ZONE OF POSITIVE ACCUM PRESENT
C         SET H=-H' FOR THE ENTIRE GRID 
          DO 1000 I=0,MXSB
            HGT(I,H)=-HGT(I,HP) 
 1000     CONTINUE
  
         ELSE IF(0.GE.(ISSB-ISNB).AND.(ISSB-ISNB).GE.0) THEN
C         THIS IS THE SMALL ICE SHEET CASE. 
  
C         DO H EXTRAPOLATION
          DO 2000 I=ISNB-1,ISSB+1 
            HGT(I,H)=(1.-BHAT(I)*DELTAT)*HGT(I,H) 
     1        -DHPDT(I)+AHAT(I)*DELTAT
            IF (HGT(I,H)+HGT(I,HP).LT.0.0) HGT(I,H)=-HGT(I,HP)
 2000     CONTINUE
C         EXTRAPOLATE H NORTH OF THE ICE SHEET
          DO 2100 I=0,ISNB-2
            HGT(I,H)=-HGT(I,HP) 
 2100     CONTINUE
C         EXTRAPOLATE H SOUTH OF THE ICE SHEET
          DO 2200 I=ISSB+2,MXSB 
            HGT(I,H)=-HGT(I,HP) 
 2200     CONTINUE
  
C         APPLY BOUNDARY CONDITION AT N BD : ISNB-1 
          IF (HGT(ISNB,H).LT.MHPOIS) THEN 
            HGT(ISNB-1,H) = HGT(ISNB,H) 
           ELSE 
            HGT(ISNB-1,H) = MHPOIS
          ENDIF 
          IF(HGT(ISNB,H).LT.0.0) HGT(ISNB-1,H)= 
     1        HGT(ISNB,H)+HGT(ISNB,HP)-HGT(ISNB-1,HP) 
C         ADD THIS TEST TO AVOID HAVING NEGATIVE THICKNESS
C           WHEN THE ICE SHEET READVANCES. BG 
  
          HVIRT(H)=HGT(ISSB+1,H)
          WRITE (*,9001) COUNT,(HGT(I,H),I=MAX(0,ISSB-9), 
     1        MAX(9,ISSB)),HVIRT(H) 
 9001     FORMAT (' COUNT:',I2, 11(2X,F9.3))
  
       ELSE 
C         THERE IS AN ICE SHEET AND/OR POSITIVE ACCUM ZONE PRESENT
C         THIS IS THE LARGE ICE SHEET CASE
          IF(ISNB.GT.1) WRITE (*,9002)
  
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C-----CALCULATE CONSTANT FORCING FUNCTIONS
  
C-----NORTH BOUNDARY SPECIAL POINT
          I=ISNB-1
          DMC = SINHLF(I)*(HGT(I+1,H)-HGT(I,H))**3
     1          *(MAX(0.,HGT(I,H)+HGT(I+1,H)
     2           +HGT(I,HP)+HGT(I+1,HP)))**5
          DMCSLI = (HGT(I,H)+HGT(I+1,H)+HGT(I,HP)+HGT(I+1,HP))**3*
     1          (HGT(I+1,H)-HGT(I,H))**2
     2          *SINHLF(I)
  
C-----INTERIOR POINTS 
          DO 3100 I = ISNB, ISSB
            DMP = DMC 
            DMPSLI = DMCSLI 
            DMC = SINHLF(I)*(HGT(I+1,H)-HGT(I,H))**3
     1         *(MAX(0.,HGT(I,H)+HGT(I+1,H)+HGT(I,HP)+HGT(I+1,HP)))**5
          DMCSLI = (HGT(I,H)+HGT(I+1,H)+HGT(I,HP)+HGT(I+1,HP))**3*
     1          (HGT(I,H)-HGT(I-1,H))**2
     2          *SINHLF(I)
C           SPECIAL TREATMENT FOR ISSB TO ENSURE MASS BALANCE 
            IF (I.EQ.ISSB) DMC=SINHLF(I)*(HVIRT(H)-HGT(I,H))**3 
     1        *(MAX(0.,HGT(I,H)+HGT(I,HP)))**5
            FRCNG(I) = (MU1-DTAF2*BHAT(I))*HGT(I,H) 
     1                   -DHPDT(I)
     2                   +DELTAT*AHAT(I)
     3                   +DTDXSN(I)*FLOPARM(I)*(DMC-DMP)
     4                   -(SLIDE(I)+SLIDE(I+1))*DMCSLI
     5                   +(SLIDE(I)+SLIDE(I-1))*DMPSLI
 3100     CONTINUE
  
C-----SOUTH EDGE SPECIAL POINT (POINT M7) 
          FRCNG(ISSB+1)=HGT(ISSB+1,H)+AHAT(ISSB+1)*DELTAT 
     1      -DHPDT(I) 
     1      +DELTAT*(BHAT(ISSB+1))*HGT(ISSB+1,HP) 
     2      -DTDXSN(ISSB+1)*FLOPARM(ISSB+1)*SINHLF(ISSB+1)
     3        *(MAX(0.,HGT(ISSB,H)+HGT(ISSB,HP)))**5
     4        *(HVIRT(H)-HGT(ISSB,H))**3
     5        +(SLIDE(ISSB)+SLIDE(ISSB+1))*SINHLF(ISSB+1)*
     6          (HGT(ISSB,H)+HGT(ISSB,HP))**3*(HVIRT(H)-HGT(ISSB,H))**2 
  
  
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C     BEGIN ITERATION 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 3000 CONTINUE
            COUNT = COUNT + 1 
  
C-----COMPUTE ELEMENTS IN THE TRIDIAGONAL MATRIX
C     AND BEGIN MATRIX INVERSION
  
C-----CAL AI,BI,CI,FI, GAMMA(I), G(I) FOR NORTH BD POINT
C     NOTE: G(I) IS UPVCT(I) IN CODE
  
            G11 =  MAX(0.,HGT(ISNB-1,H) + HGT(ISNB,  H) 
     1            + HGT(ISNB-1,HP) + HGT(ISNB,  HP))
            G21 =  MAX(0.,HGT(ISNB,  H) + HGT(ISNB+1,H) 
     1            + HGT(ISNB,  HP) + HGT(ISNB+1,HP))
  
            G12 =   HGT(ISNB,  H) - HGT(ISNB-1,H) 
            G22 =   HGT(ISNB+1,H) - HGT(ISNB,  H) 
  
            G10 = -SINHLF(ISNB-1) * G11**4 * G12**2 
            G20 =  SINHLF(ISNB)   * G21**4 * G22**2 
  
            AI = (MU2 - DTAF2*BHAT(ISNB)) - DTDXSN(ISNB)*FLOPARM(ISNB)
     1           * ( (5.0*G12+3.0*G11)*G10 + (5.0*G22-3.0*G21)*G20 )
     2          +(SINHLF(ISNB)*(SLIDE(ISNB)+SLIDE(ISNB+1))* 
     3              (3*G21**2*G22**2-2.*G21**3*G22)-SINHLF(ISNB-1)* 
     4              (SLIDE(ISNB-1)+SLIDE(ISNB))*(3.*G11**2*G12**2 
     5            +2.*G11**3*G12))
            BI = -DTDXSN(ISNB)*FLOPARM(ISNB) * (5.0*G12-3.0*G11) * G10
     1        -SINHLF(ISNB-1)*(SLIDE(ISNB)-SLIDE(ISNB-1))*
     2          (3.*G11**2*G12**2-2.*G11**3*G12)
            CI = -DTDXSN(ISNB)*FLOPARM(ISNB) * (5.0*G22+3.0*G21) * G20
     1        +SINHLF(ISNB)*(SLIDE(ISNB)+SLIDE(ISNB+1))*
     2          (3.*G21**2*G22**2+2.*G21**3*G22)
            FI = FRCNG(ISNB) - (MU2 + DTAF2*BHAT(ISNB))*HGT(ISNB,H) 
     1           + DTDXSN(ISNB)*FLOPARM(ISNB)*(G10*G11*G12+G20*G21*G22) 
     1        -SINHLF(ISNB)*(SLIDE(ISNB)+SLIDE(ISNB+1))*G21**3*G22**2 
     2        +SINHLF(ISNB-1)*(SLIDE(ISNB)+SLIDE(ISNB-1))*
     3          (G11**3*G12**2) 
  
            IF (HGT(ISNB,H).LT.MHPOIS) AI=AI+BI 
  
            GAMMA(ISNB) = CI/AI 
            UPVCT(ISNB) = FI/AI 
  
C-----CAL AI,BI,CI,FI AND GAMMA(I),G(I) FOR INTERIOR PTS
  
            DO 3200 I = ISNB+1, ISSB
  
              G11 =  G21
              G21 =  MAX(0., HGT(I, H) + HGT(I+1, H)
     1              +HGT(I,HP) + HGT(I+1,HP)) 
              G12 =  G22
              G22 =  HGT(I+1,H) - HGT(I,H)
                IF (I.EQ.ISSB) G22 = HVIRT(H)-HGT(I,H)
              G10 = -G20
              G20 =  SINHLF(I) * G21**4 * G22**2
  
              AI = (MU2 + DTAF2*BHAT(I)) - DTDXSN(I)*FLOPARM(I) 
     1             * ((5.0*G12+3.0*G11)*G10 + (5.0*G22-3.0*G21)*G20)
     2          +(SINHLF(I)*(SLIDE(I)+SLIDE(I+1))*
     3              (3*G21**2*G22**2-2.*G21**3*G22)-SINHLF(I-1)*
     4              (SLIDE(I-1)+SLIDE(I))*(3.*G11**2*G12**2 
     5            +2.*G11**3*G12))
              BI = -DTDXSN(I)*FLOPARM(I) * (5.0*G12-3.0*G11) * G10
     1        -SINHLF(I-1)*(SLIDE(I)-SLIDE(I-1))* 
     2          (3.*G11**2*G12**2-2.*G11**3*G12)
              CI = -DTDXSN(I)*FLOPARM(I) * (5.0*G22+3.0*G21) * G20
     1        +SINHLF(I)*(SLIDE(I)+SLIDE(I+1))* 
     2          (3.*G21**2*G22**2+2.*G21**3*G22)
              FI = FRCNG(I) - (MU2 + DTAF2*BHAT(I)) * HGT(I,H)
     1             + DTDXSN(I)*FLOPARM(I)*(G10*G11*G12+G20*G21*G22) 
     1        -SINHLF(I)*(SLIDE(I)+SLIDE(I+1))*G21**3*G22**2
     2        +SINHLF(ISNB-1)*(SLIDE(I)+SLIDE(I-1))*
     3          (G11**3*G12**2) 
  
              GAMMA(I) = CI / (AI - BI*GAMMA(I-1))
              UPVCT(I) = (FI - BI*UPVCT(I-1)) / (AI - BI*GAMMA(I-1))
 3200       CONTINUE
  
C-----CAL AI,BI,CI,FI AND GAMMA(I), G(I) FOR SOUTH EDGE PT
C-----NEW TREATMENT 4-26-84.
  
            G11 =  G21
            G12 =  G22
            DM  =  DTDXSN(ISSB+1)*FLOPARM(ISSB+1)*SINHLF(ISSB+1)
  
            AI = 1.+3.*DM*G11**5*G12**2 
     1        -SINHLF(ISSB)*(SLIDE(ISSB+1)+SLIDE(ISSB)) 
     2        *(3.*G11**2*G12**2+2.*G11**3*G12) 
            BI = DM*(5.*G11**4*G12**3-3.*G11**5*G12**2) 
     1        -SINHLF(ISSB)+(SLIDE(ISSB+1)+SLIDE(ISSB)) 
     2        *(3.*G11**2*G12**2+2.*G11**3*G12) 
            FI = FRCNG(ISSB+1)
     1            -HGT(ISSB+1,H)
     2            -DM*G11**5*G12**3 
     3        -SINHLF(ISSB)*(SLIDE(ISSB+1)+SLIDE(ISSB))*
     4       G11**3*G12**2
  
C           NOTE NEW HVIRT IS SIMPLY G(I) AT THIS PT
C           ADD THE OLD VALUES HERE.  THIS GETS RID OF UJM1.
            DELH=(FI-BI*UPVCT(ISSB))/(AI-BI*GAMMA(ISSB))
            HVIRT(H)=HGT(ISSB+1,H)+DELH 
            TOLRNC = .TRUE. 
  
            DO 3300 I = ISSB, ISNB, -1
              DELH = UPVCT(I) - DELH*GAMMA(I) 
              HGT(I,H)=HGT(I,H)+DELH
              TOLRNC = TOLRNC .AND. (ABS(DELH).LT.TLIMIT) 
 3300       CONTINUE
  
            IF(OUTDAT) THEN 
              WRITE (*,9001) COUNT,(HGT(I,H),I=MAX(0,ISSB-9), 
     1          MAX(9,ISSB)),HVIRT(H) 
            ENDIF 
  
C           APPLY THE BOUNDARY CONDITION TO THE NORTH EDGE. 
            IF (HGT(ISNB,H).LT.MHPOIS) THEN 
              HGT(ISNB-1,H) = HGT(ISNB,H) 
             ELSE 
              HGT(ISNB-1,H) = MHPOIS
            ENDIF 
            IF(HGT(ISNB,H).LT.0.0) HGT(ISNB-1,H)= 
     1          HGT(ISNB,H)+HGT(ISNB,HP)-HGT(ISNB-1,HP) 
C           ADD THIS TEST TO AVOID HAVING NEGATIVE THICKNESS
C             WHEN THE ICE SHEET READVANCES. BG 
  
  
C           IF HVIRT+H' IS POSITIVE, SET H(ISSB+1) TO HVIRT.
C           OTHERWISE, SET H(ISSB+1) TO -H'(ISSSB+1). 
            IF (HVIRT(H)+HGT(ISSB+1,HP).GT.0.0) THEN
              HGT(ISSB+1,H)=HVIRT(H)
             ELSE 
              HGT(ISSB+1,H)=-HGT(ISSB+1,HP) 
            ENDIF 
C           CHECK THAT THE THICKNESS OVER THE ICE SHEET IS >= 0 
            DO 3500 I=ISNB-1,ISSB+1 
              IF (HGT(I,H)+HGT(I,HP).LT.0.) HGT(I,H)=-HGT(I,HP) 
 3500       CONTINUE
  
C     END OF ITERATION LOOP.  ITERATE OR EXIT HERE. 
      IF ((COUNT.LT.MAXIT).AND.(.NOT.TOLRNC)) GO TO 3000
      ENDIF 
C-----END OF TIME STEP LOOP IN ISHEET 
C     EXTRAPOLATE H NORTH AND SOUTH OF THE ICE SHEET. 
      DO 3510 I=0,ISNB-2
        HGT(I,H)=-HGT(I,HP) 
 3510 CONTINUE
      DO 3520 I=ISSB+2,MXSB 
        HGT(I,H)=-HGT(I,HP) 
 3520 CONTINUE
  
C 
      IF(OUTDAT) THEN 
        WRITE(*,9004) ISNB,ISSB,TIMELP/SECPYR,TIMCNT/SECPYR,
     1      SNLXIN/DELTAX,COUNT,TOLRNC
      ENDIF 
  
10000 CONTINUE
  
C-------COMPUTE MASS FLUXES INTO AND OUT OF THE ICE SHEET.
        RNETAC=0.0
        DO 500 I=ISNB,ISSB
        IF (AHAT(I)-BHAT(I)*HGT(I,H).GE.0.0)
     1    RNETAC=RNETAC+(AHAT(I)-BHAT(I)*HGT(I,H))*SINHLF(I)
 500    CONTINUE
        RNETAC=RNETAC*SECPYR*DELTAX 
  
        POLFLX=FLOPARM(ISNB-1)*(MAX(0.,HGT(ISNB,H)+HGT(ISNB,HP)+
     1          HGT(ISNB-1,H)+
     1    HGT(ISNB-1,HP)))**5*((HGT(ISNB,H)-HGT(ISNB-1,H))/DELTAX)**3 
        POLFLX=SINHLF(ISNB-1)*POLFLX*SECPYR/32.0
  
        MSNAUT=FLOPARM(ISSB)*(MAX(0.,HGT(ISSB,H)+HGT(ISSB,HP)))**5* 
     1    ((HVIRT(H)-HGT(ISSB,H))/DELTAX)**3
        MSNAUT=ABS(SINHLF(ISSB+1)*MSNAUT*SECPYR/32.0) 
        MAPRIM=0.0
        DO 600 I=ISNB,ISSB
          IF (AHAT(I)-BHAT(I)*HGT(I,H).LT.0.0)
     1      MAPRIM=MAPRIM+ABS((AHAT(I)-BHAT(I)*HGT(I,H))*DELTAX)
     2      *SINHLF(I)*SECPYR 
 600    CONTINUE
  
C-----RETURN TO MAIN PROGRAM CALLING ICE SHEET
  
      RETURN
  
C------------------------------------------------------------------ 
20000 CONTINUE
C-----SECTION FOR INITIALIZING ICE SHEET, CALLED WHEN TRIPS=0.
  
      PI      = 3.1415926535898 
      SECPYR  = 31557600.0
      READ (*,9006) ACMRAT
      ACMRAT  = ACMRAT/SECPYR 
      READ (*,9006) ABLAT 
      ABLAT   = ABLAT/SECPYR
      READ (*,9006) SLOPE 
      READ (*,9006) SNLAMP
      SNLAMP  = SNLAMP*1000.0 
      READ (*,9006) SNLMPT
      SNLMPT  = SNLMPT*1000.0 
      READ (*,9006) PERIOD
C       PERIODS ARE, IN YEARS 
      READ (*,9006) ICEFAC
      READ (*,9006) BNAUT 
      READ (*,9006) BWON
      BWON=BWON/SECPYR
      READ (*,9006) ALPHA 
      READ (*,9006) GAM 
      READ (*,9006) BETA
      READ (*,9008) HPCALL
      READ (*,9008) MAXIT 
      READ (*,9006) TLIMIT
      READ (*,9006) MHPOIS
  
      READ (*,9006) TM
      READ (*,9006) RHOI
      READ (*,9006) GEE 
      READ (*,9006) GEOTH 
      READ (*,9006) CONDUC
      READ (*,9006) SLIVEL
      READ (*,9006) CAPA
      READ (*,9006) ACTEN 
      READ (*,9006) GASCON
      READ (*,9006) TNAUT 
  
      DTDYB = -GEOTH/CONDUC 
      QCDIVR= ACTEN/GASCON
  
      RADPDG = PI / 180.0 
      NBCLAT = 12.0*RADPDG
      OSCARG = 2*PI/(PERIOD*SECPYR) 
      MTPDEG = 111111.111111
      SLIVEL = SLIVEL/SECPYR
      CAPA    = CAPA* SLIVEL
  
C     POINTERS
      H=0 
      HP=1
      ISSB=MXSB-20
      ISNB=0
  
      TIMELP = 0.0
  
      RADSTP = GRDSTP * RADPDG
      DELTAT = TIMSTP * SECPYR
      DELTAX = GRDSTP * MTPDEG
  
C     ISHEET CONSTANTS
      DTAF2  = DELTAT/2.
      MU1    = 1.0
      MU2    = 1.0
  
      TRIPS=1+TRIPS 
      DO 8000 I = 0, MXSB 
        SINHLF(I) = SIN((I+0.5)*RADSTP+NBCLAT)
        DTDXSN(I)=DELTAT/(64.*SIN(I*RADSTP+NBCLAT)*DELTAX**4) 
        SLIPARM(I) =CAPA*DELTAT/(16.*DELTAX**3*SIN(I*RADSTP+NBCLAT))
 8000 CONTINUE
  
      HVIRT(0)=0.0
      HVIRT(1)=0.0
  
 9002 FORMAT (' WE HAVE A NON-POLAR ICE SHEET ')
  
 9006 FORMAT (F14.4)
  
 9004 FORMAT (' ISNB:',I3,4X,'ISSB:',I3,4X,'TIMELP:',F7.1,4X, 
     1   'TIMCNT:',F5.0,4X,'SNLXIN',F8.3,4X,
     2   ' ITERS: ',I2,4X,' TOLRNC: ',L1) 
  
 9008 FORMAT(I3)
  
      RETURN
      END 
