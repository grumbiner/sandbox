      SUBROUTINE ISHEET ( TIME )
  
C------------------------------------------------------------------ 
  
C     ICE SHEET CONSTANTS 
  
      INTEGER MXSB
      PARAMETER (MXSB  = 160) 
      REAL SECPYR, MTPDEG, RADPDG 
      REAL BRLXTM, NU, ETA, ICEFAC
      REAL NBCLAT, MHPOIS 
      REAL RHOI, RHOR 
      INTEGER TRIPS 
      REAL BNAUT, BWON, ALPHA, GAM
  
  
C------------------------------------------------------------------ 
  
      COMMON /ISCOM/ HGT(0:MXSB,0:1), H, HP,
     1      ACMRAT, SLOPE, TIMELP, OSCARG, TIMSTP, GRDSTP,
     2      SNLAMP, SNLMPT, SNLXIN, TRIPS, RNETAC, POLFLX,
     3      MSNAUT, MAPRIM, OUTDAT, ISSB
  
C     ARGUMENTS 
      REAL TIME, TIMSTP, GRDSTP 
  
C     COMMON BLOCK HEIGHT DATA
      REAL HGT
  
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
      REAL DMC, DMP, DM
      REAL G10, G11, G12, G20, G21, G22
      REAL AI, BI, CI, FI
      REAL HVIRT(0:1)

C     COMPUTED CONSTANT ARRAYS
      REAL SINHLF(0:MXSB), DTDXSN(0:MXSB)

C     COMPUTED CONSTANTS
      REAL RADSTP
      REAL DELTAT, DELTAX
      REAL DTET, DTNU, DTAF2, MU1, MU2

      REAL DELI, TAU, PERIOD

C     SAVE LOCAL VARIABLES FOR MULTIPLE CALLS.
      SAVE

C------------------------------------------------------------------

      IF(TRIPS.EQ.0) GOTO 20000
C-----MAIN TIME EXTRAPOLATION LOOP IN ISHEET
  
      DO 10000 TIMCNT = DELTAT, TIME*SECPYR, DELTAT 
  
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C     DO THE BEDROCK EXTRAPOLATION
      DO 100 I=0,MXSB 
        HGT(I,HP)=MAX(0.,(1.-DTNU)*HGT(I,HP)+DTET*HGT(I,H)) 
 100  CONTINUE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C     CALCULATE THE ACCUMULATION RATE OVER THE ICE SHEET GRID 
  
C     CALCULATE POSITION OF PERIODICALLY VARYING SNOW LINE
        TIMELP = TIMELP + DELTAT
        SNLXIN = SIN(OSCARG*TIMELP)*SNLAMP + SNLMPT 
  
C       TEMPERATURE DEPENDANT ACCUMULATION RATE.
        DO 200 I=0,MXSB 
          TSTAR=GAM*(SLOPE*(I*DELTAX-SNLXIN)-HGT(I,H))
          IF(TSTAR.LE.0.0) THEN 
            AHAT(I)=ACMRAT*(1.+GAM*BNAUT*SLOPE*(I*DELTAX-SNLXIN)) 
            BHAT(I)=ACMRAT*GAM*BNAUT
           ELSE 
            AHAT(I)=-ACMRAT-ALPHA*BWON*GAM*SLOPE*(I*DELTAX-SNLXIN)
            BHAT(I)=-ALPHA*BWON*GAM 
          ENDIF 
 200    CONTINUE
C       FIND THE EXACT INTERSECTION 
        I=MIN(ISSB+10,MXSB) 
 210    IF(AHAT(I)-BHAT(I)*HGT(I,H).LT.0.0.AND.I.GT.1) THEN 
          I=I-1 
          GO TO 210 
        ENDIF 
        DELI=(HGT(I,H)-SLOPE*(I*DELTAX-SNLXIN))/
     1        (HGT(I,H)-HGT(I+1,H)+SLOPE*DELTAX)
C       MODIFY THE ACCUMULATION RATE ACCORDINGLY
        IF(DELI.LE.0.5) THEN
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
        ISSB = MXSB - 1 
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
          STOP
        ENDIF 
  
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
            HGT(I,H)=(1.-DTET-BHAT(I)*DELTAT)*HGT(I,H)
     1        +DTNU*HGT(I,HP)+AHAT(I)*DELTAT
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
      IF (OUTDAT) THEN
          WRITE (*,9001) COUNT,(HGT(I,H),I=MAX(0,ISSB-9), 
     1        MAX(9,ISSB)),HVIRT(H) 
      ENDIF 
 9001     FORMAT (' COUNT:',I2, 11(2X,F9.3))
  
       ELSE 
C         THERE IS AN ICE SHEET AND/OR POSITIVE ACCUM ZONE PRESENT
C         THIS IS THE LARGE ICE SHEET CASE
          IF(ISNB.GT.1) WRITE (*,9002)
 9002     FORMAT (' WE HAVE A NON POLAR ICE SHEET. ')
  

C---------------------------------------------------------------------------
C-----CALCULATE CONSTANT FORCING FUNCTIONS
  
C-----NORTH BOUNDARY SPECIAL POINT
          I=ISNB-1
          DMC = SINHLF(I)*(HGT(I+1,H)-HGT(I,H))**3
     1          *(MAX(0.,HGT(I,H)+HGT(I+1,H)
     2           +HGT(I,HP)+HGT(I+1,HP)))**5
  
C-----INTERIOR POINTS 
          DO 3100 I = ISNB, ISSB
            DMP = DMC 
            DMC = SINHLF(I)*(HGT(I+1,H)-HGT(I,H))**3
     1         *(MAX(0.,HGT(I,H)+HGT(I+1,H)+HGT(I,HP)+HGT(I+1,HP)))**5
C           SPECIAL TREATMENT FOR ISSB TO ENSURE MASS BALANCE 
            IF (I.EQ.ISSB) DMC=SINHLF(I)*(HVIRT(H)-HGT(I,H))**3 
     1        *(MAX(0.,HGT(I,H)+HGT(I,HP)))**5
            FRCNG(I) = (MU1-DTAF2*BHAT(I))*HGT(I,H) 
     1                   +DTNU*HGT(I,HP)
     2                   +DELTAT*AHAT(I)
     3                   +DTDXSN(I)*(DMC-DMP) 
 3100     CONTINUE
  
C-----SOUTH EDGE SPECIAL POINT (POINT M7) 
          FRCNG(ISSB+1)=HGT(ISSB+1,H)+AHAT(ISSB+1)*DELTAT 
     1      +DELTAT*(BHAT(ISSB+1)+ETA+NU)*HGT(ISSB+1,HP)
     2      -DTDXSN(ISSB+1)*SINHLF(ISSB+1)
     3        *(MAX(0.,HGT(ISSB,H)+HGT(ISSB,HP)))**5
     4        *(HVIRT(H)-HGT(ISSB,H))**3
  
  
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
  
            AI = (MU2 - DTAF2*BHAT(ISNB)) - DTDXSN(ISNB)
     1           * ( (5.0*G12+3.0*G11)*G10 + (5.0*G22-3.0*G21)*G20 )
            BI = -DTDXSN(ISNB) * (5.0*G12-3.0*G11) * G10
            CI = -DTDXSN(ISNB) * (5.0*G22+3.0*G21) * G20
            FI = FRCNG(ISNB) - (MU2 + DTAF2*BHAT(ISNB))*HGT(ISNB,H) 
     1           + DTDXSN(ISNB)*(G10*G11*G12+G20*G21*G22) 
  
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
  
              AI = (MU2 + DTAF2*BHAT(I)) - DTDXSN(I)
     1             * ((5.0*G12+3.0*G11)*G10 + (5.0*G22-3.0*G21)*G20)
              BI = -DTDXSN(I) * (5.0*G12-3.0*G11) * G10 
              CI = -DTDXSN(I) * (5.0*G22+3.0*G21) * G20 
              FI = FRCNG(I) - (MU2 + DTAF2*BHAT(I)) * HGT(I,H)
     1             + DTDXSN(I)*(G10*G11*G12+G20*G21*G22)
  
              GAMMA(I) = CI / (AI - BI*GAMMA(I-1))
              UPVCT(I) = (FI - BI*UPVCT(I-1)) / (AI - BI*GAMMA(I-1))
 3200       CONTINUE
  
C-----CAL AI,BI,CI,FI AND GAMMA(I), G(I) FOR SOUTH EDGE PT
C-----NEW TREATMENT 4-26-84.
  
            G11 =  G21
            G12 =  G22
            DM  =  DTDXSN(ISSB+1)*SINHLF(ISSB+1)
  
            AI = 1.+3.*DM*G11**5*G12**2 
            BI = DM*(5.*G11**4*G12**3-3.*G11**5*G12**2) 
            FI = FRCNG(ISSB+1)
     1            -HGT(ISSB+1,H)
     2            -DM*G11**5*G12**3 
  
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
     1        SNLXIN/DELTAX,COUNT,TOLRNC
      ENDIF 
 9004   FORMAT (' ISNB:',I3,4X,'ISSB:',I3,4X,'TIMELP:',F7.1,4X, 
     1   'TIMCNT:',F5.0,4X,'SNLXIN',F8.3,4X,
     2   ' ITERS: ',I2,4X,' TOLRNC: ',L1) 
  
10000 CONTINUE
  
C-------COMPUTE MASS FLUXES INTO AND OUT OF THE ICE SHEET.
        RNETAC=0.0
        DO 500 I=ISNB,ISSB
        IF (AHAT(I)-BHAT(I)*HGT(I,H).GE.0.0)
     1    RNETAC=RNETAC+(AHAT(I)-BHAT(I)*HGT(I,H))*SINHLF(I)
 500    CONTINUE
        RNETAC=RNETAC*SECPYR*DELTAX 
  
        POLFLX=ICEFAC*(MAX(0.,HGT(ISNB,H)+HGT(ISNB,HP)+HGT(ISNB-1,H)+ 
     1    HGT(ISNB-1,HP)))**5*((HGT(ISNB,H)-HGT(ISNB-1,H))/DELTAX)**3 
        POLFLX=SINHLF(ISNB-1)*POLFLX*SECPYR/32.0
  
        MSNAUT=ICEFAC*(MAX(0.,HGT(ISSB,H)+HGT(ISSB,HP)))**5*
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
  
      SECPYR  = 31557600.0
      READ (1,9006) ACMRAT
      ACMRAT  = ACMRAT/SECPYR 
      READ (1,9006) SLOPE 
      READ (1,9006) SNLAMP
      SNLAMP  = SNLAMP*1000.0 
      READ (1,9006) SNLMPT
      SNLMPT  = SNLMPT*1000.0 
      READ (1,9006) PERIOD
  
      READ (1,9006) RHOI
      READ (1,9006) RHOR
      READ (1,9006) TAU 
      BRLXTM = TAU*SECPYR 
      NU     = (1.-RHOI/RHOR)/BRLXTM
      ETA    = RHOI/(RHOR*BRLXTM)
       
      READ (1,9006) ICEFAC
      READ (1,9006) BNAUT
      READ (1,9006) BWON
      BWON=BWON/SECPYR
      READ (1,9006) ALPHA
      READ (1,9006) GAM
      READ (1,9008) MAXIT
      READ (1,9006) TLIMIT
      READ (1,9006) MHPOIS

      OSCARG  = 2.0*3.1415926535898/(PERIOD*SECPYR)
      MTPDEG = 111111.111111
      RADPDG = 3.1415926535898 / 180.0
      NBCLAT = 12.0*RADPDG


C     POINTERS
      H=0
      HP=1
      ISSB=MXSB-20
      ISNB=1

      TIMELP = 0.0

      RADSTP = GRDSTP * RADPDG
      DELTAT = TIMSTP * SECPYR
      DELTAX = GRDSTP * MTPDEG

C     ISHEET CONSTANTS

      DTNU   = DELTAT*NU
      DTET   = DELTAT*ETA
      DTAF2  = DELTAT/2.
      MU1    = 1.0 - DTET/2.0
      MU2    = 1.0 + DTET/2.0

      TRIPS=1+TRIPS
      DO 8000 I = 0, MXSB
        SINHLF(I) = SIN((I+0.5)*RADSTP+NBCLAT)
        DTDXSN(I)=ICEFAC*DELTAT/(64.*SIN(I*RADSTP+NBCLAT)*DELTAX**4)
 8000 CONTINUE

      HVIRT(0)=0.0
      HVIRT(1)=0.0

 9006 FORMAT (F14.4)

 9008 FORMAT (I5)

 9999 FORMAT (E15.7)

      RETURN

      END
