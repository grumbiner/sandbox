      SUBROUTINE THERMO (NY,M,N,I,J,IT,JT,IHEMI)
C
      IMPLICIT none
      INTEGER NY, M, N, I, J, IT, JT, IHEMI

      REAL TEMP, FLON, DEW, VP, CLDD, DRAGW
      REAL AREA, AMIN, DT, DMIX, EYN, QS, QI, SIGMA, SK, TB, YK
      REAL FSEN, FSENW, FMI, DAY, SNO(12)
      REAL TS, TI, HI, HS, TW, WIND, TWI, FLAT, DRAG, FLATW, FLONW
      REAL RLAT, RLONG, DLAT, DLONG    
      INTEGER NSTEP
      REAL COSZ, STIME, TIME, TSFLUX, XLAT(42,31), XLON(42,31)
      REAL NCC, NCB

      COMMON /C1/ TEMP,FLON,DEW  ,VP,CLDD,DRAGW
      COMMON /C2/ AREA,AMIN,DT,DMIX,EYN,QS,QI,SIGMA,SK,TB,YK
      COMMON /C3/ FSEN,FSENW,FMI,DAY,SNO
      COMMON /C4/ TS,TI,HI,HS,TW,WIND,TWI,FLAT,DRAG,FLATW,FLONW
      COMMON /C6/ RLAT,RLONG,DLAT,DLONG,NSTEP,
     1COSZ,STIME,TIME,TSFLUX,XLAT,XLON
      COMMON /C7/ NCC,NCB

      REAL EMI, EMS, EMW, CW
      REAL AI, AS, AW, HMIN

      REAL FDMO, ALAST, HL, HSL
      REAL A1, TPAST, CON, EYE, FO, TT1, TT2, STPC, FA, FS
      REAL FLUXIN, TTRANS, T1T3

      DATA EMI/0.97/,EMS/0.99/,EMW/0.97/,CW/4.19E6/
      DATA AI/0.5/,AS/0.75/,AW/0.1/,HMIN/0.01/
      FDMO=0.0
C     FDMO=FMI
C ***************************
      ALAST=AREA
      HL=HI
      HSL=HS
C1 CASE OF NO ICE
      IF (HI.NE.0.0) GO TO 10
      TW=TW+DT*((1.0-AW)*TSFLUX+FSENW+FLATW+FLONW*EMW-SIGMA*(TW**4)*
     1 EMW+FDMO)/(DMIX*CW)
CD      PRINT *,'H .EQ. 0.0'
 
      IF (TW.GT.TB) GO TO 11
      HI=HMIN
      A1=1.0-(TB-TW)*DMIX*CW/(QI*HMIN)
      AREA=AMIN1(.999,A1)
      AREA=AMAX1(AMIN,AREA)
      TW=TB
      TWI=TB
      TS=TB
      GO TO 11
   10 CONTINUE

CD      PRINT *,'HI .NE. 0.0'
      TPAST=TS
      STPC=SIGMA*(TPAST**3)
C2 CASE OF ICE WITH NO SNOW
      IF (HS.NE.0.0) GO TO 12
C   SOLVE FOR SURFACE ICE TEMPERATURE
      CON=YK/HI
C     EYE=0.17*0.4
      EYE=0.068
      TI=TPAST+(FLAT+FSEN+FLON*EMI+(1.0-EYE)*(1.0-AI)*TSFLUX-STPC
     1 *TPAST*EMI +CON*(TB  -TPAST))/(4.*STPC*EMI+CON)
      TI=0.25*TI   +0.75*TPAST
      TI=AMIN1(TI,273.05)
      TS=TI
C   COMBINE FLUXES
      FA=-FLAT-FSEN-(1.0-EYE)*(1.0-AI)*TSFLUX+SIGMA*(TS**4)*EMI-FLON*EMI
      FO=YK*(TB-TI)/HI
      IF (NCC.NE.2) GO TO 21
      TT1=STPC*TPAST*EMI
      TT2=SIGMA*(TS**4)*EMI
      PRINT 27, FMI,FO,TT1,TT2     ,TI,TS,STPC,CON,FA
   27 FORMAT (4X,'ICE WITH NO SNOW,   OCEAN FLUX =',F11.6,', CONDUCTIVE
     2FLUX =',F11.6,', AND OUTGOING LONGWAVE FLUX =',F11.5,',', F11.5
     3 /2F9.4,3F15.5)
   21 CONTINUE
      IF(TI.EQ.273.05) GO TO 13
C   ALLOW SNOWFALL ON FROZEN ICE SURFACE
      IF (SNO(M) .GT. 0.0) HS=0.01
      IF(IHEMI.EQ.1.AND.M.EQ.8.AND.DAY.GT.20.) HS=0.01
      GO TO 14
   13 CONTINUE
C  MELT ICE IF SURFACE TEMPERATURE EXCEEDS  273.05K
      HI=HI+DT*(FA-FO)/QI
      HI=AMIN1(HI,HL)
      GO TO 14
   12 CONTINUE
C3 CASE OF SNOW-COVERED ICE
      CON=SK*YK/(HI*SK+HS*YK)
C   SOLVE FOR SNOW SURFACE TEMPERATURE
      TS=TPAST+(FLAT+FSEN+FLON*EMS+(1.0-AS)*TSFLUX-STPC*TPAST*EMS+(TB-
     1 TPAST)*CON)/(4.0*STPC*EMS+CON)
      TS=0.25*TS+0.75*TPAST
      IF(TS.GT.273.15) TS=273.15
C   SOLVE FOR SNOW-ICE INTERFACE TEMPERATURE
      TI=(HI*SK*TS +  HS*YK*TB  )/(HI*SK+   HS*YK)
C   COMPUTE FLUXES
      FA=-FLAT-FSEN-(1.0-AS)*TSFLUX+SIGMA*(TS**4)*EMS-FLON*EMS
      FS=SK*(TI-TS)/HS
      FO=FS
      IF (NCC.NE.2) GO TO 22
      TT1=STPC*TPAST*EMS
      TT2=SIGMA*(TS**4)*EMS
      PRINT 28, FMI,FO,TT1,TT2      ,TS,TI,CON,STPC,FA,FS
   28 FORMAT (4X,'SNOW-COVERED ICE,   OCEAN FLUX =',F11.6,', CONDUCTIVE
     2FLUX =',F11.6,', AND OUTGOING LONGWAVE FLUX =',F11.5,',',F11.5
     3 /2F9.4,4F15.5)
   22 CONTINUE
      IF(TS.GE.273.15) GO TO 15
C   WHEN SNOW IS NOT MELTING, UPDATE TEMPERATURE AND ADD SNOWFALL
      HS=HS+DT*SNO(M)
      IF(IHEMI.EQ.1.AND.M.EQ.8.AND.DAY.GT.20.)  HS=HS+DT*SNO(M+1)
      GO TO 14
   15 CONTINUE
C   CALCULATE SNOW MELT
      HS=HS+DT*(FA-FS)/QS
      HS=AMIN1(HS,HSL)
      IF (HS.LT.0.01) HS=0.0
      IF (FA.LE.FS) GO TO 14
      T1T3=DT*(FA-FS)/QS
      PRINT 8,NY,M,N,I,J,FA,FS,T1T3
    8 FORMAT (1X,5I3,3F20.10)
   14 CONTINUE
C2+C3 CONTINUED
C  CALCULATE BOTTOM ABLATION OR ACCRETION AND THE CHANGE IN LEAD AREA.
      HI=HI+DT*(FO-FMI)/QI
      IF (HI.GE.HL) GO TO 16
      HI=AMAX1(HI,0.0)
C ADJUST WATER TEMPERATURE FOR MELT
      TWI=(TWI*(DMIX-0.88*HL)+TB   *0.88*(HL-HI))/(DMIX-0.88*HI)
   16 CONTINUE
C CALCULATE LEAD TEMPERATURE AND CHANGE IN LEAD AREA
      FLUXIN=DT*((1.0-AW)*TSFLUX+FDMO-SIGMA*(TW**4)*EMW+FSENW+FLATW+
     1  FLONW*EMW)
      IF (FLUXIN.LT.0.0) GO TO 17
C IF FLUXIN IS POSITIVE,INCREASE THE WATER TEMPERATURE AND LEAD AREA
      TW=TW+FLUXIN*ALAST/(DMIX*CW)
      IF (HI.LT.0.001) GO TO 33
      AREA=ALAST+FLUXIN*ALAST*(1.0-ALAST)/(QI*HI+QS*HS)
   33 CONTINUE
      TTRANS=(TWI*(DMIX-0.88*HI)+TB*0.88*HI)/DMIX
      IF (AREA.GT.1.0) GO TO 19
      TW=(TW*ALAST+TTRANS*(AREA-ALAST))/AREA
      GO TO 20
   19 CONTINUE
      TW=TW*ALAST+TTRANS*(1.0-ALAST)
      TW=TW+(QI*HI+QS*HS)*(AREA-1.0)/(DMIX*CW)
      TWI=TW
      GO TO 26
C  IF FLUXIN IS NEGATIVE, DECREASE THE WATER TEMPERATURE
   17 CONTINUE
      TW=TW+FLUXIN/(DMIX*CW)
C  IF DECREASED WATER TEMPERATURE IS BELOW FREEZING, DECREASE LEAD AREA
      IF (TW.GE.TB) GO TO 20
      IF (HI.LT.0.001) GO TO 34
      AREA=ALAST-(TB-TW)*DMIX*CW*ALAST/(QI*HI)
   34 CONTINUE
      TW=TB
C IF LEAD AREA FALLS BELOW MINIMUM, COOL THE WATER
      IF (AREA.GE.AMIN) GO TO 23
      TWI=(TWI*(1.0-ALAST)+TW*(ALAST-AMIN))/(1.0-AMIN)
      TWI=TWI-(QI*HI*(AMIN-AREA))/(DMIX*CW*(1.0-AMIN ))
      AREA=AMIN
      IF (TWI.GE.TB) GO TO 20
      HI=HI+DMIX*CW*(TB-TWI)/QI
      TWI=TB
      GO TO 20
   23 CONTINUE
      TWI=(TWI*(1.0-ALAST)+TW*(ALAST-AREA))/(1.0-AREA)
   20 CONTINUE
C  ADJUST TEMPERATURES FOR LATERAL MIXING
      TWI=TWI+0.25*AREA*(TW-TWI)
      TW=TW+0.25*(1.0-AREA)*(TWI-TW)*(1.0-(0.88*HI/DMIX))
      IF (HI.LT.HMIN) GO TO 26
      GO TO 11
C  RESET VALUES WHEN ICE DISAPPEARS.
   26 CONTINUE
      HS=0.
      HI=0.
      AREA=1.0
      TS=TB
   11 CONTINUE
      IF (NCC.NE.2) GO TO 24
      TT1=SIGMA*(TW**4)*EMW
      TT2=TT2*(1.0-AREA)+TT1*AREA
      PRINT 25, AREA,TT1,TT2
   25 FORMAT (4X,'NEW LEAD AREA =',F7.4,',OUTGOING LW FROM LEADS =',
     2 F13.6,', GRID SQUARE OUTGOING LW =',F13.6)
   24 CONTINUE

      IF (TW .LT. 270.) THEN
        WRITE (*,9001) TW, DT, AW, TSFLUX, FSENW, FLATW, FLONW, EMW,
     1       SIGMA, FDMO, DMIX, CW,
     2       TB, FLUXIN, ALAST, AREA, TTRANS,
     3       QS, HS, QI, HI
 9001   FORMAT (F9.3,F9.0,4F9.3,/,'  ',2F9.3,E10.3,2F9.3,E10.3,
     1          /,'   ',F7.1,E13.6,3F8.3,E13.6,F8.3,E13.6,F8.3)
        WRITE (*,9002) AI, AS, AW, EMI, EMS, EMW, TI, TW, TS
 9002   FORMAT (8F9.3,F8.3)
        STOP
      ENDIF

      RETURN
      END
