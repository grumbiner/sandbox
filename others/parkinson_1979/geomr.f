      SUBROUTINE GEOMR(LSTEP,IT,JT)
C
C  CALCULATING THE COSINE OF THE ZENITH ANGLE GIVEN THE TIME OF DAY
C  LSTEP OBTAINS NDAY, THE NUMBER OF THE DAY OF THE YEAR.
C  STIME=SOLAR TIME OF DAY.
C  NEED LSTEP, STIME,RLAT
      IMPLICIT none
      INTEGER LSTEP, IT, JT

      REAL RLAT, RLONG, DLAT, DLONG
      REAL COSZ, STIME, TIME, TSFLUX, XLAT(42,31), XLON(42,31)
      INTEGER NSTEP

      INTEGER J, NDAY
      REAL HA, DDEC, RDEC, ANG

      COMMON /C6/ RLAT,RLONG,DLAT,DLONG,NSTEP, 
     1  COSZ,STIME,TIME,TSFLUX,XLAT,XLON

      HA=(12.0-STIME)*0.2618
C  OBTAIN DECLINATION
      NDAY=1+LSTEP*30/NSTEP
      J=172-NDAY
      ANG=FLOAT(J)*0.01745
      DDEC=23.44*COS(ANG)
      RDEC=DDEC*0.01745
C  OBTAIN DESIRED COSINE OF THE ZENITH ANGLE
      COSZ=SIN(RLAT)*SIN(RDEC)+ COS(RLAT)*COS(RDEC)*COS(HA)
      IF (COSZ .LT. 0.0) COSZ=0.0
      RETURN
      END
