      SUBROUTINE SHWARA(IIC)
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL            MPI, HAMBURG                           1987
C  MODIFIED BY:
C     -A.STOESSEL            MPI, HAMBURG                           1989
C  PURPOSE:
C     -CALCULATION OF COSINE OF ZENITH DISTANCE AND SOLAR CONSTANT
C  METHOD:
C     -THE DAILY MEAN SOLAR RADIATION IS DETERMINED BY AVERAGING THE
C       HOURLY CALCULATED COSINE OF ZENITH DISTANCE IN ORDER TO OPTIMIZE
C       THE ABL-ROUTINES
C  INTERFACE:
C     -IIC: CURRENT TIME STEP=DAY OF INTEGRATION
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE DAY OF THE YEAR
C-----------------------------------------------------------------------
C**MODIFICATION FOR ACTUAL DAILY FORCING VALUES:
      DJ=MOD(IIC,365)*360./365.
C-----------------------------------------------------------------------
C  DECLINATION OF THE SUN
C-----------------------------------------------------------------------
      DECL=23.44*COS((170.6-DJ)*RAD)
      SINDECL=SIN(DECL*RAD)
      COSDECL=COS(DECL*RAD)
C-----------------------------------------------------------------------
C  DETERMINE SOLAR CONSTANT
C-----------------------------------------------------------------------
      SOL=1353.*(1.000110+0.034221*COS(DJ*RAD)+0.001280*SIN(DJ*RAD)
     1+0.000718*COS(2.*DJ*RAD)+0.000077*SIN(2.*DJ*RAD))
C-----------------------------------------------------------------------
C  START CALCULATING COSINE OF ZENITH DISTANCE AS FUNCTION OF LATITUDE
C-----------------------------------------------------------------------
      DO 2 J=0,M
       TZ=0.
       SCOSZ=0.
C  SUM UP HOURLY VALUES:
       DO 1 IT=1,24
        TZ=TZ+1.
C  HOUR ANGLE:
        HA=(12.2-TZ)*PI/12.
C  COSINE OF ZENITH DISTANCE:
        COSZ(J)=SINPHI(J)*SINDECL+COSPHI(J)*COSDECL*COS(HA)
        COSZ(J)=MAX(COSZ(J),0.)
        SCOSZ=SCOSZ+COSZ(J)
    1  CONTINUE
C  DAILY MEAN COSINE OF ZENITH DISTANCE:
       COSZ(J)=SCOSZ/24.
   2  CONTINUE
      RETURN
      END
