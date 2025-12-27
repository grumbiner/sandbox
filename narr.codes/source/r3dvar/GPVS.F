                            SUBROUTINE GPVS
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    GPVS        COMPUTE SATURATION VAPOR PRESSURE TABLE
C   AUTHOR: N PHILLIPS       W/NP2      DATE: 30 DEC 82
C
C ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF
C   TEMPERATURE FOR THE TABLE LOOKUP FUNCTION FPVS.
C   EXACT SATURATION VAPOR PRESSURES ARE CALCULATED IN SUBPROGRAM FPVSX.
C   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH
C   OF 7501 FOR TEMPERATURES RANGING FROM 180.0 TO 330.0 KELVIN.
C
C PROGRAM HISTORY LOG:
C   91-05-07  IREDELL
C   94-12-30  IREDELL             EXPAND TABLE
C   96-02-19  HONG                ICE EFFECT
C
C USAGE:  CALL GPVS
C
C SUBPROGRAMS CALLED:
C   (FPVSX)  - INLINABLE FUNCTION TO COMPUTE SATURATION VAPOR PRESSURE
C
C COMMON BLOCKS:
C   COMPVS   - SCALING PARAMETERS AND TABLE FOR FUNCTION FPVS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C----------------------------------------------------------------------
      PARAMETER(NX=7501)
      REAL TBPVS(NX),TBPVS0(NX)
      COMMON/COMPVS0/ C1XPVS0,C2XPVS0,TBPVS0
      COMMON/COMPVS/ C1XPVS,C2XPVS,TBPVS
C----------------------------------------------------------------------
      XMIN=180.0
      XMAX=330.0
      XINC=(XMAX-XMIN)/(NX-1)
      C1XPVS=1.-XMIN/XINC
      C2XPVS=1./XINC
      C1XPVS0=1.-XMIN/XINC
      C2XPVS0=1./XINC
C
      DO JX=1,NX
        X=XMIN+(JX-1)*XINC
        T=X
        TBPVS(JX)=FPVSX(T)
        TBPVS0(JX)=FPVSX0(T)
      ENDDO
C 
      RETURN
      END
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
                           FUNCTION FPVS(T)
C-----------------------------------------------------------------------
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    FPVS        COMPUTE SATURATION VAPOR PRESSURE
C   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
C
C ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
C   A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE
C   COMPUTED IN GPVS. SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
C   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
C   THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
C   ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
C   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
C
C PROGRAM HISTORY LOG:
C   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
C   94-12-30  IREDELL             EXPAND TABLE
C   96-02-19  HONG                ICE EFFECT
C
C USAGE:   PVS=FPVS(T)
C
C   INPUT ARGUMENT LIST:
C     T        - REAL TEMPERATURE IN KELVIN
C
C   OUTPUT ARGUMENT LIST:
C     FPVS     - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
C
C COMMON BLOCKS:
C   COMPVS   - SCALING PARAMETERS AND TABLE COMPUTED IN GPVS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C-----------------------------------------------------------------------
      PARAMETER(NX=7501)
      REAL TBPVS(NX)
      COMMON/COMPVS/ C1XPVS,C2XPVS,TBPVS
C-----------------------------------------------------------------------
      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))
C
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                         FUNCTION FPVS0(T)
C-----------------------------------------------------------------------
      PARAMETER(NX=7501)
      REAL TBPVS0(NX)
      COMMON/COMPVS0/ C1XPVS0,C2XPVS0,TBPVS0
C-----------------------------------------------------------------------
      XJ1=MIN(MAX(C1XPVS0+C2XPVS0*T,1.),FLOAT(NX))
      JX1=MIN(XJ1,NX-1.)
      FPVS0=TBPVS0(JX1)+(XJ1-JX1)*(TBPVS0(JX1+1)-TBPVS0(JX1))
C
      RETURN
      END
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
                         FUNCTION FPVSX(T)
C-----------------------------------------------------------------------
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    FPVSX       COMPUTE SATURATION VAPOR PRESSURE
C   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
C
C ABSTRACT: EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
C   THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS
C   FOR GAS AND LIQUID, AND NEGLECTS THE VOLUME OF THE LIQUID.
C   THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT
C   OF CONDENSATION WITH TEMPERATURE.  THE ICE OPTION IS NOT INCLUDED.
C   THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT
C   TO GET THE FORMULA
C       PVS=PSATK*(TR**XA)*EXP(XB*(1.-TR))
C   WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS
C   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
C
C PROGRAM HISTORY LOG:
C   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
C   94-12-30  IREDELL             EXACT COMPUTATION
C   96-02-19  HONG                ICE EFFECT 
C
C USAGE:   PVS=FPVSX(T)
C REFERENCE:   EMANUEL(1994),116-117
C
C   INPUT ARGUMENT LIST:
C     T        - REAL TEMPERATURE IN KELVIN
C
C   OUTPUT ARGUMENT LIST:
C     FPVSX    - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C-----------------------------------------------------------------------
      PARAMETER(CP=1.0046E+3,RD=287.04,RV=4.6150E+2 
     1,         TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2
     2,         CLIQ=4.1855E+3,CVAP= 1.8460E+3
     3,         CICE=2.1060E+3,HSUB=2.8340E+6)
      PARAMETER(PSATK=PSAT*1.E-3)
      PARAMETER(DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))
      PARAMETER(DLDTI=CVAP-CICE,XAI=-DLDTI/RV,XBI=XAI+HSUB/(RV*TTP))
C-----------------------------------------------------------------------
      TR=TTP/T
C
      IF(T.GE.TTP)THEN
        FPVSX=PSATK*(TR**XA)*EXP(XB*(1.-TR))
      ELSE
        FPVSX=PSATK*(TR**XAI)*EXP(XBI*(1.-TR))
      ENDIF
C 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                        FUNCTION FPVSX0(T)
C-----------------------------------------------------------------------
      PARAMETER(CP=1.0046E+3,RD=287.04,RV=4.6150E+2 
     1,         TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2
     2,         CLIQ=4.1855E+3,CVAP=1.8460E+3 
     3,         CICE=2.1060E+3,HSUB=2.8340E+6)
      PARAMETER(PSATK=PSAT*1.E-3)
      PARAMETER(DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))
      PARAMETER(DLDTI=CVAP-CICE,XAI=-DLDT/RV,XBI=XA+HSUB/(RV*TTP))
C-----------------------------------------------------------------------
      TR=TTP/T
      FPVSX0=PSATK*(TR**XA)*EXP(XB*(1.-TR))
C
      RETURN
      END
