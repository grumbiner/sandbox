CFPP$ NOCONCUR R
      SUBROUTINE CNVCLD(CLSTP,IM,RN,KBOT,KTOP,CV,CVB,CVT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CNVCLD      COMPUTES CONVECTIVE CLOUD COVER
C   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 91-05-07
C
C ABSTRACT: COMPUTES CONVECTIVE CLOUD COVER AND CLOUD TOPS AND BOTTOMS
C   AFTER THE DEEP CONVECTION IS INVOKED. CLOUD COVER IS INTERPOLATED
C   FROM A TABLE RELATING CLOUD COVER TO PRECIPITATION RATE.
C
C PROGRAM HISTORY LOG:
C   91-05-07  IREDELL
C
C USAGE:    CALL CNVCLD(CMEAN,LAT,IISTP,DT,RN,KBOT,KTOP,CV,CVB,CVT)
C
C   INPUT ARGUMENT LIST:
C     CMEAN    - REAL FLAG (GE 0 TO ACCUMULATE, EQ 99 TO RETURN VALUES)
C     LAT      - INTEGER LATITUDE INDEX
C     IISTP    - INTEGER TIME STEP NUMBER
C     DT       - REAL TIME STEP IN SECONDS
C     RN       - REAL (NX) CONVECTIVE RAIN IN METERS
C     KBOT     - INTEGER (NX) CLOUD BOTTOM LEVEL
C     KTOP     - INTEGER (NX) CLOUD TOP LEVEL
C
C   OUTPUT ARGUMENT LIST:
C     CV       - REAL (NX,NY) CONVECTIVE CLOUD COVER
C     CVB      - REAL (NX,NY) CONVECTIVE CLOUD BASE LEVEL
C     CVT      - REAL (NX,NY) CONVECTIVE CLOUD TOP LEVEL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77.
C   MACHINE:  CRAY.
C
C$$$
      DIMENSION RN(IM),KBOT(IM),KTOP(IM),CV(IM),CVB(IM),CVT(IM)
C  LOCAL WORK VARIABLES AND ARRAYS
      DIMENSION NMD(IM),PMD(IM)
C  LOCAL SAVE VARIABLES AND ARRAYS
      PARAMETER(NCC=9)
      DIMENSION CC(NCC),P(NCC)
      DATA CC/0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8/
      DATA P/.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
      DATA CVB0/100./
C-----------------------------------------------------------------------
C  INITIALIZE CONVECTIVE RAIN AND RANGE
      IF(CLSTP.LE.0..AND.CLSTP.GT.-10.) THEN
        DO I=1,IM
          CV(I)=0.
          CVB(I)=CVB0
          CVT(I)=0.
CWTAVG    CVB(I)=0.
CWTAVG    CVT(I)=0.
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C  ACCUMULATE CONVECTIVE RAIN AND RANGE
      IF(CLSTP.GT.-99.) THEN
        DO I=1,IM
          IF(RN(I).GT.0.) THEN
            CV(I)=CV(I)+RN(I)
            CVB(I)=MIN(CVB(I),FLOAT(KBOT(I)))
            CVT(I)=MAX(CVT(I),FLOAT(KTOP(I)+1))
CWTAVG      CVB(I)=CVB(I)+KBOT(I)*RN(I)
CWTAVG      CVT(I)=CVT(I)+(KTOP(I)+1)*RN(I)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C  CONVERT PRECIPITATION RATE INTO CLOUD FRACTION
      IF (CLSTP.GT.0..OR.(CLSTP.LT.0.AND.CLSTP.GT.-10.)) THEN
        DO I=1,IM
CWTAVG    IF(CV(I).GT.0.) THEN
CWTAVG      CVB(I)=NINT(CVB(I)/CV(I))
CWTAVG      CVT(I)=NINT(CVT(I)/CV(I))
CWTAVG    ELSE
CWTAVG      CVB(I)=CVB0
CWTAVG      CVT(I)=0.
CWTAVG    ENDIF
          PMD(I)=CV(I)*(24.E+3/ABS(CLSTP))
          NMD(I)=0
        ENDDO
        DO N=1,NCC
          DO I=1,IM
            IF(PMD(I).GT.P(N)) NMD(I)=N
          ENDDO
        ENDDO
        DO I=1,IM
          IF(NMD(I).EQ.0) THEN
            CV(I)=0.
            CVB(I)=CVB0
            CVT(I)=0.
          ELSEIF(NMD(I).EQ.NCC) THEN
            CV(I)=CC(NCC)
          ELSE
            CC1=CC(NMD(I))
            CC2=CC(NMD(I)+1)
            P1=P(NMD(I))
            P2=P(NMD(I)+1)
            CV(I)=CC1+(CC2-CC1)*(PMD(I)-P1)/(P2-P1)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
