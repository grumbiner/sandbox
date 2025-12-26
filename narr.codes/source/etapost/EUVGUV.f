      SUBROUTINE EUVGUV(U,V,VTLON,IMT,JMT,EVLAT,EVLON,
     1     ALATVT,ALVTOT,NRTHOT,PROJ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    EUVGUV      
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-23       
C     
C ABSTRACT:
C     THIS ROUTINE CONVERTS (ROTATES) U-V WIND COMPONENTS
C     ON THE ETA E-GRID TO U-V WIND COMPONENTS ON THE
C     SPECIFIED OUTPUT GRID.  IT ONLY DOES THE ROTATION,
C     NOT THE INTERPOLATION.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  ???
C   92-12-23  RUSS TREADON - ADDED COMMENTS AND GENERALIZED
C                            CODE TO HANDLE VARIABLE OUTPUT GRIDS.
C   93-06-13  RUSS TREADON - GENERALIZED ROUTINE TO HANDLE BOTH
C                            POLAR STEREOGRAPHIC AND LAT-LON 
C                            PROJECTIONS.
C     
C USAGE:    CALL EUVGUV(U,V,VTLON,IMT,JMT,EVLAT,EVLON,
C              ALVTOT,NRTHOT,PROJ)
C   INPUT ARGUMENT LIST:
C     U        - U WIND ON FILLED E-GRID
C     V        - V WIND ON FILLED E-GRID
C     VTLON    - V POINT LONGITUDES ON FILLED E-GRID
C     IMT      - FIRST DIMENSION OF FILLED E-GRID
C     JMT      - SECOND DIMENSION OF FILLED E-GRID
C     EVLAT    - FILLED E-GRID TRANSFORMED LATITUDES
C     EVLON    - FILLED E-GRID TRANSFORMED LONGITUDES
C     ALVTOT   - ROTATION LONGITUDE OF OUTPUT GRID
C     NRTHOT   - N/S HEMISPHERE LOGICAL SWITCH FOR OUTPUT GRID
C     PROJ     - OUTPUT GRID PROJECTION.
C
C   OUTPUT ARGUMENT LIST: 
C     U        - U WIND ROTATED TO OUTPUT GRID.
C     V        - V WIND ROTATED TO OUTPUT GRID.
C     
C   OUTPUT FILES:
C       NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - EGRID
C                  IOUNIT
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE ETA MODEL DIMENSIONS.
C---------------------------------------------------------------
      INCLUDE "parmeta"
C---------------------------------------------------------------
      PARAMETER (LP1=LM+1)
C---------------------------------------------------------------
C     
C     DECLARE VARIABLES.
      LOGICAL NORTH, NRTHOT
      CHARACTER*6 PROJ
      REAL ALONVT, DLAM, DPHI, LAM0, PHI0, POLEI
      REAL POLEJ, SINPH0, TANPH0, WLONC, XMESHL
      REAL YLATC
      REAL U(IMT,JMT),V(IMT,JMT),VTLON(IMT,JMT)
      REAL EVLAT(IMT,JMT), EVLON(IMT,JMT)
C---------------------------------------------------------------
      INCLUDE "EGRID.comm"
      INCLUDE "IOUNIT.comm"
C---------------------------------------------------------------
      EQUIVALENCE (YLATC,ALONVT), (DPHI, POLEI)
      EQUIVALENCE (WLONC, POLEJ), (DLAM,XMESHL)
C---------------------------------------------------------------
      DATA CONV2R/0.017453293/,CONV2D/57.2958/
      DATA EARTHR/6371.2/
C     
C**************************************************************
C     START EUVGUV HERE.
C     
C     CASE I:  ROTATE FROM E-GRID TO POLAR STEREOGRAPHIC GRID.
C     
      IF (INDEX(PROJ,'POLA').NE.0) THEN
         SPHI0 = SINPH0
!$omp  parallel do
!$omp& private(a,alpha,arg,cosa,elat,elon,sina,ue,ve)
         DO 10 J = 1, JMT
         DO 10 I = 1, IMT
            ELAT = EVLAT(I,J)
            ELON = EVLON(I,J)
	    ARG = SPHI0*SIN(VTLON(I,J)*CONV2R) /
     1           COS(ELAT*CONV2R)
	    IF(ABS(ARG).GT.1.0) THEN
		ARG = SIGN(1.0,ARG)  
            END IF
            ALPHA = ASIN(ARG)
            COSA = COS(ALPHA)
            SINA = SIN(ALPHA)
            UE   = U(I,J)*COSA+V(I,J)*SINA
            VE   = V(I,J)*COSA-U(I,J)*SINA
            A    = (ELON-ALVTOT)*CONV2R
            SINA = SIN(A)
            COSA = COS(A)
            IF (.NOT.NRTHOT) COSA = -COSA
            U(I,J) =  UE*COSA+VE*SINA
            V(I,J) = -UE*SINA+VE*COSA
 10      CONTINUE
C     
C     CASE II:  ROTATE FROM E-GRID TO LATITUDE-LONGITUDE GRID.
C        THE BELOW CODE IS FROM THE PROFILE OUTPUT CODE IN
C        SUBROUTINE OUTMAP OR EQUALLY SUBOMG.  NOTE THAT ELON
C        AND WLONC ARE BOTH IN DEGREES WEST.  WE WANT THE DIFFERENCE
C        IN DEGREES BETWEEN THE TWO.  WHAT MATTERS IS THAT ELON AND
C        WLONC ARE EITHER BOTH IN DEGREES EAST OR WEST.
C
      ELSEIF (INDEX(PROJ,'LOLA').NE.0) THEN
!$omp  parallel do
!$omp& private(alpha,arg,cosalp,dlm,elat,elon,sinalp,tlon,ue,ve,xx,yy)
         DO 20 J = 1,JMT
         DO 20 I = 1,IMT
            ELAT   =  EVLAT(I,J)
            ELON   =  EVLON(I,J)
            DLM    =  ELON-WLONC
            XX     =  COSPH0*COS(ELAT*CONV2R)*COS(DLM*CONV2R)+
     X                SINPH0*SIN(ELAT*CONV2R)
            YY     = -COS(ELAT*CONV2R)*SIN(DLM*CONV2R)
            TLON   = ATAN(YY/XX)
	    ARG = SINPH0*SIN(TLON)/COS(ELAT*CONV2R)
	    IF(ABS(ARG).GT.1.0)THEN
              ARG = SIGN(1.0,ARG)  
            ENDIF
            ALPHA  = ASIN(ARG)
            SINALP = SIN(ALPHA)
            COSALP = COS(ALPHA)
            UE     = U(I,J)
            VE     = V(I,J)
            U(I,J) = UE*COSALP+VE*SINALP
            V(I,J) = VE*COSALP-UE*SINALP
 20      CONTINUE
C
C     CASE III.  ROTATE FROM E-GRID TO LAMBERT CONFORMAL GRID.
C
      ELSEIF (INDEX(PROJ,'LMBC').NE.0) THEN
         SPHI0 = SINPH0
!$omp  parallel do
!$omp& private(a,alpha,arg,cone,cosa,elat,elon,sina,ue,ve)
         DO 30 J = 1, JMT
         DO 30 I = 1, IMT
            ELAT = EVLAT(I,J)
            ELON = EVLON(I,J)
	    ARG = SPHI0*SIN(VTLON(I,J)*CONV2R) /
     X           COS(ELAT*CONV2R)
	    IF(ABS(ARG).GT.1.0)THEN
              ARG = SIGN(1.0,ARG)  
            ENDIF
            ALPHA = ASIN(ARG)
            COSA = COS(ALPHA)
            SINA = SIN(ALPHA)
            UE   = U(I,J)*COSA+V(I,J)*SINA
            VE   = V(I,J)*COSA-U(I,J)*SINA
            CONE = SIN(ALATVT*CONV2R)
            A    = CONE*(ELON-ALVTOT)*CONV2R
            SINA = SIN(A)
            COSA = COS(A)
            IF (.NOT.NRTHOT) COSA = -COSA
            U(I,J) =  UE*COSA+VE*SINA
            V(I,J) = -UE*SINA+VE*COSA
 30      CONTINUE
C     
C     CASE IV.  OUTPUT WINDS ON ETA E_GRID.  DO NOTHING CASE.
      ELSE
         CONTINUE
C
      ENDIF
C     
C     END OF ROUTINE.
C
      RETURN
      END
