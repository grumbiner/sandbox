      SUBROUTINE GENLL(GDLAT,GDLON)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GENLL       COMPUTE (LAT,LON) FOR OUTPUT GRID
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-23       
C     
C ABSTRACT:
C     GIVEN THE STANDARD NMC GRID SPECIFICATIONS THIS ROUTINE
C     COMPUTES THE GEODETIC (LAT,LON) OF THE GRID POINTS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  DAVID PLUMMER - SUBROUTINE CGTLL IN ETAPACKC
C   93-02-26  RUSS TREADON  - EXTRACTED THIS CODE FROM CGTLL
C                             AND GENERALIZED TO HANDLE
C                             VARIOUS OUTPUT GRIDS.
C   93-06-13  RUSS TREADON  - ADDED LOLA PROJECTION.
C     
C USAGE:    CALL GENLL(GDLAT,GDLON)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     GDLAT    - GEODETIC LATITUDE OF OUTPUT GRID POINTS.
C     GDLON    - GEODETIC LONGITUDE OF OUTPUT GRID POINTS.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - OUTGRD
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C     
C     INCLUDE/SET PARAMETERS.
C
      INCLUDE "parmeta"
      INCLUDE "parmout"
C     
C     DECLARE VARIABLES.
C
      LOGICAL NORTH
      CHARACTER*6 PROJ
      REAL LAMBDA
      REAL GDLAT(IMX,JMX), GDLON(IMX,JMX)
C     
C     INCLUDE OUTPUT GRID COMMON BLOCK.
      INCLUDE "OUTGRD.comm"
C
C     SET EARTH RADIUS.
      DATA EARTHR /6371.2/
C     
C*********************************************************************
C     START GENLL HERE.
C
C     SET CONSTANTS.
      PI     = ACOS(-1.)
      HALFPI = PI/2.
      TWOPI  = 2.*PI
      D2R    = PI/180.
      R2D    = 1./D2R
C     
C     CASE 1:  POLAR STEROGRAPHIC PROJECTION.
C     
      IF (INDEX(PROJ,'POLA').NE.0) THEN
C
C        COMPUTE GEODETIC (LAT,LON) FOR OUTPUT GRID.
         DO 20 J = 1,JGOUT
            DO 10 I = 1,IGOUT
               XI   = REAL(I)
               YJ   = REAL(J)
               DELX = XI-POLEI
               DELY = YJ-POLEJ
               R2   = DELX*DELX+DELY*DELY
C
C              VARIABLES YLAT AND WLON ARE THE GEODETIC 
C              LATITUDE (POSITIVE NORTH) AND LONGITUDE 
C              (POSITIVE WEST) OF THE OUTPUT GRID POINTS.
C
               IF (R2.NE.0.) THEN
                  TLON = R2D*ATAN2(DELY,DELX)
                  IF (NORTH) THEN
                     WLON = ALONVT-90.-TLON
                     YLAT = ASIN((GI2-R2)/(GI2+R2))*R2D
                  ELSE
                     WLON = ALONVT+90.+TLON
                     YLAT = -ASIN((GI2-R2)/(GI2+R2))*R2D
                  ENDIF
                  IF (WLON.GT.360.) WLON = WLON-360.
                  IF (WLON.LT.0.)   WLON = WLON+360.
               ELSE
                  YLAT = 90.
                  IF (.NOT.NORTH) YLAT = -90.
                  WLON = ALONVT
               ENDIF
               GDLAT(I,J) = YLAT
               GDLON(I,J) = WLON
 10         CONTINUE
 20      CONTINUE
C     
C     CASE II:  LATITUDE-LONGITUDE PROJECTION.
C     
      ELSEIF (INDEX(PROJ,'LOLA').NE.0) THEN
         SWLAT = ALONVT
         SWLON = POLEJ
         DO 40 J = 1,JGOUT
            DLAT = (J-1)*POLEI
            DO 30 I = 1,IGOUT
               DLON = (I-1)*XMESHL
               GDLAT(I,J) = SWLAT + DLAT
               GDLON(I,J) = SWLON - DLON
 30         CONTINUE
 40      CONTINUE
C
C     CASE III:  LAMBERT CONFORMAL (TANGENT) PROJECTION).
C
      ELSEIF (INDEX(PROJ,'LMBC').NE.0) THEN
C
C        CONVERT WEST LONGITUDE TO EAST.
         POLEJE  = -1.*POLEJ
         ALONVTE = -1.*ALONVT
C
C        COMPUTE TANGENT CONE CONSTANT AND FACTOR A.
         PSIT = HALFPI - ABS(ALATVT*D2R)
         CONE = COS(PSIT)
         A    = EARTHR/CONE
C
C        COMPUTE LINEAR COORDINATES CORRESPONDING TO
C        GRIDPOINT (1,1)=(POLEI,POLEJE).
         PHI    = POLEI*D2R
         LAMBDA = POLEJE*D2R
         IF (NORTH) THEN
            PSI = HALFPI - PHI
         ELSE
            PSI = HALFPI + PHI
         ENDIF
         TERMA  = PSI/2.
         TERMB  = CONE*(LAMBDA-ALONVTE*D2R)
         X1     = A * TAN(TERMA)**CONE * SIN(TERMB)
         Y1     = A * TAN(TERMA)**CONE * COS(TERMB)
         IF (NORTH) Y1 = -1. * Y1
C
C        LOOP TO COMPUTE GEODETIC (LAT,LON).
         ALPHA = (TAN(PSIT/2.)**CONE) / SIN(PSIT)
         DO 60 J = 1,JGOUT
            DO 50 I = 1,IGOUT
               X = X1 + (I-1)*ALPHA*XMESHL
               Y = Y1 + (J-1)*ALPHA*XMESHL
C
               TERM = (SQRT(X*X+Y*Y)/A)**(1./CONE)
               GPHI = HALFPI - 2.*ATAN(TERM)
C
               IF (.NOT.NORTH) GPHI = -1.*GPHI
               IF (NORTH) THEN
                  GTHETA = ATAN2(X,-1.*Y)
               ELSE
                  GTHETA = ATAN2(X,Y)
               ENDIF
C
               GLAM = ALONVTE*D2R + GTHETA/CONE
               IF (GLAM.GT.PI)     GLAM = GLAM - TWOPI
               IF (GLAM.LT.-PI) GLAM = GLAM + TWOPI
C
               GDLAT(I,J) = GPHI*R2D
               GDLON(I,J) = ABS(GLAM*R2D)
C
 50         CONTINUE
 60      CONTINUE
C     
C     CASE IV:  ETA PROJECTION (TRANSFORMED LAT-LON).
C        (DO NOTHING HERE, SINCE THE MODEL PROVIDES GDLAT,GDLON.)
C     
      ENDIF
C     
C     END OF ROUTINE.
C
      RETURN
      END
