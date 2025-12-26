      SUBROUTINE GENLL(POLEI,POLEJ,ALONVT,XMESHL,GDLAT,GDLON)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GENLL       COMPUTE (LAT,LON) FOR OUTPUT GRID
C   PRGRMMR: TREADON         ORG: W/NMC2     DATE: 92-12-23       
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
C USAGE:    CALL GENLL(IJOUT,JGOUT,POLEI,POLEJ,ALONVT,XMESHL)
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
C     MACHINE : CRAY Y-MP
C$$$  
C
C     
C     DECLARE VARIABLES.
C
      PARAMETER(IGOUT=147,JGOUT=111)
      LOGICAL NORTH
      REAL LAMBDA
      REAL GDLAT(IGOUT,JGOUT), GDLON(IGOUT,JGOUT)
C     
C     SET EARTH RADIUS.
      DATA EARTHR / 6371.2 /
      DATA NORTH /.TRUE./
C
      COMMON /IPOINT/ IPT,JPT
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
      RE     = (EARTHR * 1.86603) / XMESHL
      GI2    = RE * RE
C     
C     CASE 1:  POLAR STEROGRAPHIC PROJECTION.
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
               GDLON(I,J) = 360.0 - WLON
 10         CONTINUE
 20      CONTINUE
C     
C     END OF ROUTINE.
C
c     do j = 1,jgout
c      do i = 1,igout
         elon = 360.0 - GDLON(IPT,JPT)
         WRITE(6,10000) ipt,jpt,GDLAT(Ipt,Jpt),GDLON(Ipt,Jpt),
     1      elon
c            WRITE(52,10001) i,j,GDLAT(I,J),GDLON(I,J),elon
10000        FORMAT(2x,2i8,2x,f12.5,1x,f12.5,1x,f12.5)
10001        FORMAT(2X,i8,3(F12.5,1x))
c      end do
c     end do
      RETURN
      END
