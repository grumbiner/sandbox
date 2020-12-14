      SUBROUTINE terp(globe, polar, pole, halfo)
C=======================================================================
C  Programmed by:
C     Robert W. Grumbine          NMC  Camp Springs, MD         Dec. '92
C  Purpose:
C     Interpolate from a spherical grid derived from the MRF to the
C       polar stereographic grid used by the ice model.
C  EXTERNALS:
C     W3FT32 -  NMC W3LIB routine to conduct interpolations between
C                 grids.  Variant modified by BG must be used.
C  Last Modified 10 September 1996
C=======================================================================
C     IMPLICIT none is a non-standard feature.  It compels 
C       all variables to be typed, on systems which recognize it.
      IMPLICIT none

      INCLUDE "icegrid.inc"
      INCLUDE "mgrid.inc"
C=======================================================================

C     Interpolation parameters
      INTEGER tlat, tlong
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm +1)

      REAL field(tlong+1, tlat/2+1), polar(0:L, 0:M)
      REAL globe(tlong, tlat)
      INTEGER interp, pole
      LOGICAL halfo

      INTEGER imaxin, jmaxin, ijout
      REAL xlat, wlon, xiin, xjin, d

C     Local variables. 
      INTEGER i, j
C=======================================================================

      DO 100 j = 1, tlat/2 + 1
        DO 110 i = 1, tlong
          field(i,j) = 0.0
  110   CONTINUE
  100 CONTINUE

C     Section off the polar domain (select the polar region)
 9001 FORMAT (I3)
      DO 2000 j = 1, tlat/2+1
        DO 2100 i = 1, tlong
          IF (pole .EQ. 1) THEN  ! North Hemisphere
            field(i,tlat/2+2-j) = globe(i,j)
           ELSE
            field(i,j) = globe(i,tlat+1-j)
          ENDIF
 2100   CONTINUE
          IF (pole .EQ. 1) THEN
            field(tlong+1,tlat/2+2-j) = field(1,tlat/2+2-j)
           ELSE
            field(tlong+1,j) = field(1,j)
          ENDIF
 2000 CONTINUE

C     Interpolate to the polar stereographic grid
      interp = 1
C     Changed from tempor to polar (0:L, 0:M grid rather than L,M
C       3/9/94, BG.
C     Describe the 1 degree grid (hemisphere)
      IMAXIN = (360./dlonm + 1)
      JMAXIN = (90./dlatm + 1)
      IJOUT = 0
      IF (.NOT. halfo) THEN
        DO 6740 J = 0, M
          DO 6740 I =0, L
            IF (PTYPE .EQ. 3) THEN
              CALL mapxy(i*dx+xorig, j*dy+yorig, xlat, WLON, 
     1                   slat, slon, sgn, SQRT(eccen2), rearth)
            ELSE IF (PTYPE .EQ. 2) THEN
CD              PRINT *,'in terp, ptype = 2'
              xlat = LATMIN + j*dlat
              WLON = lonmin + i*dlon
CD              PRINT *,xlat, wlon
            ELSE
              PRINT *,'Unsupported PTYPE in terp ',PTYPE
            ENDIF
            IF (sgn .LT. 0) xlat = xlat + 90.
            IF (WLON.GT.360.)  WLON = WLON - 360.
            IF (WLON.LT.0.)  WLON = WLON + 360.
            IF (WLON .LT. 0.) THEN
              PRINT *,'wlon = ',wlon
            ENDIF
            XIIN  = WLON/dlonm + 1.
            XJIN  = xlat/dlatm + 1.
            CALL W3FT01
     1          (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
            IJOUT = IJOUT + 1
            polar(i, j) = D
 6740   CONTINUE

      ELSE
        DO 6840 J = 1, M
          DO 6840 I = 1, L
            IF (PTYPE .EQ. 3) THEN
            CALL mapxy((i-.5)*dx+xorig, (j-.5)*dy+yorig, xlat, WLON,
     1                 slat, slon, sgn, SQRT(eccen2), rearth)
            ELSE IF (PTYPE .EQ. 2) THEN
              xlat = LATMIN + (j-0.5)*dlat
              WLON = lonmin + (i-0.5)*dlon
            ELSE
              PRINT *,'Unsupported PTYPE in terp ',PTYPE
            ENDIF
            IF (sgn .LT. 0) xlat = xlat + 90.
            IF (WLON.GT.360.)  WLON = WLON - 360.
            IF (WLON.LT.0.)  WLON = WLON + 360.
            XIIN  = WLON/dlonm + 1.
            XJIN  = xlat/dlatm + 1.
            CALL W3FT01
     1          (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
            IJOUT = IJOUT + 1
            polar(i, j) = D
 6840   CONTINUE

      ENDIF

      RETURN
      END
