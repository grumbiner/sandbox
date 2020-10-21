CD      SUBROUTINE terph(globe, polar, pole, halfo, dlatm, dlonm)
      SUBROUTINE terph(globe, polar, pole, halfo)
C=======================================================================
C  Programmed by:
C     Robert W. Grumbine          NMC  Camp Springs, MD         Dec. '92
C     Robert W. Grumbine          NMC  Camp Springs, MD         Mar. '94
C     Robert W. Grumbine          NCEP Camp Springs, MD         Jul. '95
C  Purpose:
C     Interpolate from a spherical grid derived from the MRF to the
C       polar stereographic grid used by the ice model.
C     This version to work on grids which start and 1/2 dx, dy 7/13/95.
C  EXTERNALS:
C     W3FT32 -  NMC W3LIB routine to conduct interpolations between
C                 grids.  Variant modified by BG must be used.
C  Last Modified 20 July 1995
C=======================================================================
C     IMPLICIT none is a non-standard feature.  It compels 
C       all variables to be typed, on systems which recognize it.
      IMPLICIT none

      INCLUDE "icegrid.inc"
      INCLUDE "mgrid.inc"
C=======================================================================

C     Interpolation parameters
CD      REAL dlonm, dlatm
      INTEGER tlat, tlong
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm )

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
      IMAXIN = tlong + 1
      JMAXIN = tlat/2 + 1
      IJOUT = 0
      IF (.NOT. halfo) THEN
        DO 6740 J = 0, M
          DO 6740 I =0, L
            CALL mapxy(i*dx+xorig, j*dy+yorig, XLAT, WLON, 
     1                 slat, slon, sgn, SQRT(eccen2), rearth)
            IF (sgn .LT. 0) XLAT = XLAT + 90.
            IF (WLON.GT.360.)  WLON = WLON - 360.
            IF (WLON.LT.0.)  WLON = WLON + 360.
            XIIN  = WLON/dlonm + 1.
            XJIN  = XLAT/dlatm + 1. + dlatm/2.
            CALL W3FT01
     1          (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
            IJOUT = IJOUT + 1
            polar(i, j) = D
 6740   CONTINUE

      ELSE
        DO 6840 J = 1, M
          DO 6840 I = 1, L
            CALL mapxy((i-.5)*dx+xorig, (j-.5)*dy+yorig, XLAT, WLON,
     1                 slat, slon, sgn, SQRT(eccen2), rearth)
            IF (sgn .LT. 0) XLAT = XLAT + 90.
            IF (WLON.GT.360.)  WLON = WLON - 360.
            IF (WLON.LT.0.)  WLON = WLON + 360.
            XIIN  = WLON/dlonm + 1.
            XJIN  = XLAT/dlatm + 1. + dlatm/2.
            CALL W3FT01
     1          (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
            IJOUT = IJOUT + 1
            polar(i, j) = D
 6840   CONTINUE

      ENDIF

      RETURN
      END
