      PROGRAM aatopo
C     Read in antarctic surface elevation fields and write out on
C       latitude-longitude grid for incorporation into MRF topography
C       file.
C     Robert Grumbine 30 June 1995
 
      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 281)
      PARAMETER (ny = 281)
      REAL height(0:nx-1,0:ny-1)
      REAL minutes, latmax, latmin
      PARAMETER (minutes = 10.0)
      PARAMETER (latmin  = -90. + minutes/60./2.)
      PARAMETER (latmax  = -50. + minutes/60./2.)
      INTEGER jcnt, icnt
      PARAMETER (jcnt = (latmax - latmin) * 60. / minutes )
      PARAMETER (icnt = 360. * 60. / minutes )
      REAL elev(1:icnt, 1:jcnt)
      REAL slat, slon, sgn, eccen2, rearth, dx, dy, xorig, yorig
      PARAMETER (slat =   60.0)
      PARAMETER (slon =  -90.0)
      PARAMETER (sgn  =  -1.0 )
      PARAMETER (eccen2 = 0.006693883)
      PARAMETER (rearth = 6378.273E3)
      PARAMETER (dx     = 20.E3)
      PARAMETER (dy     = 20.E3)
      PARAMETER (xorig  = -140.*dx)
      PARAMETER (yorig  = -140.*dy)

      
      INTEGER i, j, ix, jy
      REAL llat, llon, xpt, ypt, x

      OPEN (10, FILE="surf", FORM="FORMATTED", STATUS="OLD")

      READ (10,9001) (height(0,j),j=0,ny-1)
      WRITE (*,9001) (height(0,j),j=0,ny-1)
      DO 1000 j = 0, ny-1
        READ(10,9002) x, (height(i,j),i=0,nx-1)
        WRITE(*,9002) x, (height(i,j),i=0,nx-1)
 1000 CONTINUE

 9001 FORMAT (281F5.1)
 9002 FORMAT (F5.1, 281E10.3)

      DO 2000 j = 1, jcnt
        llat = latmin + (j-1)*minutes/60.
        DO 2100 i = 1, icnt
          llon = minutes/60./2. + (i-1)*minutes/60.

          CALL mapll(xpt, ypt, ABS(llat), llon, slat, slon, sgn, 
     1                SQRT(eccen2), rearth )

          ix = NINT( (-xpt - xorig)/dx ) 
          jy = NINT( ( ypt - yorig)/dy )
CD          PRINT *,'i, j, ix, jy',llat, llon, ix, jy
          IF (ix .GE. 0 .AND. ix .LT. nx .AND.
     1        jy .GE. 0 .AND. jy .LT. ny)    THEN
            elev(i, j) = height(ix, jy) * 1000.
          ELSE
            elev(i,j) = -9999.
          ENDIF
 2100   CONTINUE
 2000 CONTINUE

      WRITE (11) elev

      STOP
      END
      SUBROUTINE mapll (X, Y, ALATI, ALONG, SLAT, SLON, SGN, E, RE)
C$*****************************************************************************
C$                                                                            *
C$    DESCRIPTION:                                                            *
C$                                                                            *
C$    This subroutine converts from geodetic latitude and longitude to Polar  *
C$    Stereographic (X,Y) coordinates for the polar regions.  The equations   *
C$    are from Snyder, J. P., 1982,  Map Projections Used by the U.S.         *
C$    Geological Survey, Geological Survey Bulletin 1532, U.S. Government     *
C$    Printing Office.  See JPL Technical Memorandum 3349-85-101 for further  *
C$    details.                                                                *
C$                                                                            *
C$    ARGUMENTS:                                                              *
C$                                                                            *
C$    Variable    Type        I/O    Description                              *
C$                                                                            *
C$    ALATI       REAL         I     Geodetic Latitude (degrees, +90 to -90)  *
C$    ALONG      REAL          I     Geodetic Longitude (degrees, 0 to 360)   *
C$    X          REAL          O     Polar Stereographic X Coordinate (km)    *
C$    Y          REAL          O     Polar Stereographic Y Coordinate (km)    *
C$                                                                            *
C$                  Written by C. S. Morris - April 29, 1985                  *
C$                  Revised by C. S. Morris - December 11, 1985               *
C$                                                                     
C$                  Revised by V. J. Troisi - January 1990                  
C                   Modified by R. W. Grumbine - 6 April 1994
C$                  SGN - provides hemisphere dependency (+/- 1)
C$*****************************************************************************
      IMPLICIT none

      REAL   X, Y, ALAT, ALONG, E, E2, CDR, PI, SLAT, SLON, MC,  ALATI
      REAL RE, RHO, SGN, SL, T, TC
C$*****************************************************************************
C$                                                                            *
C$    DEFINITION OF CONSTANTS:                                                *
C$    Conversion constant from degrees to radians = 57.29577951.              *
      CDR=57.29577951
      E2=E*E
      PI=3.141592654
C BG Modification -- Use degrees!!
      alat = alati / cdr
C$*****************************************************************************
C     Compute X and Y in grid coordinates.
      IF (ABS(ALAT).LT.PI/2.) GOTO 250
      X=0.0
      Y=0.0
      GOTO 999
  250 CONTINUE
      T=TAN(PI/4.-ALAT/2.)/((1.-E*SIN(ALAT))/(1.+E*SIN(ALAT)))**(E/2.)

C     BG reduce tolerance from 1E-5 to 1E-3.  1/31/94
      IF (ABS(90.-SLAT).LT.1.E-3) THEN
        RHO=2.*RE*T/((1.+E)**(1.+E)*(1.-E)**(1.-E))**(E/2.)
      ELSE
        SL=SLAT*PI/180.
        TC=TAN(PI/4.-SL/2.)/((1.-E*SIN(SL))/(1.+E*SIN(SL)))**(1/2.)
        MC=COS(SL)/SQRT(1.0-E2*(SIN(SL)**2))
        RHO=RE*MC*T/TC
      END IF

C     BG change -- use degrees for along
      Y= RHO*SGN*SIN(SGN*(ALONG/cdr+SLON/CDR))
      X= RHO*SGN*COS(SGN*(ALONG/cdr+SLON/CDR))

  999 CONTINUE
      RETURN
      END
