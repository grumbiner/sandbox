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
C$    ALATI       REAL*4       I     Geodetic Latitude (degrees, +90 to -90)  *
C$    ALONG      REAL*4        I     Geodetic Longitude (degrees, 0 to 360)   *
C$    X          REAL*4        O     Polar Stereographic X Coordinate (km)    *
C$    Y          REAL*4        O     Polar Stereographic Y Coordinate (km)    *
C$                                                                            *
C$                  Written by C. S. Morris - April 29, 1985                  *
C$                  Revised by C. S. Morris - December 11, 1985               *
C$                                                                     
C$                  Revised by V. J. Troisi - January 1990                  
C                   Modified by R. W. Grumbine - 6 April 1994
C      LAST MODIFIED 6 April 1994
C$                  SGN - provides hemisphere dependency (+/- 1)
C$*****************************************************************************
      IMPLICIT none

      REAL*4 X, Y, ALAT, ALONG, E, E2, CDR, PI, SLAT, SLON, MC,  ALATI
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
C Fix bug 11 December 1996, last exponent should be 1/2, not E/2.
        RHO=2.*RE*T/((1.+E)**(1.+E)*(1.-E)**(1.-E))**(1./2.)
      ELSE
        SL=SLAT*PI/180.
        TC=TAN(PI/4.-SL/2.)/((1.-E*SIN(SL))/(1.+E*SIN(SL)))**(E/2.)
        MC=COS(SL)/SQRT(1.0-E2*(SIN(SL)**2))
        RHO=RE*MC*T/TC
      END IF

C     BG change -- use degrees for along
      Y= RHO*SGN*SIN(SGN*(ALONG/cdr+SLON/CDR))
      X= RHO*SGN*COS(SGN*(ALONG/cdr+SLON/CDR))

  999 CONTINUE
      RETURN
      END
