      REAL FUNCTION albedo(ts, ta, hs, hi)
!     Albedo of possibly snow covered sea ice.
!     Implementation of the algorithm from Ross and Walsh, 1987.
!     ts is surface temperature of the ice/snow in C
!     ta is the atmospheric temperature in C
!     hs is the snow thickness (in meters)
!     hi is the ice thickness (in meters)
!     In this algorithm, the ice thickness is not needed.  The
!       argument is passed for future modifications.  The snow
!       thickness is only needed to determine if there is snow
!       present.  It, too, is included for future experiments.
!     12 September 1994  Robert Grumbine - Original Implementation
!     24 January 1996 BG - Fix initial clause, hi, not hs.
!     25 November 2002 1996 BG - Re-Fix initial clause, hs, not hi.  (argh)

      IMPLICIT none

      REAL ts, ta, hs, hi

      IF (hs .GT. 0.) THEN
        IF (ts .LT. -5.) THEN
          albedo = 0.8
        ELSE IF (ts .LT. 0.) THEN
          albedo = 0.65 - 0.03*ts
        ELSE 
          albedo = 0.65
        ENDIF

       ELSE IF (hi .GT. 0.) THEN
        IF (ts .LT. 0.) THEN
          albedo = 0.65
         ELSE IF (ta .LT. 5.) THEN
          albedo = 0.65 - 0.04 * ta
         ELSE
          albedo = 0.45
        ENDIF

       ELSE
!        Ocean albedo.
         albedo = 0.10
 
      ENDIF
        
      RETURN
      END
