      PROGRAM eccen
C     Compute the eccentricity of the earth's form given
C       either the two radii or the flattening.
      DOUBLE PRECISION e, e2, f, r1, r2

      INTEGER i

      PRINT *,'Are you working with flattening (1) or radii (2)'
      READ (*,*) i

      IF (i .EQ. 1) THEN
        PRINT *,'What is the inverse of flattening?'
        READ (*,*) f
        f = 1./f
        e2 = f*(2.-f)
        e  = e2**0.5
        PRINT *,'Eccentricity squared ', e2, e
      ELSE
        PRINT *,'What is radius 1'
        READ (*,*) r1
        PRINT *,'What is radius 2'
        READ (*,*) r2
        f = (r1 - r2)/r1
        IF (f .LT. 0.) THEN
          f = (r2 - r1)/r2
        ENDIF
        e2 = f*(2.-f)
        e  = e2**0.5
        PRINT *,'e2, f = ', e2, e, 1./f
      ENDIF

      STOP
      END
        
        
      
