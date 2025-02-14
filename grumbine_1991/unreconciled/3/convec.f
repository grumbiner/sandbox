C***********************************************************----------!!
      SUBROUTINE convec(sd, nx, ny, tstep)
C     Subroutine to look for convective overturning and cabbeling.
C     Overturn added 3-9-88.
C     Uses short form entry point to rho 5-26-88.
C     Drop the passing of SS  7-16-90.
C     Note that this method is at least as fast as using sd = min(sd, 0)
C       - on the macintosh.  7-16-90.  don't know about other machines.
      IMPLICIT none

      INTEGER nx, ny, tstep
      REAL sd(nx*ny)

      INTEGER  j
      LOGICAL ovrtrn, ovrlst
      SAVE    ovrlst

      ovrtrn = .FALSE.
      DO 1000 j = 1, nx*ny
        IF (sd(j) .GT. 0.0) THEN
          sd(j) = 0.0
          ovrtrn = .TRUE.
        ENDIF
 1000 CONTINUE

      IF (tstep .EQ. 1) GO TO 9999

CD      IF (ovrtrn .AND. (.NOT. ovrlst) ) THEN
CD        PRINT *,'Started convection at tstep',tstep
CD       ELSE IF ((.NOT. ovrtrn)  .AND. ovrlst ) THEN
CD        PRINT *,'Stopped convection at tstep',tstep
CD      ENDIF

 9999 CONTINUE
      ovrlst = ovrtrn

      RETURN
      END
