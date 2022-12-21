C***********************************************************----------!!
      SUBROUTINE convec(ss, sd, nx, ny, tstep)
C     Subroutine to look for convective overturning and cabbeling.
C     Overturn added 3-9-88.
C     Uses short form entry point to rho 5-26-88.

      INTEGER nx, ny, tstep
      REAL ss(nx, ny), sd(nx, ny)

      INTEGER i, j
      LOGICAL ovrtrn, ovrlst
      SAVE    ovrlst

      ovrtrn = .FALSE.
      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
CF        rho1 = rhos1p(ss(i,j)+sd(i,j), 0.0, 0.)
CF        rho2 = rhos1p(ss(i,j)-sd(i,j), 0.0, 0.)
          IF (sd(i,j) .GT. 0.0) THEN
            sd(i,j) = 0.0
            ovrtrn = .TRUE.
          ENDIF
 1010   CONTINUE
 1000 CONTINUE

      IF (tstep .EQ. 1) GO TO 9999

      IF (ovrtrn .AND. (.NOT. ovrlst) ) THEN
        PRINT *,'Started convection at tstep',tstep
       ELSE IF ((.NOT. ovrtrn)  .AND. ovrlst ) THEN
        PRINT *,'Stopped convection at tstep',tstep
      ENDIF

 9999 CONTINUE
      ovrlst = ovrtrn

      RETURN
      END
