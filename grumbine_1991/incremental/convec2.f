C***********************************************************----------!!
      SUBROUTINE CONVEC(SD, nx, ny, TSTEP)
C     SUBROUTINE TO LOOK FOR CONVECTIVE OVERTURNING AND CABBELING.
C     OVERTURN ADDED 3-9-88.
C     USES SHORT FORM ENTRY POINT TO RHO 5-26-88.
C     Drop the passing of SS  7-16-90.
C     Note that this method is at least as fast as using sd = min(sd, 0)
C       - on the macintosh.  7-16-90.  don't know about other machines.
      IMPLICIT none
      INTEGER nx, ny, TSTEP
      REAL SD(nx, ny)

      INTEGER I, J
      LOGICAL OVRTRN, OVRLST
      SAVE    OVRLST

      OVRTRN = .FALSE.
      DO 1000 J = 1, ny
        DO 1010 I = 1, nx
          IF (SD(I,J) .GT. 0.0) THEN
            SD(I,J) = 0.0
            OVRTRN = .TRUE.
          ENDIF
 1010   CONTINUE
 1000 CONTINUE

      IF (TSTEP .EQ. 1) GO TO 9999

      IF (OVRTRN .AND. (.NOT. OVRLST) ) THEN
        PRINT *,'STARTED CONVECTION AT TSTEP',TSTEP
       ELSE IF ((.NOT. OVRTRN)  .AND. OVRLST ) THEN
        PRINT *,'STOPPED CONVECTION AT TSTEP',TSTEP
      ENDIF

 9999 CONTINUE
      OVRLST = OVRTRN

      RETURN
      END
