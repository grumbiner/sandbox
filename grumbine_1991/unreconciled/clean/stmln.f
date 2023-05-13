      SUBROUTINE stmln(u, v, l, n)

C     Subroutine to plot arrows in the direction of velocity streamlines
      INTEGER l, n
      REAL u(l, n), v(l, n)

C     Local variables.
      INTEGER uunit, vunit, tstep, tdes
      LOGICAL yes, movie
      INTEGER frames

CC    WRITE (*,9003)
CC    IF (yes(.FALSE.)) CALL JCLR

      WRITE (*,9005)
      READ  (*,9006) uunit, vunit

      WRITE (*,9007)
      READ  (*,9002) tdes

      PRINT *,'Would you like to see this as a movie?'
      movie = yes(.FALSE.)
      IF (movie) THEN
        PRINT *,'How many frames do you want?'
        READ (*,9002) frames
      ENDIF

 1000 CONTINUE
        READ (uunit) tstep
        CALL read2 (u, l, n, uunit, .FALSE.)
        IF (tstep .LT. tdes) GO TO 1000

 2000 CONTINUE
        READ (vunit) tstep
        CALL read2 (v, l, n, vunit, .FALSE.)
        IF (tstep .LT. tdes) GO TO 2000

        CALL EZVEC(u, v, l, n)

C       Call JMCUR to ensure that everything has been plotted.
CD      CALL JMCUR

        IF (movie .AND. tdes .LT. frames) THEN
          tdes = tdes + 1
          GO TO 1000
        ENDIF

 9002 FORMAT (I3)

 9003 FORMAT (' Would you like to clear the screen (y or n) ?')

 9005 FORMAT ('Which file numbers do you want to use as the sources for
     1u, and v.  (Enter two    integers on the same line please.)')

 9006 FORMAT (2I3)

 9007 FORMAT ('At what time step do you want the data? (The program will
     1 read until it finds a time step >= this.) ')

      RETURN
      END
