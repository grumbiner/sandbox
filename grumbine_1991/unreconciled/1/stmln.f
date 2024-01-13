C***********************************************************----------!!
      SUBROUTINE stmln(u, v, l, n)
C     Subroutine to plot arrows in the direction of velocity streamlines

      INTEGER l, n
      REAL u(l, n), v(l, n)

C     Local variables.
      INTEGER uunit, vunit, tstep, tdes
      LOGICAL yes, movie
      INTEGER frames

      WRITE (*,9005)
      READ  (*,9006) uunit, vunit

      WRITE (*,9007)
      READ  (*,9002) tdes
      tstep = tdes

 1000 CONTINUE
        CALL read2 (u, l, n, uunit, .FALSE.)
        IF (tstep .LT. tdes) GO TO 1000

 2000 CONTINUE
        CALL read2 (v, l, n, vunit, .FALSE.)
        IF (tstep .LT. tdes) GO TO 2000

        CALL EZVEC(u, v, l, n)

 9002 FORMAT (I3)

 9005 FORMAT ('Which file numbers do you want to use as the sources for
     1u, and v.  (Enter two    integers on the same line please.)')

 9006 FORMAT (2I3)

 9007 FORMAT ('At what time step do you want the data? (The program will
     1 read until it finds a time step >= this.) ')

      RETURN
      END
