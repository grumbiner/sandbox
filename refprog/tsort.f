      SUBROUTINE tsort(name, nref, n)
C     Sort author or journal names by the number of citations
C       to them.
C     Robert Grumbine 15 Dec 1994

      IMPLICIT none

      INTEGER n
      INTEGER nref(n)
      CHARACTER*24 name(n)

      INTEGER i, step
      INTEGER tn
      CHARACTER*24 tname
      LOGICAL change
 
      step = n/2
 100  CONTINUE
      change = .FALSE.
      DO 1000 i = 1, n-step
        IF (nref(i+step) .GT. nref(i)) THEN
          tn           = nref(i)
          nref(i)      = nref(i+step)
          nref(i+step) = tn
          tname        = name(i)
          name(i)      = name(i+step)
          name(i+step) = tname
          change = .TRUE.
        ENDIF
 1000 CONTINUE
      IF (change) THEN
        GO TO 100
       ELSE IF (step .GT. 1) THEN
        step = step/2
        GO TO 100
       ELSE
C       finished
      ENDIF

      RETURN
      END
