      PROGRAM tsedit
C     Set the non-land t, s, values to specified.
C     Needed because ts are currently being set via the Levitus
C       atlas, and the atlas has no information over the lake.
 
      INCLUDE "icegrid.glk"
 
      REAL vmask(L, M), smask(lp, mp), dmask(lp, mp)
      REAL dmask2(lp, mp, 2)
      REAL t(lp, mp), s(lp, mp)
      CHARACTER*60 fname
 
      PRINT *,'What is the name of the land mask file?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')

      CALL BCSINIT(vmask, smask, dmask, dmask2)
 
 9001 FORMAT (A60)
 9002 FORMAT (E13.6)

      PRINT *,'What is the name of the shallow file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      READ (11) t
      READ (11) s

      PRINT *,'What value do you want to reset the shallow temps to?'
      READ (*,9002) trevise
      srevise = 0.0

      DO 1100 j = 1, mp
       DO 1000 i = 1, lp
         IF (smask(i,j) .EQ. 1.0 ) THEN
           t(i,j) = trevise
           s(i,j) = srevise
          ELSE
           t(i,j) = 0.0
           s(i,j) = 0.0
         ENDIF
CD         PRINT *,i, j, t(i,j) 
 1000 CONTINUE
 1100 CONTINUE

      REWIND (11)
      WRITE (11) t
      WRITE (11) s

      PRINT *,'What is the name of the deep file?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      READ (12) t
      READ (12) s

      PRINT *,'What value do you want to reset the deep temps to?'
      READ (*,9002) trevise
      srevise = 0.0

      DO 2100 j = 1, mp
        DO 2000 i = 1, lp
         IF (smask(i,j) .EQ. 1.0 ) THEN
           t(i,j) = trevise
           s(i,j) = srevise
         ENDIF
 2000   CONTINUE
 2100 CONTINUE
     
      REWIND(12)
      WRITE (12) t
      WRITE (12) s

      STOP
      END
