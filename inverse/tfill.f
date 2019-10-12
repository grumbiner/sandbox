      SUBROUTINE tfill(ain, aout, nx, ny, nt)
C     Fill in no data and bad data points with first guesses
C     Work first through time
      IMPLICIT none

      INTEGER nx, ny, nt
      REAL ain(nx, ny, nt), aout(nx, ny, nt)

      INTEGER i, j, k

      INTEGER kl, kr
      REAL al, ar

      DO 999 k = 1, nt
      DO 1000 j = 1, ny
      DO 1100 i = 1, nx 
        IF (ain(i,j,k) .LT. 1.28 ) THEN
C         Do nothing
          aout(i,j,k) = ain(i,j,k)
        ELSE 
          kl = k
          kr = k
 2000     CONTINUE
            kl = kl-1
            IF (ain(i,j,kl) .GE. 1.28 .AND. kl .GT. 1) GO TO 2000
            IF (kl .EQ. 1) THEN 
              IF (ain(i,j,kl) .LT. 1.28) THEN
                al = ain(i,j,kl)
              ELSE
                DO 2001 kl = k+1, nt
                  IF (ain(i,j,kl) .LT. 1.28) THEN
                    al = ain(i,j,kl)
                    GO TO 2002
                  ENDIF
 2001           CONTINUE
                al = 0.
 2002           CONTINUE
              ENDIF

             ELSE
              al = ain(i, j, kl)
            ENDIF
 2100     CONTINUE
            kr = kr+1
            IF (ain(i,j, kr) .GE. 1.28 .AND. kr .LT. nt) GO TO 2100
            IF (kr .EQ. nt) THEN 
              IF (ain(i,j,kr) .LT. 1.28) THEN
                ar = ain(i,j,kr)
              ELSE
                DO 2101 kr = k-1, 1
                  IF (ain(i,j,kr) .LT. 1.28) THEN
                    ar = ain(i,j,kr)
                    GO TO 2102
                  ENDIF
 2101           CONTINUE
                ar = 0.
 2102           CONTINUE
              ENDIF
              
             ELSE
              ar = ain(i, j, kr)
            ENDIF
 2200     CONTINUE
C         Now have values to interpolate between.
          IF (kl .GT. k) THEN
            aout(i,j,k) = al
          ELSE IF (kr .LT. k) THEN
            aout(i,j,k) = ar
          ELSE
            aout(i,j, k) = ((kr-k)*al+(k-kl)*ar)/(kr-kl)
          ENDIF
 
        ENDIF

 1100   CONTINUE
 1000 CONTINUE
  999 CONTINUE

      RETURN
      END
