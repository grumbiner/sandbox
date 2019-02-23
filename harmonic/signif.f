      SUBROUTINE signif( x, y, n, alpmax, subtr, sfreq, nsfreq, scan )
C     Subroutine to find spectral lines which are significant according 
C       to the Fisher test, modified by Shimshoni, and using the Grumbine
C       algorithm.
C     Robert Grumbine 8-31-86 
C     Scan = true means to check the largest amplitude only (ie see if ANY-
C       thing is significant)
C     Robert Grumbine 12-9-87
C     Minor F90-ish updating -- implicit none, do loops 7 April 2014
      IMPLICIT none

      INTEGER n
      REAL x(n), y(n)
      INTEGER nsfreq
      REAL sfreq(500)
      REAL p
      LOGICAL subtr, scan
 
C     Local
      REAL bnot, bwon, avg, xbar, varian
      REAL stat(1000, 2), g, alpha, alpmax 
      INTEGER i, k, r

CD     PRINT *,'Computing line'
      CALL detrnd(x, y, n, bnot, bwon, avg, .FALSE.)
      IF (subtr) THEN
        xbar = .5*(x(1) + x(n))
        DO i = 1, n
          y(i) = y(i) - bwon*(x(i) - xbar)
        ENDDO
      ENDIF

CD     PRINT *,bnot, bwon, avg

CD     PRINT *,'Squaring the amplitudes.'
      varian = 0.0
      DO i = 1, n
        y(i)    = y(i) * y(i)
        varian  = varian + y(i)
      ENDDO
CD     PRINT *,'varian = ', varian

C     Sort into descending order.
CD     PRINT *,'Sorting the variances'
      CALL sort(x, y, n, 2, 1)

C     Compute the significance of each frequency.  First find all points
C       which are significant at alpmax/5, then find frequencies significant
C       at alpmax.
C     Using alpmax allows specification of tolerance by calling program.
      k = 1
      r = 1
      alpha = alpmax/ 5.
CD     PRINT *,'Applying the significance tests.'
 3000 CONTINUE
        g          = y(k) / varian
        stat(k, 1) = p(n, r, g)
        stat(k, 2) = FLOAT(r)
CD       PRINT *,k, stat(k, 1), stat(k, 2)
        IF ( (stat(k, 1) .LT. alpmax) .AND. scan) THEN
C         This is a pre-emptive check.  If any frequency is significant, stop 
C           now.  This is primarily of use for the global significance check,
C           since globally, several dozen freqs. may be significant, and for
C           the global check, we only need to know if any are. BG 12-9-87.
CD         PRINT *,'entered the test for scanning'
          nsfreq = 1
          sfreq(1) = x(1)
          RETURN
        ENDIF 

        IF ( stat(k, 1) .LT. alpha) THEN
C         The frequency has a statistically significant amplitude.
CD          PRINT *,'statistically significant'
           k = k + 1
         ELSE
          IF ( k .NE. r) THEN
C           Some points have been found to be significant since the last
C             time r was updated.
            r = k
           ELSE
C           No points have been found significant since the last pass.
C             increase alpha to alpmax if it hasn't already.
            IF ( alpha .NE. alpmax) THEN
              alpha = alpmax 
             ELSE
C             Done.
              GO TO 4000
            ENDIF
          ENDIF
        ENDIF
        GO TO 3000         

 4000 CONTINUE

C     Write out the significant frequencies.
CD     OPEN (1, FILE ='sigfreq', FORM='FORMATTED', STATUS='UNKNOWN')

CD     PRINT *,'    Frequency       Variance        P              r'
      nsfreq = 0
      DO i = 1, k
        IF (stat(i, 1) .GT. alpha) GO TO 5000
        nsfreq = nsfreq + 1
        sfreq(nsfreq) = x(i)
CD       WRITE (*,9001) x(i), y(i), stat(i, 1), stat(i, 2)
CD       WRITE (1,9001) x(i), y(i), stat(i, 1), stat(i, 2)
 5000 ENDDO

 9001 FORMAT (4(4X,E10.3))

      RETURN
      END
