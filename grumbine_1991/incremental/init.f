C*************************************************----------++++++++++!!
      SUBROUTINE INIT

      INTEGER xmax, ymax
      PARAMETER ( xmax = 45 )
      PARAMETER ( ymax = 42 )

      COMMON /main/ u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1              s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax),
     2              q(0:xmax+1, 0:ymax)

      REAL u, v, s, t, q


      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ybox, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ybox, outden
      LOGICAL debug


C     Local variables
      INTEGER i, j
      REAL sinit

C     Read in parameters.
      READ (4,9001) deltat
      READ (4,9001) deltax
      READ (4,9001) e
      READ (4,9001) r
      READ (4,9001) epsiln
      READ (4,9001) sigma
      READ (4,9002) xbox
      READ (4,9002) ybox
      READ (4,9001) tend
      READ (4,9001) sinit
      READ (4,9003) debug
      READ (4,9002) outden

      WRITE (6,9001) deltat
      WRITE (6,9001) deltax
      WRITE (6,9001) e
      WRITE (6,9001) r
      WRITE (6,9001) epsiln
      WRITE (6,9001) sigma
      WRITE (6,9002) xbox
      WRITE (6,9002) ybox
      WRITE (6,9001) tend
      WRITE (6,9001) sinit
      WRITE (6,9003) debug
      WRITE (6,9002) outden

      DO 1000 j = 0, ymax
        DO 1010 i = 0, xmax + 1

          IF (i .GT. xbox .AND. j .LT. ybox) THEN
            s(i, j) = 0.0
           ELSE
C           not on continent
            s(i, j) = sinit
          ENDIF

          t(i, j) = 0.0
          u(i, j) = 0.0
          v(i, j) = 0.0

 1010   CONTINUE
 1000 CONTINUE

C     This is the place to initialize selected points to non-zero values
C     v(xbox  , 6) = 1
C     u(xbox-1, 6) = 1

C     u(1, 15) = -1.0

 9001 FORMAT (F8.4)

 9002 FORMAT (I5)

 9003 FORMAT (L10)

      RETURN
      END
