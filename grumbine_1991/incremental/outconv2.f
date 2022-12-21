      PROGRAM prepgr
C      rewrite model output for input to NCSA graphics package.

      INCLUDE "grid.inc"
CD      IMPLICIT none
CD      INTEGER nx, ny
CD      PARAMETER (nx = 36)
CD      PARAMETER (ny = 36)
      REAL z(nx, ny)
      CHARACTER*1 zo(nx*ny)
      CHARACTER*60 fname
      LOGICAL yes
      REAL upper, lower
      INTEGER nstep, i, j, k, n, iref

 9001 FORMAT (A60)
 9002 FORMAT (E13.6)
 9003 FORMAT (I3)

    1 CONTINUE
        PRINT *,'What is the name of the file?'
        READ (*,9001) fname
        OPEN (UNIT=10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        iref = INDEX(fname, ' ')
        PRINT *,'What is the upper bound?'
        READ (*,9002) upper
        PRINT *,'What is the lower bound?'
        READ (*,9002) lower
        PRINT *,'How many steps are there?'
        READ (*,9003) nstep
        DO 1000 n =1, nstep
          READ (10) z
          k = 0
          DO 1010 j = ny, 1, -1
            DO 1020 i = 1, nx
              k = k+1

CD              zo(k) = CHAR( MOD( INT(256.* ((z(i,j)-lower)/
CD     1                   (ABS(lower)+ABS(upper)) ) ), 255 ) )
              zo(k) = CHAR( MOD( INT(256.*z(i,j)) , 255  )  )
 1020       CONTINUE
 1010     CONTINUE
          fname(iref  :iref  ) = CHAR( 65+n/16)
          fname(iref+1:iref+1) = CHAR( 65+MOD(n,16) )
C          Warning!! if n > 255, the character conversion will not work
          OPEN (UNIT=11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
          WRITE (11) zo
          CLOSE (UNIT=11)
 1000   CONTINUE

        PRINT *,'Would you like to process another file?'
        IF (yes(.FALSE.)) GO TO 1

      END
      FUNCTION yes(DEFALT)
C     FUNCTION TO RETURN .TRUE. IF THE USER RESPONDS Y, .FALSE. IF HE
C       SAYS N, AND THE DEFAULT VALUE OTHERWISE.

      LOGICAL YES, DEFALT
      CHARACTER RESP

      READ (*,9001) RESP
 9001 FORMAT(A1)

      YES = (RESP.EQ.'Y') .OR. (DEFALT .AND. RESP.NE.'Y'
     1                                 .AND. RESP.NE.'N')

      RETURN
      END
