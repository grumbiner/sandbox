      PROGRAM delmet
C     Difference two binary meteorological forcing files
C     Robert Grumbine
C     Last Modified 15 April 1996

      IMPLICIT none

      INCLUDE "icegrid.inc"

      REAL tair(LP, MP), slp(LP, MP), rh(LP, MP), tsfc(LP, MP)
      REAL swdn(LP, MP), lwdn(LP, MP), lwup(LP, MP), prec(LP, MP)

      REAL del(LP, MP, 8)

      CHARACTER*60 fname

      READ (*,9001) fname
      WRITE (*,9001) fname
      OPEN (10, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")
 9001 FORMAT (A60)
      READ (*,9001) fname
      WRITE (*,9001) fname
      OPEN (11, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")

      del=0.

      READ (10) tair
      READ (10) slp
      READ (10) rh
      READ (10) tsfc
      READ (10) swdn
      READ (10) lwdn
      READ (10) lwup
      READ (10) prec
      
      CALL ar2set(del, tair, 1, LP, MP)
      CALL ar2set(del, slp, 2, LP, MP)
      CALL ar2set(del, rh, 3, LP, MP)
      CALL ar2set(del, tsfc, 4, LP, MP)
      CALL ar2set(del, swdn, 5, LP, MP)
      CALL ar2set(del, lwdn, 6, LP, MP)
      CALL ar2set(del, lwup, 7, LP, MP)
      CALL ar2set(del, prec, 8, LP, MP)

      READ (11) tair
      READ (11) slp
      READ (11) rh
      READ (11) tsfc
      READ (11) swdn
      READ (11) lwdn
      READ (11) lwup
      READ (11) prec

      CALL ar2del(del, tair, 1, LP, MP)
      WRITE (12) tair
      CALL ar2del(del, slp, 2, LP, MP)
      WRITE (12) slp
      CALL ar2del(del, rh, 3, LP, MP)
      WRITE (12) rh
      CALL ar2del(del, tsfc, 4, LP, MP)
      WRITE (12) tsfc
      CALL ar2del(del, swdn, 5, LP, MP)
      WRITE (12) swdn
      CALL ar2del(del, lwdn, 6, LP, MP)
      WRITE (12) lwdn
      CALL ar2del(del, lwup, 7, LP, MP)
      WRITE (12) lwup
      CALL ar2del(del, prec, 8, LP, MP)
      WRITE (12) prec

      STOP
      END

      SUBROUTINE ar2del(del, x, n, nx, ny)
      INTEGER n, nx, ny
      REAL x(nx, ny), del(nx, ny, 8)
      INTEGER i, j
   
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        x(i,j) = del(i,j,n) - x(i, j)
 1000 CONTINUE

      RETURN
      END

      SUBROUTINE ar2set(del, x, n, nx, ny)
      INTEGER n, nx, ny
      REAL x(nx, ny), del(nx, ny, 8)
      INTEGER i, j

      DO 1000 j = 1, ny
      DO 1010 i = 1, nx
        del(i,j,n) = x(i,j)
 1010 CONTINUE
 1000 CONTINUE
   
      RETURN
      END 
