      PROGRAM oisst
C     Read 1 degree OI sst fields in original (180x380) form.
C     Note that some non-standard things are done here.
C     Robert Grumbine 4 June 1997.

      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 380)
      PARAMETER (ny = 180)
      REAL anal(nx, ny), error(nx, ny)
      REAL oanal(360, 180), oerr(360, 180)
      INTEGER iyrst, imst, idst, iyrend, imend, idend, ndays, index
      INTEGER i, j
      INTEGER jo, io

      READ (11) iyrst, imst, idst, iyrend, imend, idend, ndays, index
      PRINT *, iyrst, imst, idst, iyrend, imend, idend, ndays, index
      READ (11) anal
      READ (11) error

      DO 2000 j = 1, ny
        jo = 181 - j
        DO 2100 i = 10, 369
          io = i - 189
          IF (io .LE. 0) io = io + 360

          oanal(io,jo) = anal(i,j)
          oerr(io,jo)  = error(i,j)
          PRINT *,'i,j,sst,error ', io, jo, oanal(io,jo), oerr(io,jo)
 2100   CONTINUE
 2000 CONTINUE

      WRITE (12) oanal
      WRITE (12) oerr

      STOP
      END
