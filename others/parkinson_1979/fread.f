      SUBROUTINE FREAD(x, iunit, i, j, k)
C     Emulate the calls to FREAD made by MAINPG, FREAD
C       is actually a system program to read from tapes.
C     For compatibility with Parkinson '79, '83 model.
C       by Bob Grumbine 2/13/92.
      REAL x(42,31)
      INTEGER i, j, k, iunit

      READ (iunit) x

      RETURN
      END
