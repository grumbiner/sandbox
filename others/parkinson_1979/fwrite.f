      SUBROUTINE FWRITE(x,iunit,j)
C     Emulate FWRITE, the IBM system routine for calling
C       tape writes.
C     For compatibility with Parkinson '79, '83 model.
C       By Bob Grumbine 2/13/92.
      REAL x(42,31)
      INTEGER iunit, j

      WRITE (iunit) x

      RETURN
      END
