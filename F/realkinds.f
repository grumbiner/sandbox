      PROGRAM alpha
      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 100)
      PARAMETER (ny = 100)
      REAL x
      REAL*8 x8
      REAL(SELECTED_REAL_KIND(15,307)) xk(nx, ny)

      REAL(SELECTED_REAL_KIND(p=15)) x15(nx, ny)
      REAL(SELECTED_REAL_KIND(p=16)) x16(nx, ny) !pgf90 does not allow here down
      REAL(SELECTED_REAL_KIND(p=18)) x18(nx, ny) !gfortran allows
!      REAL(SELECTED_REAL_KIND(p=19)) x19(nx, ny) !gfortran does not allow here down
!      REAL(SELECTED_REAL_KIND(p=22)) x22(nx, ny)
!      REAL(SELECTED_REAL_KIND(p=25)) x25(nx, ny)
!      REAL(SELECTED_REAL_KIND(p=28)) x28(nx, ny)
!      REAL(SELECTED_REAL_KIND(p=33)) x33(nx, ny) !ifort allows everything down to here
!      REAL(SELECTED_REAL_KIND(p=34)) x34(nx, ny)

      OPEN(15,FILE="f15", FORM="UNFORMATTED", STATUS="NEW")
      WRITE(15) x15
      CLOSE(15)
      OPEN(16,FILE="f16", FORM="UNFORMATTED", STATUS="NEW")
      WRITE(16) x16
      CLOSE(16)
      OPEN(18,FILE="f18", FORM="UNFORMATTED", STATUS="NEW")
      WRITE(18) x18
      CLOSE(18)
!      OPEN(28,FILE="f28", FORM="UNFORMATTED", STATUS="NEW")
!      WRITE(28) x28
!      CLOSE(28)
!      OPEN(31,FILE="f31", FORM="UNFORMATTED", STATUS="NEW")
!      WRITE(31) x31
!      CLOSE(31)
!      OPEN(32,FILE="f32", FORM="UNFORMATTED", STATUS="NEW")
!      WRITE(32) x32
!      CLOSE(32)
!      OPEN(33,FILE="f33", FORM="UNFORMATTED", STATUS="NEW")
!      WRITE(33) x33
!      CLOSE(33)
!      OPEN(34,FILE="f34", FORM="UNFORMATTED", STATUS="NEW")
!      WRITE(34) x34
!      CLOSE(34)

      END
