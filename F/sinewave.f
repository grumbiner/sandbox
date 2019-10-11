      PROGRAM hello

      IMPLICIT none
      REAL x
      INTEGER i
      REAL pi

      pi = 3
      do i = 1, 1000
       pi = pi   + .001
       CALL fred(pi)
      end do

      END

      subroutine fred(pi)

      x = sin (360*(pi/180))
      PRINT *, 'sinewave', pi, sin(360*(pi/180))

      end subroutine
