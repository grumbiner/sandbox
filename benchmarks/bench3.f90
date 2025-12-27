      PROGRAM uvtest
!
!     Main test program for linear algebra program testing.
!
!     Robert Grumbine 27 Sep 1995

      IMPLICIT none

      INCLUDE "grid.inc"
!
      REAL*8 deltat, deltax, e, r
      COMMON /params/ deltat, deltax, e, r
      REAL timediff, tick, s1, s2
!
      INTEGER i, clock
!
      SAVE
!
      data  tick  /1.0/
!
      s1 = second()     
      e = 2.5D-4
      r = 1.25D-2
      deltat = .005D+0
      deltax = .033333D+0
!
!     WRITE (*, 9001) clock(0)
      PRINT *,'calling uvset' 
      DO  i = 1, 1
        CALL uvset
      ENDDO
!      
!     WRITE (6, 9001) clock(0)
      s2 = second()
      timediff = (s2 - s1) * tick
      print *,'Cpu time in seconds = ',timediff
!
      PRINT *,'done'
!      
 9001 FORMAT (I11)
      STOP
      END
!*************************************************----------++++++++++!!
      SUBROUTINE UVEXT
!     This subroutine extrapolates u and v to the next time step
!       using fully implicit differencing on u, v, and fully explicit
!       differencing on s, t.
!
      INCLUDE "grid.inc"
!
      REAL*8 deltat, deltax, e, r
      COMMON /params/ deltat, deltax, e, r
!
!     Common /matcom/ is used to reduce the memory demands of the pro-
!       gram. (uvmat requires O(8*xmax^3*ymax) words, about O(648,000) ).
!
      REAL*8 uvmat, force
      COMMON /matcom/ uvmat(2*xmax*ymax, -2*xmax:2*xmax), force(2*xmax*ymax)
!
!     Local variables
!
      REAL*8 rdt, edxdx, halfdx
      INTEGER i, j, row
!
      SAVE
!
      RETURN

      ENTRY UVSET
!
!     Entry to initialize some constants, and get the LU decomposition
!       of the UV matrix.  Save the factorization to speed the back
!       substitution.

!     First, fill uvmat with 0's.
      DO j = -2*xmax, 2*xmax
        DO i = 1, 2*xmax*ymax
          uvmat(i,j) = 0.D0
        ENDDO
      ENDDO
!
!     Compute some common constants.
!
      rdt    = DBLE(r/deltat)
      edxdx  = DBLE(e/deltax**2)
      halfdx = DBLE(1./(4.*deltax))
!
!     Now fill the coefficients matrix
!
      row = -1
      DO j = 1, ymax
        DO i = 1, xmax
        row = row + 2
!
         IF (j .eq. ymax) THEN 
!
!         No flux in the y direction.
!         Equations for u
!
          uvmat(row,      0) = rdt + 3.D0*edxdx
          uvmat(row,      1) = 1.D0
          uvmat(row,      2) = edxdx
          uvmat(row,     -2) = edxdx
          uvmat(row,-2*xmax) = edxdx
!
!         now the equations for v.
!
          uvmat(row+1,      0) = rdt + 3.D0*edxdx
          uvmat(row+1,     -1) = 1.D0
          uvmat(row+1,      2) = edxdx
          uvmat(row+1,     -2) = edxdx
          uvmat(row+1,-2*xmax) = edxdx

         ELSE
!
!         Not at a boundary
!         Equations for u.
!
          uvmat(row,      0) = rdt + 4.D0*edxdx
          uvmat(row,      1) = 1.D0
          uvmat(row,      2) = -edxdx
          uvmat(row,     -2) = -edxdx
          uvmat(row, 2*xmax) = -edxdx
          uvmat(row,-2*xmax) = -edxdx
!
!         equations for v.
!
          uvmat(row+1,      0) = rdt + 4.D0*edxdx
          uvmat(row+1,     -1) = 1.D0
          uvmat(row+1,      2) = -edxdx
          uvmat(row+1,     -2) = -edxdx
          uvmat(row+1, 2*xmax) = -edxdx
          uvmat(row+1,-2*xmax) = -edxdx

        ENDIF  

        ENDDO
      ENDDO
!
!     Do the factorization.
!
      PRINT *,'calling matsol'
      CALL MATSOL
      PRINT *,'returned from matsol'

      RETURN
      END
!*************************************************----------++++++++++!!
      SUBROUTINE MATSOL
!
!     This part does the LU factorization.
!
      INCLUDE "grid.inc"

      REAL*8 a, b
      COMMON /matcom/ a(2*xmax*ymax, -2*xmax: 2*xmax), b(2*xmax*ymax)
!
!     Local variables.
!
      INTEGER ii, i, j
      INTEGER k, n
      REAL*8 piv(2*xmax*ymax, -2*xmax: -1)

      SAVE 

      k = 2*xmax
      n = 2*xmax*ymax
!
!     Initialize the matrix to hold the pivots in temporarily.
!
      DO ii = 1, n
        DO j = -k, -1
          piv(ii, j) = 0.D0
!          PRINT *,'100 loop i, j, piv', ii, j, piv(ii,j)
        ENDDO
      ENDDO
!
!     Decompose the matrix
!
      DO ii = 1, n-1
        IF (ABS(a(ii, 0)) .LT. 1.D-10) THEN
          WRITE (*, 9001) ii
          WRITE (10, 9001) ii 
        ENDIF 
        DO i = 1, MIN(k, n-ii)
          piv(ii+i, -i) = a(ii+i, -i)/ a(ii, 0)
          DO j = 1, k
            a(ii+i, j-i) = a(ii+i, j-i) -a(ii, j)*piv(ii+i, -i)
          ENDDO
        ENDDO
      ENDDO
!
!     Now put the pivots on the lower diagonal of the matrix.
!
      DO ii = 1, n
        DO j = -k, -1
          a(ii, j) = piv(ii, j)
        ENDDO
      ENDDO

 9001 FORMAT (' WARNING!! a small pivot (.le. 1.d-10) was found on step ', I5,' of the decomposition. Accuracy will be impaired.')
 
      RETURN
      END
