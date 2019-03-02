C*************************************************----------++++++++++!!
      SUBROUTINE benchsub(xmax, ymax, uvmat, force)
C     This subroutine extrapolates u and v to the next time step
C       using fully implicit differencing on u, v, and fully explicit
C       differencing on s, t.
C
      IMPLICIT none

      INTEGER xmax, ymax
C
      REAL*8 deltat, deltax, e, r
      COMMON /params/ deltat, deltax, e, r
C
C     Common /matcom/ is used to reduce the memory demands of the pro-
C       gram. (uvmat requires O(8*xmax^3*ymax) words, about O(648,000) ).
C
      REAL*8 uvmat(2*xmax*ymax, -2*xmax:2*xmax),
     1       force(2*xmax*ymax)
C
C     Local variables
C
      REAL*8 rdt, edxdx, halfdx
      INTEGER i, j, row
C
      SAVE
C
      RETURN

      ENTRY UVSET
C
C     Entry to initialize some constants, and get the LU decomposition
C       of the UV matrix.  Save the factorization to speed the back
C       substitution.

C     First, fill uvmat with 0's.
      DO 5000 j = -2*xmax, 2*xmax
        DO 5010 i = 1, 2*xmax*ymax
          uvmat(i,j) = 0.D0
 5010   CONTINUE
 5000 CONTINUE
C
C     Compute some common constants.
C
      rdt    = DBLE(r/deltat)
      edxdx  = DBLE(e/deltax**2)
      halfdx = DBLE(1./(4.*deltax))
C
C     Now fill the coefficients matrix
C
      row = -1
      DO 6000 j = 1, ymax
        DO 6010 i = 1, xmax
        row = row + 2
C
         IF (j .eq. ymax) THEN 
C
C         No flux in the y direction.
C         Equations for u
C
          uvmat(row,      0) = rdt + 3.D0*edxdx
          uvmat(row,      1) = 1.D0
          uvmat(row,      2) = edxdx
          uvmat(row,     -2) = edxdx
          uvmat(row,-2*xmax) = edxdx
C
C         now the equations for v.
C
          uvmat(row+1,      0) = rdt + 3.D0*edxdx
          uvmat(row+1,     -1) = 1.D0
          uvmat(row+1,      2) = edxdx
          uvmat(row+1,     -2) = edxdx
          uvmat(row+1,-2*xmax) = edxdx

         ELSE
C
C         Not at a boundary
C         Equations for u.
C
          uvmat(row,      0) = rdt + 4.D0*edxdx
          uvmat(row,      1) = 1.D0
          uvmat(row,      2) = -edxdx
          uvmat(row,     -2) = -edxdx
          uvmat(row, 2*xmax) = -edxdx
          uvmat(row,-2*xmax) = -edxdx
C
C         equations for v.
C
          uvmat(row+1,      0) = rdt + 4.D0*edxdx
          uvmat(row+1,     -1) = 1.D0
          uvmat(row+1,      2) = -edxdx
          uvmat(row+1,     -2) = -edxdx
          uvmat(row+1, 2*xmax) = -edxdx
          uvmat(row+1,-2*xmax) = -edxdx

        ENDIF  

 6010   CONTINUE
 6000 CONTINUE
C
C     Do the factorization.
C
      PRINT *,'calling matsol'
      CALL MATSOL
      PRINT *,'returned from matsol'
C
C     DO 9999 i = 1, 2*xmax*ymax
C       WRITE (*,9998) (uvmat(i, j),j=-2*xmax, 2*xmax)
C9998   FORMAT (7D10.3)
C9999 CONTINUE

      RETURN
      END
