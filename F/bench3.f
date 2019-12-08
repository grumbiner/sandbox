      PROGRAM uvtest
C
C     Main test program for linear algebra program testing.
C
      INTEGER xmax, ymax
      PARAMETER (xmax = 40)
      PARAMETER (ymax = 40)
C
      REAL*8 deltat, deltax, e, r
      COMMON /params/ deltat, deltax, e, r
C
      INTEGER clock
C
      SAVE
C
      data  tick  /0.001/
C
CD      s1 = second()     
      e = 2.5D-4
      r = 1.25D-2
      deltat = .005D+0
      deltax = .033333D+0
C
C     WRITE (*, 9001) clock(0)
      PRINT *,'calling uvset' 
      DO 1 i = 1, 1
      CALL uvset
   1  CONTINUE
C      
C     WRITE (6, 9001) clock(0)
CD      s2 = second()
CD      timediff = (s2 - s1) * tick
      print *,'Cpu time in seconds = ',timediff
c
      PRINT *,'done'
C      
 9001 FORMAT (I11)
      STOP
      END
C*************************************************----------++++++++++!!
      SUBROUTINE UVEXT
C     This subroutine extrapolates u and v to the next time step
C       using fully implicit differencing on u, v, and fully explicit
C       differencing on s, t.
C
      INTEGER xmax, ymax
      PARAMETER ( xmax = 40 )
      PARAMETER ( ymax = 40 )
C
      REAL*8 deltat, deltax, e, r
      COMMON /params/ deltat, deltax, e, r
C
C     Common /matcom/ is used to reduce the memory demands of the pro-
C       gram. (uvmat requires O(8*xmax^3*ymax) words, about O(648,000) ).
C
      REAL*8 uvmat, force
      COMMON /matcom/ uvmat(2*xmax*ymax, -2*xmax:2*xmax),
     1                force(2*xmax*ymax)
C
C     Local variables
C
      REAL*8 rdt, edxdx, halfdx
      INTEGER i, j, row
C
      SAVE
C
C$EMA  /matcom/
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
C*************************************************----------++++++++++!!
      SUBROUTINE MATSOL
C
C     This part does the LU factorization.
C
      INTEGER xmax, ymax
      PARAMETER ( xmax = 40 )
      PARAMETER ( ymax = 40 )

      REAL*8 a, b
      COMMON /matcom/ a(2*xmax*ymax, -2*xmax: 2*xmax), 
     1                b(2*xmax*ymax)
C
C     Local variables.
C
      INTEGER ii, i, j
      INTEGER k, n
      REAL*8 piv(2*xmax*ymax, -2*xmax: -1)

      SAVE 
C$EMA  /matcom/

      k = 2*xmax
      n = 2*xmax*ymax
C
C     Initialize the matrix to hold the pivots in temporarily.
C
      DO 100 ii = 1, n
        DO 110 j = -k, -1
          piv(ii, j) = 0.D0
C          PRINT *,'100 loop i, j, piv', ii, j, piv(ii,j)
  110   CONTINUE
  100 CONTINUE
C
C     Decompose the matrix
C
      DO 1000 ii = 1, n-1
        IF (DABS(a(ii, 0)) .LT. 1.D-10) THEN
          WRITE (*, 9001) ii
          WRITE (10, 9001) ii 
        ENDIF 
        DO 1010 i = 1, MIN(k, n-ii)
          piv(ii+i, -i) = a(ii+i, -i)/ a(ii, 0)
          DO 1020 j = 1, k
            a(ii+i, j-i) = a(ii+i, j-i) -a(ii, j)*piv(ii+i, -i)
 1020     CONTINUE
 1010   CONTINUE
 1000 CONTINUE
C
C     Now put the pivots on the lower diagonal of the matrix.
C
      DO 2000 ii = 1, n
        DO 2010 j = -k, -1
          a(ii, j) = piv(ii, j)
 2010   CONTINUE
 2000 CONTINUE

 9001 FORMAT (' WARNING!! a small pivot (.le. 1.d-10) was found on step
     1', I5,' of the decomposition. Accuracy will be impaired.')
 
      RETURN
      END
