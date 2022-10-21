C*************************************************----------++++++++++!!
      SUBROUTINE UVEXT( u, v, s, t )
C     This subroutine extrapolates u and v to the next time step
C       using fully implicit differencing on u, v, and fully explicit
C       differencing on s, t.

      INTEGER xmax, ymax
      PARAMETER (xmax = 20)
      PARAMETER (ymax = 18)

      REAL  u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1      s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax)


      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ycoast, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ycoast, outden
      LOGICAL debug


C     Matrices to compute u and v at the new time step.
      REAL uvabd(6*xmax+1, 2*xmax*ymax),
     1                force(2*xmax*ymax)

C     Variables for the LINPACK routines.
      INTEGER IPVT(2*xmax*ymax), INFO


C     Local variables
      REAL rdt, edxdx, halfdx
      INTEGER i, j, row

      SAVE uvabd, IPVT, INFO, rdt, edxdx, halfdx, /params/

C$EMA  uvabd

C     Begin the extrapolation.
      row  = -1

      DO 1000 j = 1, ymax
        DO 1010 i = 1, xmax

          row = row + 2

          IF ( (j .EQ. 1  .AND. i .LE. xbox) .OR. (j .EQ. ycoast
     1           .AND. i .GE. xbox) )  THEN
C         Apply no slip condition at horizontal bndy.
          force(row)   = 0.0
          force(row+1) = 0.0

         ELSE IF (j .LT. ycoast .AND. (i .EQ. 1 .OR. i .EQ. xbox))
     1    THEN
C         Apply no slip at a vertical boundary.
          force(row)   = 0.0
          force(row+1) = 0.0

         ELSE IF (j .EQ. ymax) THEN
C         Apply no flux in y direction here.
          force(row)   = rdt*u(i,j)+ (t(i+1,j)-t(i-1,j))*halfdx
          force(row+1) = rdt * v(i, j)
          IF (i .EQ. xmax) THEN
C           Experimentally remove the velocity at the previous step.
C           The idea here is that I don't think I can compute the full
C           laplacian at xmax, ymax.  In doing this I do allow there to
C           be a velocity at the corner of the domain, but it has no mem
C           for previous values.
            force(row  ) = (t(i+1,j)-t(i-1,j))*halfdx
            force(row+1) = 0.0
          ENDIF
C*************************************************----------++++++++++!!

         ELSE IF (j .LT. ycoast .AND. i .GT. xbox) THEN
C         You are on the continent.
          force(row)   = 0.0
          force(row+1) = 0.0

         ELSE
C         Main body of the sea.
          force(row)   = rdt* u(i,j) +(t(i+1,j)-t(i-1,j))*halfdx
          force(row+1) = rdt* v(i,j) +(t(i,j+1)-t(i,j-1))*halfdx

        ENDIF

 1010   CONTINUE
 1000 CONTINUE

C     Now call the routine to do the back substitution.
C     SGBSL is the LINPACK single precision routine for back substitutio
C       in banded matrices.
C     IF (debug) WRITE (6,9002)
      CALL SGBSL (uvabd, 6*xmax+1, 2*xmax*ymax, 2*xmax, 2*xmax,
     1              IPVT, force, 0)

C     Fill u and v with the appropriate values.
      row = -1
      DO 2000 j = 1, ymax
        DO 2010 i = 1, xmax

          row = row + 2
          u(i,j) = force(row)
          v(i,j) = force(row+1)

 2010   CONTINUE
 2000 CONTINUE

C     Apply the periodicity condition.
      DO 3000 j = 0, ymax
        IF (j .GE. ycoast) THEN
          u(0,     j) = u(xmax, j)
          u(xmax+1,j) = u(1,    j)
          v(0,     j) = v(xmax, j)
          v(xmax+1,j) = v(1,    j)
         ELSE
          u(0,     j) = 0.0
          u(xmax+1,j) = 0.0
          v(0,     j) = 0.0
          v(xmax+1,j) = 0.0
        ENDIF
 3000 CONTINUE

 9002 FORMAT (' calling back substitution')

      RETURN

      ENTRY UVSET(u, v, s, t)
C     Entry to initialize some constants, and get the LU decomposition
C       of the UV matrix.  Save the factorization to speed the back
C       substitution.

C     First, fill uvabd with 0's.
      DO 5000 j = 1, 2*xmax*ymax
        DO 5010 i = 1, 6*xmax+1
          uvabd(i,j) = 0.0
 5010   CONTINUE
 5000 CONTINUE

C     Compute some common constants.
      rdt    = r/deltat
      edxdx  = e/deltax**2
      halfdx = 1./(4.*deltax)

C     Now fill the coefficients matrix
      row = -1
      DO 6000 j = 1, ymax
        DO 6010 i = 1, xmax
        row = row + 2
        IF ( j .EQ. 1 .AND. i .LE. xbox .OR. (j .EQ. ycoast .AND.
     1         i .GE. xbox) ) THEN
C         Apply flux at at a horizontal boundary.
C           The constant is arbitrary (since the forcing is 0), so
C           choose 1.
          uvabd(4*xmax+1, row  ) = 1.0
          uvabd(4*xmax+1, row+1) = 1.0

         ELSE IF (j .LT. ycoast .AND. (i .EQ. 1 .OR. i .EQ. xbox))
     1    THEN
C         Apply no slip at a vertical boundary.
C           Constant is arbitrary, so use 1.
          uvabd(4*xmax+1, row  ) = 1.0
          uvabd(4*xmax+1, row+1) = 1.0

         ELSE IF (j .eq. ymax) THEN
          IF (i .NE. xmax) THEN
C           No flux in the y direction.
C           Equations for u
            uvabd(4*xmax+1, row  ) = rdt + 3.*edxdx
            uvabd(4*xmax  , row+1) = 1.0
            uvabd(4*xmax-1, row+2) = -edxdx
            uvabd(4*xmax+3, row-2) = -edxdx
            uvabd(6*xmax+1, row-2*xmax) = -edxdx
C           now the equations for v.
            uvabd(4*xmax+1, row+1) = rdt + 3.*edxdx
            uvabd(4*xmax+2, row  ) = -1.0
            uvabd(4*xmax-1, row+3) = -edxdx
            uvabd(4*xmax+3, row-1) = -edxdx
            uvabd(6*xmax+1, row+1-2*xmax) = -edxdx
           ELSE
C           use second deriv in x = 0 to keep from overflowing the array
C             this only occurs at one point, so there shouldn't be much
C             from this point.
            uvabd(4*xmax+1, row  ) = rdt + 3.*edxdx
            uvabd(4*xmax  , row+1) = 1.0
            uvabd(4*xmax+3, row-2) = -edxdx
            uvabd(6*xmax+1, row-2*xmax) = -edxdx
            uvabd(4*xmax+1, row+1) = rdt + 3.*edxdx
            uvabd(4*xmax+2, row  ) = 1.0
            uvabd(4*xmax+3, row-1) = -edxdx
            uvabd(6*xmax+1, row+1-2*xmax) = -edxdx
          ENDIF
         ELSE IF (j .LT. ycoast .AND. i .GT. xbox) THEN
C         On the continent.  Coefficients are arbitrary, since there
C           is no forcing, so use 1.
          uvabd(4*xmax+1, row  ) = 1.0
          uvabd(4*xmax+1, row+1) = 1.0

         ELSE
C         Not at a boundary
C         Equations for u.
          uvabd(4*xmax+1, row  ) = rdt + 4.*edxdx
          uvabd(4*xmax  , row+1) = 1.0
          uvabd(4*xmax-1, row+2) = -edxdx
          uvabd(4*xmax+3, row-2) = -edxdx
          uvabd(2*xmax+1, row+2*xmax) = -edxdx
          uvabd(6*xmax+1, row-2*xmax) = -edxdx
C         equations for v.
          uvabd(4*xmax+1, row+1) = rdt + 4.*edxdx
          uvabd(4*xmax+2, row  ) = -1.0
          uvabd(4*xmax-1, row+3) = -edxdx
          uvabd(4*xmax+3, row-1) = -edxdx
          uvabd(2*xmax+1, row+1+2*xmax) = -edxdx
          uvabd(6*xmax+1, row+1-2*xmax) = -edxdx

        ENDIF

 6010   CONTINUE
 6000 CONTINUE

C     Do the factorization.
C     SGBFA is the LINPACK routine for factoring a banded matrix,
C       single precision.
      CALL SGBFA( uvabd, 6*xmax+1, 2*xmax*ymax, 2*xmax, 2*xmax,
     1            IPVT, INFO)
      IF (INFO .NE. 0) STOP 'unusable decompostion'

      RETURN
      END
