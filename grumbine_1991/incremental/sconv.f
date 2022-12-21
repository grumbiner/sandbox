      SUBROUTINE sconv(uc, vc, ut, vt, we, ss, sd, qs, qd, h,
     1                 nx, ny, delx, dely, delt, dref, sref,
     2                 ash, asv, tstep, s1, s2,
     3                 s3, s4, s5, s6, s7, s8              )
C     Subroutine to extrapolate the salinity field to the next
C       time step.
C     Del(us) computed as d(us)/dx + d(vs)/dy 3-30-88.
C     Robert Grumbine.

      INTEGER nx, ny, tstep
      REAL we(nx, ny), uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL qs(nx, ny), qd(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny)
      REAL delx, dely, delt, dref, sref, ash, asv

      INTEGER i, j, nnx, nny
      PARAMETER (nnx = 36)
      PARAMETER (nny = 36)
      REAL fss(nnx, nny)
      REAL dx2, dy2

      INTEGER k, l
      DOUBLE PRECISION s1, s2
      DOUBLE PRECISION s3, s4, s5, s6, s7, s8

      dx2 = 2.*delx
      dy2 = 2.*dely
      s1 = 0.D0
      s2 = 0.D0
      s3 = 0.D0
      s4 = 0.D0
      s5 = 0.D0
      s6 = 0.D0
      s7 = 0.D0
      s8 = 0.D0

C     Compute forcing for the interior points:
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          fss(i,j) =
     1     - ut(i,j)*(ss(i+1,j)-ss(i-1,j))/dx2
     2     - vt(i,j)*(ss(i,j+1)-ss(i,j-1))/dy2
     3     - uc(i,j)*(sd(i+1,j)-sd(i-1,j))/dx2
     4     - vc(i,j)*(sd(i,j+1)-sd(i,j-1))/dy2
     5     - sd(i,j)*(  (uc(i+1,j)-uc(i-1,j))/dx2
     6                + (vc(i,j+1)-vc(i,j-1))/dy2   ) *(4./3.)
     7     + qs(i,j)/h(i,j)
     8     - 2.*we(i,j)*sd(i,j)/h(i,j)
C          Add diffusive terms 3-30-88
     9     + ash*( (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/(delx*delx)
     1            +(ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/(dely*dely) )
C          Adopt Lax-Wendroff differencing. 8-8-89.
     2     + ut(i,j)**2*delt/2.
     3          *( (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/(delx*delx) )
     4     + vt(i,j)**2*delt/2.
     5          *( (ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/(dely*dely) )

          s3 = s3 + DBLE(  - ut(i,j)*(ss(i+1,j)-ss(i-1,j))/dx2
     1                     - vt(i,j)*(ss(i,j+1)-ss(i,j-1))/dy2
C          Adopt Lax-Wendroff differencing. 8-8-89.
     2     + ut(i,j)**2*delt/2.
     3          *( (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/(delx*delx) )
     4     + vt(i,j)**2*delt/2.
     5          *( (ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/(dely*dely) )     )
          s4 = s4 + DBLE( - uc(i,j)*(sd(i+1,j)-sd(i-1,j))/dx2
     2               - vc(i,j)*(sd(i,j+1)-sd(i,j-1))/dy2    )
          s5 = s5 + DBLE( - sd(i,j)*(  (uc(i+1,j)-uc(i-1,j))/dx2
     3                + (vc(i,j+1)-vc(i,j-1))/dy2   ) *(4./3.) )
          s6 = s6 + DBLE( + qs(i,j)/h(i,j) )
          s7 = s7 + DBLE( - 2.*we(i,j)*sd(i,j)/h(i,j) )
          s8 = s8 + DBLE(ash*(
     4                  (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/(delx*delx)
     5                 +(ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/(dely*dely) ) )

 1010   CONTINUE
 1000 CONTINUE

C     Compute the total salt and the total forcing.
      s1 = 0.D0
      s2 = 0.D0
      DO 2000 j = 2, ny-1
        DO 2010 i = 2, nx-1
          s1 = s1 + DBLE(ss(i,j))
          s2 = s2 + DBLE(fss(i,j))
 2010   CONTINUE
 2000 CONTINUE
      s1 = s1 * DBLE(delx*dely)
      s2 = s2 * DBLE(delx*dely)
      s3 = s3 * DBLE(delx*dely)
      s4 = s4 * DBLE(delx*dely)
      s5 = s5 * DBLE(delx*dely)
      s6 = s6 * DBLE(delx*dely)
      s7 = s7 * DBLE(delx*dely)
      s8 = s8 * DBLE(delx*dely)

      RETURN
      END
