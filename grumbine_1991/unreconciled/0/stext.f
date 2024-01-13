      SUBROUTINE stext(uc, vc, ut, vt, we, ss, sd, qs, qd, h,
     1                 nx, ny, delx, dely, delt, dref, sref,
     2                 ash, asv, tstep)
C     Subroutine to extrapolate the salinity field to the next
C       time step.
C     Del(us) computed as d(us)/dx + d(vs)/dy 3-30-88.

      INTEGER nx, ny, tstep
      REAL we(nx, ny), uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL qs(nx, ny), qd(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny)
      REAL delx, dely, delt, dref, sref, ash, asv

      INTEGER i, j, nnx, nny
      PARAMETER (nnx = 36)
      PARAMETER (nny = 36)
      REAL fss(nnx, nny), fsd(nnx, nny)
      REAL dx2, dy2, dxtdx, dytdy, href, pvdif

      REAL psi
      INTEGER k, l
      psi(k,l) = 0.0
CD    psi(k,l) = SIGN(1., we(k,l)
CD   1  +  h(k,l)*((uc(i+1,j)-uc(i-1,j))/dx2+(vc(i,j+1)-vc(i,j-1))/dy2)
CD   2  +  uc(i,j)*(h(i+1,j)-h(i-1,j))/dx2
CD   3  +  vc(i,j)*(h(i,j+1)-h(i,j-1))/dy2
CD   4  -  ut(i,j)*(h(i+1,j)-h(i-1,j))/dx2
CD   5  -  vt(i,j)*(h(i,j+1)-h(i,j-1))/dy2          )

      dx2 = delx+delx
      dy2 = dely+dely
      dxtdx = delx*delx
      dytdy = dely*dely
      href  = h(nx/2, ny/2)
      pvdif = 8.*asv/href

C     Compute forcing for the interior points:
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          fss(i,j) =
     1     - (ut(i,j)*(ss(i+1,j)-ss(i-1,j))
     2       +vt(i,j)*(ss(i,j+1)-ss(i,j-1))
     3       +uc(i,j)*(sd(i+1,j)-sd(i-1,j))
     4       +vc(i,j)*(sd(i,j+1)-sd(i,j-1))
     5       +sd(i,j)*(4./3.)*(  (uc(i+1,j)-uc(i-1,j))
     6                         + (vc(i,j+1)-vc(i,j-1)) )  )/dx2
     7     + (qs(i,j) - 2.*we(i,j)*sd(i,j)) / href
CT   8     - 2.*we(i,j)*sd(i,j)/href
C          Add diffusive terms 3-30-88
CT   9     + ash*( (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/dxtdx
CT   1            +(ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/dytdy )
C          Adopt Lax-Wendroff differencing. 8-8-89.
     2     + ( (ash + ut(i,j)*ut(i,j)*delt*0.5)
     3           * (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))
     4       + (ash + vt(i,j)*vt(i,j)*delt*0.5)
     5           * (ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))  ) / dxtdx


          fsd(i,j) =
     1     - (ut(i,j)*(sd(i+1,j)-sd(i-1,j))
     2       +vt(i,j)*(sd(i,j+1)-sd(i,j-1))
     3       +uc(i,j)*(ss(i+1,j)-ss(i-1,j))
     4       +vc(i,j)*(ss(i,j+1)-ss(i,j-1))     ) / dx2
     5     + (qd(i,j) - sd(i,j)*(pvdif-we(i,j)) ) / href
CT   6     - 8.*asv*sd(i,j)/href/href
CT   9     - we(i,j)*sd(i,j)/href
CT   7     + ash*( (sd(i+1,j)-2.*sd(i,j)+sd(i-1,j))/dxtdx
CT   8            +(sd(i,j+1)-2.*sd(i,j)+sd(i,j-1))/dytdy )
C          Adopt Lax-Wendroff differencing. 8-8-89.
     2     + ((ash + ut(i,j)*ut(i,j)*delt*0.5 )
     3           * (sd(i+1,j)-2.*sd(i,j)+sd(i-1,j))
     4       +(ash + vt(i,j)*vt(i,j)*delt*0.5 )
     5           * (sd(i,j+1)-2.*sd(i,j)+sd(i,j-1)) ) / dxtdx


 1010   CONTINUE
 1000 CONTINUE

C     Extrapolate the interior values:
      DO 3000 j = 2, ny-1
        DO 3010 i = 2, nx-1
          ss(i,j) = ss(i,j) + delt*fss(i,j)
          sd(i,j) = sd(i,j) + delt*fsd(i,j)
 3010   CONTINUE
 3000 CONTINUE

C     Now must apply the boundary conditions.
C     Condition for the y=0 bndy, no flux: s(x,1) = s(x,2)
C                     y = ymax  ,          s(x,ny) = s(x,ny-1)
C         x=0 bc changed to no flux 5-26-88
C         x = xmax bc changed to Robin 5-26-88.
C     BC:
      DO 4000 i = 2, nx-1
        ss(i,1)  = ss(i,2)
        sd(i,1)  = sd(i,2)
        ss(i,ny) = ss(i,ny-1)
        sd(i,ny) = sd(i,ny-1)
 4000 CONTINUE
      DO 4010 j = 2, ny-1
        ss(nx,j) = ss(nx-1,j)
        sd(nx,j) = sd(nx-1,j)
        ss(1,j)  = ss(2,j)
        sd(1,j)  = sd(2,j)
 4010 CONTINUE
      ss(1,1)   = ss(2,2)
      ss(1,ny)  = ss(2,ny-1)
      ss(nx,1)  = ss(nx-1,2)
      ss(nx,ny) = ss(nx-1, ny-1)
      sd(1,1)   = sd(2,2)
      sd(1,ny)  = sd(2,ny-1)
      sd(nx,1)  = sd(nx-1,2)
      sd(nx,ny) = sd(nx-1, ny-1)

      RETURN
      END
