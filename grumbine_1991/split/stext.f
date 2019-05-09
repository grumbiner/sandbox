C***********************************************************----------!!
      SUBROUTINE stext(uc, vc, ut, vt, we, ss, sd, qs, qd, h,
     1                 delx, dely, delt, dref, sref,
     2                 ash, asv, tstep, nx, ny)
C     Subroutine to extrapolate the salinity field to the next
C       time step.
      IMPLICIT none
      INTEGER nx, ny

      INTEGER tstep
      REAL we(nx, ny), uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL qs(nx, ny), qd(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny)
      REAL delx, dely, delt, dref, sref, ash, asv

      INTEGER i, j
      REAL fss(nx, ny), fsd(nx, ny), lapl(nx, ny)
      REAL dx2, dy2, dxtdx, dytdy, difu, pvdif, href

      dx2   = 2.*delx
      dy2   = 2.*dely
      dxtdx = delx*delx
      dytdy = dely*dely
      href  = h(1,1)
      pvdif = 8.*asv/href
      difu  = ash/dxtdx

C     Evaluate the diffusion:
      DO 100 j = 3, ny-2
        DO 101 i = 3, nx-2
          lapl(i,j) =
     1    (-ss(i+2,j)+16.*ss(i+1,j)-30.*ss(i,j)
     2               +16.*ss(i-1,j)-ss(i-2,j)   )/48.
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/48.
  101   CONTINUE
        i = 2
        lapl(i,j) = (11.*ss(i-1,j)-20.*ss(i,j)+6.*ss(i+1,j)
     1               +4.*ss(i+2,j)-ss(i+3,j) )/48.
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/48.
        i = nx-1
        lapl(i,j) = ss(i+1,j)-2.*ss(i,j)+ss(i-1,j)
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/48.

  100 CONTINUE
      DO 102 i = 2, nx-1
        j = 2
        lapl(i,j) = ss(i+1,j)-2.*ss(i,j)+ss(i-1,j)
     1   +(11.*ss(i,j-1)-20.*ss(i,j)+6.*ss(i,j+1)
     2               +4.*ss(i,j+2)-ss(i,j+3) )/48.
        j = ny-1
        lapl(i,j) = ss(i+1,j)-4.*ss(i,j)+ss(i-1,j)+ss(i,j+1)+ss(i,j-1)
  102 CONTINUE

C     Compute forcing for the interior points:
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          fss(i,j) =
     1     + difu*lapl(i,j)
     2     - (  ut(i,j)*(ss(i+1,j)-ss(i-1,j))
     3        + vt(i,j)*(ss(i,j+1)-ss(i,j-1))
     4        + uc(i+1,j)*sd(i+1,j)-uc(i-1,j)*sd(i-1,j)
     5        + vc(i,j+1)*sd(i,j+1)-vc(i,j-1)*sd(i,j-1) )/dx2
     6     + (qs(i,j) - we(i,j)*sd(i,j))/href
C           Adopt Lax-Wendroff Differencing 8-8-89.
     1     + (ut(i,j)*ut(i,j)*delt*0.5)*
     2             (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/dxtdx
     3     + (vt(i,j)*vt(i,j)*delt*0.5)*
     4             (ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/dxtdx


          fsd(i,j) =
     1     - ( ut(i,j)*(sd(i+1,j)-sd(i-1,j))
     2        +vt(i,j)*(sd(i,j+1)-sd(i,j-1))
     3        +uc(i,j)*(ss(i+1,j)-ss(i-1,j))
     4        +vc(i,j)*(ss(i,j+1)-ss(i,j-1))   ) /dx2
     5     + (qd(i,j) - sd(i,j)*(pvdif+we(i,j)) )/href
C           Adopt Lax-Wendroff Differencing 8-8-89.
     1     + (ash+ut(i,j)*ut(i,j)*delt*0.5)*
     2             (sd(i+1,j)-2.*sd(i,j)+sd(i-1,j))/dxtdx
     3     + (ash+vt(i,j)*vt(i,j)*delt*0.5)*
     4             (sd(i,j+1)-2.*sd(i,j)+sd(i,j-1))/dxtdx
C      Upwind vertical differencing for test 8-10-90.BG
CUP     1     - sd(i,j)*ABS(we(i,j)/href+(uc(i+1,j)-uc(i-1,j)+
CUP     2                                 vc(i,j+1)-vc(i,j-1))/dx2 )

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
C     No normal gradient (no flux) condition at all boundaries
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
