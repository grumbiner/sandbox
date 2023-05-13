      SUBROUTINE chext(uc, vc, ut, vt, we, cs, cd, qs, qd, h,
     1                 nx, ny, delx, dely, delt, dref, sref, lambda,
     2                 gamma, ah, av, tstep)
C     Subroutine to extrapolate the chemical tracer field to the next
C       time step.
C     Taken from stdif of 1-24-89.
C     BC 1-24-89
C       y = 0, ymax d /dy = 0
C       x = 0, d /dx = 0
C       x = xmax  d / dx = gamma*(a -a0)
C     Change bc at x = xmax to d/dx = 0  ; 1-26-89

      INTEGER nx, ny, tstep
      REAL we(nx, ny), uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL qs(nx, ny), qd(nx, ny), cs(nx, ny), cd(nx, ny)
      REAL h(nx, ny)
      REAL delx, dely, delt, dref, sref, gamma, lambda, ah, av

      INTEGER i, j
      REAL fcs(40, 40), fcd(40, 40)
      REAL dx2, dy2

      REAL psi
      INTEGER k, l
      psi(k,l) = 0.0
CD    psi(k,l) = SIGN(1., we(k,l)
CD   1  +  h(k,l)*((uc(i+1,j)-uc(i-1,j))/dx2+(vc(i,j+1)-vc(i,j-1))/dy2)
CD   2  +  uc(i,j)*(h(i+1,j)-h(i-1,j))/dx2
CD   3  +  vc(i,j)*(h(i,j+1)-h(i,j-1))/dy2
CD   4  -  ut(i,j)*(h(i+1,j)-h(i-1,j))/dx2
CD   5  -  vt(i,j)*(h(i,j+1)-h(i,j-1))/dy2          )

      dx2 = 2.*delx
      dy2 = 2.*dely

C     Compute forcing for the interior points:
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          fcs(i,j) =
C              iso haline/thermal mixed layer 8-15-88.
     1     - ut(i,j)*(cs(i+1,j)-cs(i-1,j))/dx2
     2     - vt(i,j)*(cs(i,j+1)-cs(i,j-1))/dy2
     3     - uc(i,j)*(cd(i+1,j)-cd(i-1,j))/dx2
     4     - vc(i,j)*(cd(i,j+1)-cd(i,j-1))/dy2
     5     - cd(i,j)*(  (uc(i+1,j)-uc(i-1,j))/dx2
     6                + (vc(i,j+1)-vc(i,j-1))/dy2   )
     7     + qs(i,j)
     8     - we(i,j)*cd(i,j)/h(i,j)
C          Add diffusive terms 3-30-88
     9     + ah*( (cs(i+1,j)-2.*cs(i,j)+cs(i-1,j))/(delx*delx)
     1           +(cs(i,j+1)-2.*cs(i,j)+cs(i,j-1))/(dely*dely) )
C          Add topographic terms 1-18-89
     1     - (cd(i,j)/h(i,j))
     2          *2.*( uc(i,j)*(h(i+1,j)-h(i-1,j))/dx2
     3               +vc(i,j)*(h(i,j+1)-h(i,j-1))/dy2 )
     4     + (cd(i,j)/h(i,j))
     5          *( ut(i,j)*(h(i+1,j)-h(i-1,j))/dx2
     6            +vt(i,j)*(h(i,j+1)-h(i,j-1))/dy2 )


          fcd(i,j) =
     1     - ut(i,j)*(cd(i+1,j)-cd(i-1,j))/dx2
     2     - vt(i,j)*(cd(i,j+1)-cd(i,j-1))/dy2
     3     - uc(i,j)*(cs(i+1,j)-cs(i-1,j))/dx2
     4     - vc(i,j)*(cs(i,j+1)-cs(i,j-1))/dy2
     5     - cs(i,j)*(uc(i+1,j)-uc(i-1,j))/dx2
     6     - cs(i,j)*(vc(i,j+1)-vc(i,j-1))/dy2
     7     + qd(i,j)
     8     - 8.*av*cd(i,j)/h(i,j)**2
     9     + ah*( (cd(i+1,j)-2.*cd(i,j)+cd(i-1,j))/(delx*delx)
     1            +(cd(i,j+1)-2.*cd(i,j)+cd(i,j-1))/(dely*dely) )
     2     - we(i,j)*cs(i,j)/h(i,j)
CU   3     - (psi(i,j)*cd(i,j)/h(i,j))* ( we(i,j)
CU   4  +  h(k,l)*((uc(i+1,j)-uc(i-1,j))/dx2+(vc(i,j+1)-vc(i,j-1))/dy2)
CU   5  + (uc(i,j)-ut(i,j))*(h(i+1,j)-h(i-1,j))/dx2
CU   6  + (vc(i,j)-vt(i,j))*(h(i,j+1)-h(i,j-1))/dy2     )

          fcd(i,j) = fcd(i,j)
C          Add Topography separately to avoid probs with continuation
C             line limit
     1  + ut(i,j)*(cs(i,j)-2.*cd(i,j))*(h(i+1,j)-h(i-1,j))/dx2/h(i,j)
     2  + vt(i,j)*(cs(i,j)-2.*cd(i,j))*(h(i,j+1)-h(i,j-1))/dy2/h(i,j)
     3  - uc(i,j)*(cs(i,j)-   cd(i,j))*(h(i+1,j)-h(i-1,j))/dx2/h(i,j)
     4  - vc(i,j)*(cs(i,j)-   cd(i,j))*(h(i,j+1)-h(i,j-1))/dy2/h(i,j)

 1010   CONTINUE
 1000 CONTINUE

C     Extrapolate the interior values:
      DO 3000 j = 2, ny-1
        DO 3010 i = 2, nx-1
          cs(i,j) = cs(i,j) + delt*fcs(i,j)
          cd(i,j) = cd(i,j) + delt*fcd(i,j)
 3010   CONTINUE
 3000 CONTINUE

C     Now must apply the boundary conditions.
C     Condition for the y=0 bndy, no flux: s(x,1) = s(x,2)
C                     y = ymax  ,          s(x,ny) = s(x,ny-1)
C         x = 0 bc changed to no flux 5-26-88
C         x = xmax bc changed to Robin 5-26-88.
C     BC:
      DO 4000 i = 1, nx-1
        cs(i,1)  = cs(i,2)
        cd(i,1)  = cd(i,2)
        cs(i,ny) = cs(i,ny-1)
        cd(i,ny) = cd(i,ny-1)
 4000 CONTINUE
      DO 4010 j = 1, ny
CD      cs(nx,j) = (cs(nx-1,j)+delx*gamma*sref)/(1.+gamma*delx)
CD      cd(nx,j) = (cd(nx-1,j)+delx*gamma*dref)/(1.+gamma*delx)
        cs(nx,j) = cs(nx-1,j)
        cd(nx,j) = cd(nx-1,j)
        cs(1,j)  = cs(2,j)
        cd(1,j)  = cd(2,j)
 4010 CONTINUE

      RETURN
      END
