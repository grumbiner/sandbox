      PROGRAM diff
      IMPLICIT none
      INTEGER nx
      PARAMETER (nx = 20*100)
      REAL dx,dt
      PARAMETER (dx = 0.01)
      PARAMETER (dt = 30.0)
      REAL kappa, rho, cp
      PARAMETER (kappa = 2.2)
      PARAMETER (rho   = 900)
      PARAMETER (cp    = 2100)
      REAL k, q
      REAL omega
      PARAMETER (omega = 2.*3.141592654/(86400*1.) )
      REAL temp(nx, 0:1)
      INTEGER i, j, new, tmp, old


      k = (kappa / rho / cp) * (dt/dx/dx)
      temp = 0
      new = 1
      old = 0

      do j = 1, INT(0.5+86400*365.25/dt / 10.)
        q = 50*cos(omega*(j-1)*dt)
        temp(nx,new) = temp(nx-1,old) + q * dt/rho/cp/dx
        do i = nx-1, 2, -1
          temp(i,new) = temp(i,old) + k*(temp(i+1, old)-2.*temp(i,old)+
     1                                                  temp(i-1,old) )
        enddo
        temp(1,new) = temp(2,new)
        tmp = old
        old = new
        new = tmp

        IF (AMOD(j*dt, 86400./16.) .EQ. 0) THEN
          PRINT *,'days = ',j*30/86400
          do i = nx, 1, -1
            print *,i,temp(i,old)
          enddo
        ENDIF
      enddo


      PRINT *,'Final = ',j
      do i = nx, 1, -1
        print *,i,temp(i,old)
      enddo

      STOP
      END
