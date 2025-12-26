subroutine getdtends(div0_eta,dtend_eta)

!   on input, div0_eta,dtend_eta contain divergence at times 0,dt

!   on output, dtend_eta = (div(2*dt) - div(0))/(2*dt)

      INCLUDE "PARMETA.comm"                                            
      INCLUDE "mpp.h"
      include "mpif.h"
      include "my_comm.h"
!#include "sp.h"
                             logical  &                            
       RUN,FIRST,RESTRT,SIGMA                                           
!---------------------------------------------------------------------- 
      INCLUDE "CTLBLK.comm"
!-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
!-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
!-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "CLDWTR.comm"
!-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
!-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
!-----------------------------------------------------------------------

      real(4) div0_eta(idim1:idim2,jdim1:jdim2,lm)
      real(4) dtend_eta(idim1:idim2,jdim1:jdim2,lm)

      real(4) d_div_dtrms(lm)
      real(4) d_div_dtrmsall(lm)
      real(4) sum_plevs(lm),sum_plevsall(lm)

           div0rms=0. ; div0max=0.
           div1rms=0. ; div1max=0.
           d_div_dtrms=0. ; d_div_dtmax=0.
           sum_points=0.
           sum_plevs=0.
      do l=1,lm
       do j=myjs,myje
        do i=myis,myie
         htm_hbm2=htm(i,j,l)*hbm2(i,j)
         d_div_dt=htm_hbm2*(dtend_eta(i,j,l)-div0_eta(i,j,l))/dt
            div0max=max(abs(htm_hbm2*div0_eta(i,j,l)),div0max)
            div1max=max(abs(htm_hbm2*dtend_eta(i,j,l)),div1max)
            div0rms=div0rms+(htm_hbm2*div0_eta(i,j,l))**2
            div1rms=div1rms+(htm_hbm2*dtend_eta(i,j,l))**2
            sum_points=sum_points+htm_hbm2**2
            sum_plevs(l)=sum_plevs(l)+htm_hbm2**2
            d_div_dtmax=max(abs(d_div_dt),d_div_dtmax)
            d_div_dtrms(l)=d_div_dtrms(l)+d_div_dt**2
         dtend_eta(i,j,l)=d_div_dt
        end do
       end do
      end do
            call mpi_reduce(div0max,div0maxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
            call mpi_reduce(div1max,div1maxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
            call mpi_reduce(d_div_dtmax,d_div_dtmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
            call mpi_reduce(div0rms,div0rmsall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
            call mpi_reduce(div1rms,div1rmsall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
            call mpi_reduce(d_div_dtrms,d_div_dtrmsall,lm,mpi_real4,mpi_sum,0,my_comm,ierr)
            call mpi_reduce(sum_points,sum_pointsall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
            call mpi_reduce(sum_plevs,sum_plevsall,lm,mpi_real4,mpi_sum,0,my_comm,ierr)
            if(mype.eq.0) then
              div0rmsall=sqrt(div0rmsall/sum_pointsall)
              div1rmsall=sqrt(div1rmsall/sum_pointsall)
              d_div_dtrmsall=sqrt(d_div_dtrmsall/sum_plevsall)
              print *,' dt=',dt
              print *,' div0,1 max=',div0maxall,div1maxall
              print *,' div0,1 rms=',div0rmsall,div1rmsall
              print *,' ddivmax=',d_div_dtmaxall
              sum_d_all=0.
              sum_d2_all=0.
              do l=1,lm
               sum_d_all=sum_d_all+d_div_dtrmsall(l)
               print *,' ddivrms(',l,')=',d_div_dtrmsall(l)
              end do
              sum_d_all=sum_d_all/lm
              print *,' ddivrms_ave=',sum_d_all
            end if

return
end subroutine getdtends
