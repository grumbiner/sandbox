subroutine getptends(p0_eta,p1_eta)

!   on input, p0_eta,p1_eta contain pd at times 0,dt

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

      real(4) p0_eta(idim1:idim2,jdim1:jdim2)
      real(4) p1_eta(idim1:idim2,jdim1:jdim2)


           p0rms=0. ; p0max=0.
           p1rms=0. ; p1max=0.
           d_p_dtrms=0. ; d_p_dtmax=0.
           sum_points=0.
       do j=myjs,myje
        do i=myis,myie
         htm_hbm2=hbm2(i,j)
         d_p_dt=htm_hbm2*(p1_eta(i,j)-p0_eta(i,j))/dt
            p0max=max(abs(htm_hbm2*p0_eta(i,j)),p0max)
            p1max=max(abs(htm_hbm2*p1_eta(i,j)),p1max)
            p0rms=p0rms+(htm_hbm2*p0_eta(i,j))**2
            p1rms=p1rms+(htm_hbm2*p1_eta(i,j))**2
            sum_points=sum_points+htm_hbm2**2
            d_p_dtmax=max(abs(d_p_dt),d_p_dtmax)
            d_p_dtrms=d_p_dtrms+d_p_dt**2
        end do
       end do
            call mpi_reduce(p0max,p0maxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
            call mpi_reduce(p1max,p1maxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
            call mpi_reduce(d_p_dtmax,d_p_dtmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
            call mpi_reduce(p0rms,p0rmsall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
            call mpi_reduce(p1rms,p1rmsall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
            call mpi_reduce(d_p_dtrms,d_p_dtrmsall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
            call mpi_reduce(sum_points,sum_pointsall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
            if(mype.eq.0) then
              p0rmsall=sqrt(p0rmsall/sum_pointsall)
              p1rmsall=sqrt(p1rmsall/sum_pointsall)
              d_p_dtrmsall=sqrt(d_p_dtrmsall/sum_pointsall)
              print *,' dt=',dt
              print *,' p0,1 max=',p0maxall,p1maxall
              print *,' p0,1 rms=',p0rmsall,p1rmsall
              print *,' dpmax=',d_p_dtmaxall
              print *,' dprms=',d_p_dtrmsall
            end if

return
end subroutine getptends
