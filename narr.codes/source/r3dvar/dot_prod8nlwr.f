subroutine dot_prod8nlwr(prod_8,yo,xbarb,xbar,alpha,pbar,mdata,lmetaex,ibighwr,lbig2ges)

!     compute nonlinear or linear contributions to objective function

  include 'mpif.h'
      include "my_comm.h"

  real(8) prod_8,sum_8
  real(4) yo(max(1,mdata)),xbarb(lmetaex,max(1,mdata)),xbar(lmetaex,max(1,mdata)),pbar(lmetaex,max(1,mdata))
  integer(4) ibighwr(lbig2ges+2,max(1,mdata))
       !  ibighwr(lbig2ges+1,.) -- kbeamtop
       !  ibighwr(lbig2ges+2,.) -- kbeambot

  real(4) uprofile(lmetaex)

  sum_8=0._8
  prod_8=0._8
  if(mdata.gt.0) then
   do i=1,mdata
    kbeamtop=ibighwr(lbig2ges+1,i)
    kbeambot=ibighwr(lbig2ges+2,i)
    uprofile(kbeamtop:kbeambot)=xbarb(kbeamtop:kbeambot,i)+xbar(kbeamtop:kbeambot,i) &
                                                    +alpha*pbar(kbeamtop:kbeambot,i)
    call forward_radar(yg,yo(i),uprofile,kbeambot,kbeamtop)
        
    sum_8 = sum_8 + 1._8*(1._8*yo(i)-1._8*yg)**2
   end do
  end if
  call mpi_allreduce(sum_8,prod_8,1,mpi_real8,mpi_sum,my_comm,ierr)
return
end subroutine dot_prod8nlwr
