      subroutine dot_prod8nl(prod_8,pg,cg,yo,xbarb,xbar,alpha,pbar,mdata,xfw,doqc)
!     compute nonlinear or linear contributions to objective function
      include 'mpif.h'
      include "my_comm.h"
      real(8) prod_8,arg1,arg2,arg,sum_8,wgross,wnotgross
      real(4) yo(max(1,mdata)),xbarb(max(1,mdata)),xbar(max(1,mdata)),pbar(max(1,mdata))
      logical xfw(max(1,mdata)),doqc
      sum_8=0._8
      prod_8=0._8
      wnotgross = 1._8-pg
      wgross = 1._8*pg*cg
      if(mdata.gt.0) then
        do i=1,mdata
          if(.not.xfw(i) .and. doqc) then
            arg1 = wnotgross * exp(-0.5_8*(1._8*yo(i)-1._8*(xbarb(i)+xbar(i)+alpha*pbar(i)))**2)
            arg2 = wgross
            arg = arg1 + arg2
            if(arg.gt.0. .and. arg.lt.1.e38) then
              sum_8 = sum_8 - log(arg)
            endif
          else
            sum_8 = sum_8 + 1._8*(1._8*yo(i)-1._8*(xbarb(i)+xbar(i)+alpha*pbar(i)))**2
          endif
        enddo
      endif
      call mpi_allreduce(sum_8,prod_8,1,mpi_real8,mpi_sum,my_comm,ierr)
      return
      end
