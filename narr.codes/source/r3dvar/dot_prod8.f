subroutine dot_prod8(prod_8,f,g,n)

  include 'mpif.h'
      include "my_comm.h"

  real(8) prod_8
  real(4) f(n),g(n)

  real(8) fg_8,sum_8

  sum_8=0._8
  if(n.gt.0) then
   do i=1,n
    fg_8=f(i)*g(i)
    sum_8=sum_8+fg_8
   end do
  end if
  call mpi_allreduce(sum_8,prod_8,1,mpi_real8,mpi_sum,my_comm,ierr)

return
end
