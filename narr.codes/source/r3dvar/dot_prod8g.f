subroutine dot_prod8g(prod_8,f,g,nx,ny,np,myxs,myxe,myys,myye)

  include 'mpif.h'
      include "my_comm.h"

  real(8) prod_8
  real(4) f(nx,ny,np),g(nx,ny,np)

  real(8),allocatable::sumv_8(:,:)
  real(8) sum_8

  allocate(sumv_8(myxs:myxe,myys:myye))
  sumv_8=0._8
  do k=1,np
   do j=myys,myye
    do i=myxs,myxe
     sumv_8(i,j)=sumv_8(i,j)+f(i,j,k)*g(i,j,k)
    end do
   end do
  end do
  sum_8=0_8
  do j=myys,myye
   do i=myxs,myxe
    sum_8=sum_8+sumv_8(i,j)
   end do
  end do
  deallocate(sumv_8)
  call mpi_allreduce(sum_8,prod_8,1,mpi_real8,mpi_sum,my_comm,ierr)

return
end subroutine dot_prod8g
