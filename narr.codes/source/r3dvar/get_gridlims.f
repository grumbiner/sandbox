       subroutine get_gridlims(myxs,myxe,myys,myye,myxs_glb,myxe_glb,myys_glb,myye_glb,igrid)

!   obtain local range of x and y indices for interior of local domain

!  <--  myxs,myxe :  starting and ending x indices for interior of local domain
!  <--  myys,myye :  starting and ending y indices for interior of local domain
!  <--  myxs_glb,myxe_glb :  starting and ending global x indices for interior of local domain
!  <--  myys_glb,myye_glb :  starting and ending global y indices for interior of local domain
!   --> igrid     :  =1, then fine analysis grid, =2, then coarse analysis grid

         include 'mpif.h'
      include "my_comm.h"
         include 'types.h'
         include 'r3dv_data.comm'


         call mpi_comm_rank(my_comm,mype,ierr)
         myxs=kx3_loc(mype,igrid)
         myxe=kx8_loc(mype,igrid)
         myys=ky3_loc(mype,igrid)
         myye=ky8_loc(mype,igrid)
         myxs_glb=kx3_glb(mype,igrid)
         myxe_glb=kx8_glb(mype,igrid)
         myys_glb=ky3_glb(mype,igrid)
         myye_glb=ky8_glb(mype,igrid)

       return
       end subroutine get_gridlims
