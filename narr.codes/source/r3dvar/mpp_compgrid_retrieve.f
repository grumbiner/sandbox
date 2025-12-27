       subroutine mpp_compgrid_retrieve(rx,ry,nx,ny,igrid)

!  retrieve rx and ry from common

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
         include "my_comm.h"
         include "r3dv_data.comm"

         real(4) rx(nx),ry(ny)


!  retrieve rx and ry from common

         rx=gridcom(igrid)%rxcom
         ry=gridcom(igrid)%rycom

       return
       end subroutine mpp_compgrid_retrieve
