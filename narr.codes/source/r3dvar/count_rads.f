       subroutine count_rads(is,ndata)

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
         include "my_comm.h"
         include "r3dv_data.comm"
          
         ndata=nrad_com(is)%count

       return
       end
