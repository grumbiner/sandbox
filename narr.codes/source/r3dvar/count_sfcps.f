       subroutine count_sfcps(npdata)

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
         include "my_comm.h"
         include "r3dv_data.comm"

         npdata=npdatacom

       return
       end
