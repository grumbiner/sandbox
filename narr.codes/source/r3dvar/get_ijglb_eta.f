       subroutine get_ijglb_eta(i,j,iglb,jglb)

!  obtain globall indices from locale indices for eta grid

         include "PARMETA.comm"
         include "mpif.h"
         include "my_comm.h"
         include "mpp.h"

         iglb=i+idim1-1-my_is_loc+my_is_glb
         jglb=j+jdim1-1-my_js_loc+my_js_glb

       return
       end subroutine get_ijglb_eta
