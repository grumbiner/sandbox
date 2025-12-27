       subroutine get_etalims(myisout,myieout,myjsout,myjeout)

! obtain local domain eta grid limits

         include "PARMETA.comm"
         include "mpif.h"
         include "my_comm.h"
         include "mpp.h"

         myisout=myis-idim1+1
         myieout=myie-idim1+1
         myjsout=myjs-jdim1+1
         myjeout=myje-jdim1+1

       return
       end subroutine get_etalims
