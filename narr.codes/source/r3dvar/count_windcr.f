       subroutine count_windcr(nwdata,nwrdata)

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
      include "my_comm.h"
         include "r3dv_data.comm"

         mwdata=nwdatacom
         nwdata=0
         nwrdata=0
         if(mwdata.gt.0) then
          do i=1,mwdata
           if(all_loc_data(iadwdata(i))%type.gt.2269.5) then
            nwrdata=nwrdata+1
           else
            nwdata=nwdata+1
           end if
          end do
         end if
         if(nwdata+nwrdata.ne.mwdata) then
           print *,' problem in count_winds--nwdata+nwrdat  /= nwdatacom'
           call mpi_abort(my_comm,100,ierror)
         end if

       return
       end subroutine count_windcr
