       subroutine rdrads(data,nele,is,ndata,labelrad)

!   transfer satellite radiance data from common

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
         include "my_comm.h"
         include "r3dv_data.comm"

         real(4) data(nele,ndata)
         integer(8) labelrad(ndata)

         integer(8) lablocrad
         real(4) rlablocrad(2)
         equivalence(lablocrad,rlablocrad(1))
          
         if(ndata.gt.0) then
          do i=1,ndata
           data(:nele,i)=all_loc_rad(:nele,nrad_com(is)%adlist(i))
           rlablocrad(1:2)=all_loc_rad(46:47,nrad_com(is)%adlist(i))
           labelrad(i)=lablocrad
          end do
         end if

       return
       end
