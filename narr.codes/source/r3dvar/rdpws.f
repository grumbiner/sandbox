       subroutine rdpws(pwerr,pwlon,pwlat,pwlong,pwlatg,pwobs,pwges, &
               pwstaid,pwtime,pwq,pwtype,pwpresb,pwprest,ipwlabel,npwdata)

!   transfer pw data from common

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
      include "my_comm.h"
         include "r3dv_data.comm"

         real(4) pwerr(max(1,npwdata)),pwlon(max(1,npwdata))
         real(4) pwlat(max(1,npwdata))
         real(4) pwlong(max(1,npwdata)),pwlatg(max(1,npwdata))
         real(4) pwobs(max(1,npwdata)),pwges(max(1,npwdata))
         character(8) pwstaid(max(1,npwdata))
         real(4) pwtime(max(1,npwdata)),pwtype(max(1,npwdata))
         real(4) pwq(max(1,npwdata))
         real(4) pwpresb(max(1,npwdata)),pwprest(max(1,npwdata))
         integer(8) ipwlabel(max(1,npwdata))

         call mpi_comm_rank(my_comm,mype,ierr)
         if(mype.eq.0) write(0,*)' rdpws--mype,npwdata: ',mype,npwdata

         if(npwdata.gt.0) then
          do i=1,npwdata
           pwtype(i)=all_loc_data(iadpwdata(i))%type
           pwstaid(i)=all_loc_data(iadpwdata(i))%staid
           pwerr(i)=all_loc_data(iadpwdata(i))%error
           pwlon(i)=all_loc_data(iadpwdata(i))%lon
           pwlat(i)=all_loc_data(iadpwdata(i))%lat
           pwtime(i)=all_loc_data(iadpwdata(i))%time
           pwobs(i)=all_loc_data(iadpwdata(i))%pwobs
           pwpresb(i)=all_loc_data(iadpwdata(i))%pressure
           pwprest(i)=all_loc_data(iadpwdata(i))%ptop
           pwges(i)=all_loc_data(iadpwdata(i))%pwges
           pwlong(i)=all_loc_data(iadpwdata(i))%long
           pwlatg(i)=all_loc_data(iadpwdata(i))%latg
           pwq(i)=all_loc_data(iadpwdata(i))%pwq
           ipwlabel(i)=all_loc_data(iadpwdata(i))%label
          end do
         end if

       return
       end
