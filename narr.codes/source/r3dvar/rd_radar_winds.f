subroutine rd_radar_winds(werr,delta,epsilnw,wobs,bighw,ibighw,nwrdata,lbig2ges)

!   transfer radar wind data from common

  include 'types.h'
  include "PARMETA.comm"
  include "mpp.h"
  include "mpif.h"
      include "my_comm.h"
  include "r3dv_data.comm"

  real(4) werr(*)
  real(4) delta(*),epsilnw(*)
  real(4) wobs(*)
  real(4) bighw(lbig2ges,*)
  integer(4) ibighw(lbig2ges+2,*)

!------
  ii=0
  if(nwrdata.gt.0) then
   mwdata=nwdatacom
   do i=1,mwdata
    if(all_loc_data(iadwdata(i))%type.gt.2269.5) then
     ii=ii+1
     if(ii.le.nwrdata) then
      werr(ii)=all_loc_data(iadwdata(i))%error
      wobs(ii)=all_loc_data(iadwdata(i))%wobs
      delta(ii)=all_loc_data(iadwdata(i))%delta
      epsilnw(ii)=all_loc_data(iadwdata(i))%epsilnw
      bighw(1:lbig2ges,ii)=all_loc_data(iadwdata(i))%bigh(1:lbig2ges)
      ibighw(1:lbig2ges,ii)=all_loc_data(iadwdata(i))%ibigh(1:lbig2ges)
      ibighw(lbig2ges+1,ii)=all_loc_data(iadwdata(i))%qges         !   kbeamtop
      ibighw(lbig2ges+2,ii)=all_loc_data(iadwdata(i))%qobs         !  kbeambot
     end if
    end if
   end do
  end if
  if(ii.ne.nwrdata) then
   print *,' problem with rd_radar_winds, program stops'
   call mpi_abort(my_comm,130,ierror)
  end if

return
end subroutine rd_radar_winds
