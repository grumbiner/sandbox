subroutine rd_conventional_winds(werr,wlon,wlat,wlong,wlatg,wpres,etheta,delta,epsilnw,wobs,wges, &
                   wletaobs,bighw,ibighw, &
                   wstaid,wtime,welev,wqm,wtype,iwlabel,nwdata,lbig3ges)

!   transfer wind data from common

  include 'types.h'
  include "PARMETA.comm"
  include "mpp.h"
  include "mpif.h"
      include "my_comm.h"
  include "r3dv_data.comm"

  real(4) werr(*),wlon(*),wlat(*)
  real(4) wlong(*),wlatg(*)
  real(4) wpres(*)
  real(4) etheta(*)
  real(4) delta(*),epsilnw(*)
  real(4) wobs(*),wges(*)
  real(4) wletaobs(*),bighw(lbig3ges,*)
  integer(4) ibighw(lbig3ges,*)
  character*8 wstaid(*)
  real(4) wtime(*)
  real(4) welev(*),wtype(*)
  real(4) wqm(*)
  integer(8) iwlabel(*)

!------
  ii=0
  if(nwdata.gt.0) then
   mwdata=nwdatacom
   do i=1,mwdata
    if(all_loc_data(iadwdata(i))%type.lt.2269.5) then
     ii=ii+1
     if(ii.le.nwdata) then
      wtype(ii)=all_loc_data(iadwdata(i))%type
      wstaid(ii)=all_loc_data(iadwdata(i))%staid
      werr(ii)=all_loc_data(iadwdata(i))%error
      wlon(ii)=all_loc_data(iadwdata(i))%lon
      wlat(ii)=all_loc_data(iadwdata(i))%lat
      wpres(ii)=all_loc_data(iadwdata(i))%pressure
      wobs(ii)=all_loc_data(iadwdata(i))%wobs
      etheta(ii)=all_loc_data(iadwdata(i))%theta
      delta(ii)=all_loc_data(iadwdata(i))%delta
      epsilnw(ii)=all_loc_data(iadwdata(i))%epsilnw
      wtime(ii)=all_loc_data(iadwdata(i))%time
      welev(ii)=all_loc_data(iadwdata(i))%elevobs
      wges(ii)=all_loc_data(iadwdata(i))%wges
      wletaobs(ii)=all_loc_data(iadwdata(i))%rletaobs
      bighw(1:lbig3ges,ii)=all_loc_data(iadwdata(i))%bigh(1:lbig3ges)
      ibighw(1:lbig3ges,ii)=all_loc_data(iadwdata(i))%ibigh(1:lbig3ges)
      wlong(ii)=all_loc_data(iadwdata(i))%long
      wlatg(ii)=all_loc_data(iadwdata(i))%latg
      wqm(ii)=all_loc_data(iadwdata(i))%wqm
      iwlabel(ii)=all_loc_data(iadwdata(i))%label
     end if
    end if
   end do
  end if
  if(ii.ne.nwdata) then
   print *,' problem with rd_conventional_winds, program stops'
   call mpi_abort(my_comm,120,ierror)
  end if

return
end subroutine rd_conventional_winds
