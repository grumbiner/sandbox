subroutine wrwinds(werr,wlon,wlat,wlong,wlatg,wrange,wpres,etheta,delta,epsilnw, &
                   wobs,wges,wletaobs,bighw,ibighw,kbeambot,kbeamtop, &
                   wstaid,wtime,welev,wqm,wtype,iwlabel,mwdata,lbig3ges)

!   transfer wind data from common

  include 'types.h'
  include "PARMETA.comm"
  include "mpp.h"
  include "mpif.h"
         include "my_comm.h"
  include "r3dv_data.comm"

  real(4) werr(*),wlon(*),wlat(*)
  real(4) wlong(*),wlatg(*),wrange(*)
  real(4) wpres(*)
  real(4) etheta(*)
  real(4) delta(*),epsilnw(*)
  real(4) wobs(*),wges(*)
  real(4) wletaobs(*),bighw(lbig3ges,*)
  integer(4) ibighw(lbig3ges,*)
  character*8 wstaid(*)
  real(4) wtime(*)
  real(4) welev(*),wtype(*),wqm(*)
  integer(8) iwlabel(*)
  integer(4) kbeambot(*),kbeamtop(*)

!------
  nwdatacom=mwdata
  if(mwdata.gt.0) then
   do i=1,mwdata
    all_loc_data(iadwdata(i))%type=wtype(i)
    all_loc_data(iadwdata(i))%staid=wstaid(i)
    all_loc_data(iadwdata(i))%error=werr(i)
    all_loc_data(iadwdata(i))%lon=wlon(i)
    all_loc_data(iadwdata(i))%lat=wlat(i)
    all_loc_data(iadwdata(i))%tobs=wrange(i)
    all_loc_data(iadwdata(i))%pressure=wpres(i)
    all_loc_data(iadwdata(i))%wobs=wobs(i)
    all_loc_data(iadwdata(i))%theta=etheta(i)
    all_loc_data(iadwdata(i))%delta=delta(i)
    all_loc_data(iadwdata(i))%epsilnw=epsilnw(i)
    all_loc_data(iadwdata(i))%time=wtime(i)
    all_loc_data(iadwdata(i))%elevobs=welev(i)
    all_loc_data(iadwdata(i))%wges=wges(i)
    all_loc_data(iadwdata(i))%rletaobs=wletaobs(i)
    all_loc_data(iadwdata(i))%bigh(1:lbig3ges)=bighw(1:lbig3ges,i)
    all_loc_data(iadwdata(i))%ibigh(1:lbig3ges)=ibighw(1:lbig3ges,i)
    all_loc_data(iadwdata(i))%long=wlong(i)
    all_loc_data(iadwdata(i))%latg=wlatg(i)
    all_loc_data(iadwdata(i))%wqm=wqm(i)
    all_loc_data(iadwdata(i))%label=iwlabel(i)
    all_loc_data(iadwdata(i))%qobs=kbeambot(i)         !  use these spaces to store
    all_loc_data(iadwdata(i))%qges=kbeamtop(i)         !   kbeambot,top
   end do
  end if

return
end subroutine wrwinds
