subroutine rdwinds(werr,wlon,wlat,wlong,wlatg,wrange,wpres,etheta,delta,epsilnw,wobs,wges, &
                   wletaobs,bighw,ibighw,kbeambot,kbeamtop, &
                   wstaid,wtime,welev,wqm,wtype,iwlabel,nwdata,lbig3ges)

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
  real(4) welev(*),wtype(*)
  real(4) wqm(*)
  integer(8) iwlabel(*)
  integer(4) kbeambot(*),kbeamtop(*)

  call mpi_comm_rank(my_comm,mype,ierr)
!------
  if(nwdata.gt.0) then
   do i=1,nwdata
    wtype(i)=all_loc_data(iadwdata(i))%type
    wstaid(i)=all_loc_data(iadwdata(i))%staid
    werr(i)=all_loc_data(iadwdata(i))%error
    wlon(i)=all_loc_data(iadwdata(i))%lon
    wlat(i)=all_loc_data(iadwdata(i))%lat
    wrange(i)=all_loc_data(iadwdata(i))%tobs
    wpres(i)=all_loc_data(iadwdata(i))%pressure
    wobs(i)=all_loc_data(iadwdata(i))%wobs
    etheta(i)=all_loc_data(iadwdata(i))%theta
    delta(i)=all_loc_data(iadwdata(i))%delta
    epsilnw(i)=all_loc_data(iadwdata(i))%epsilnw
    wtime(i)=all_loc_data(iadwdata(i))%time
    welev(i)=all_loc_data(iadwdata(i))%elevobs
    wges(i)=all_loc_data(iadwdata(i))%wges
    wletaobs(i)=all_loc_data(iadwdata(i))%rletaobs
    bighw(1:lbig3ges,i)=all_loc_data(iadwdata(i))%bigh(1:lbig3ges)
    ibighw(1:lbig3ges,i)=all_loc_data(iadwdata(i))%ibigh(1:lbig3ges)
    wlong(i)=all_loc_data(iadwdata(i))%long
    wlatg(i)=all_loc_data(iadwdata(i))%latg
    wqm(i)=all_loc_data(iadwdata(i))%wqm
    iwlabel(i)=all_loc_data(iadwdata(i))%label
    kbeambot(i)=all_loc_data(iadwdata(i))%qobs         !  use these spaces to store
    kbeamtop(i)=all_loc_data(iadwdata(i))%qges         !   kbeambot,top
   end do
  end if

return
end subroutine rdwinds
