subroutine wrtemps(terr,tlon,tlat,tlong,tlatg,tpres,tobs,tges, &
                   tletaobs,bight,ibight, &
                   tstaid,ttime,telev,tqm,ttype,iqtflg,itlabel,mtdata,lbig3ges)

!   transfer temperature data from common

  include 'types.h'
  include "PARMETA.comm"
  include "mpp.h"
  include "mpif.h"
         include "my_comm.h"
  include "r3dv_data.comm"

  real(4) terr(*),tlon(*),tlat(*)
  real(4) tlong(*),tlatg(*),tpres(*)
  real(4) tobs(*),telev(*),tges(*)
  real(4) tletaobs(*),bight(lbig3ges,*)
  integer(4) ibight(lbig3ges,*)
  real(4) ttime(*),ttype(*),tqm(*)
  integer(4) iqtflg(*)
  integer(8) itlabel(*)
  character(8) tstaid(*)


!------
  ntdatacom=mtdata
  if(mtdata.gt.0) then
   do i=1,mtdata
    all_loc_data(iadtdata(i))%type=ttype(i)
    all_loc_data(iadtdata(i))%staid=tstaid(i)
    all_loc_data(iadtdata(i))%error=terr(i)
    all_loc_data(iadtdata(i))%lon=tlon(i)
    all_loc_data(iadtdata(i))%lat=tlat(i)
    all_loc_data(iadtdata(i))%pressure=tpres(i)
    all_loc_data(iadtdata(i))%tobs=tobs(i)
    all_loc_data(iadtdata(i))%time=ttime(i)
    all_loc_data(iadtdata(i))%elevobs=telev(i)
    all_loc_data(iadtdata(i))%qtflag=iqtflg(i)
    all_loc_data(iadtdata(i))%tges=tges(i)
    all_loc_data(iadtdata(i))%rletaobs=tletaobs(i)
    all_loc_data(iadtdata(i))%bigh(1:lbig3ges)=bight(1:lbig3ges,i)
    all_loc_data(iadtdata(i))%ibigh(1:lbig3ges)=ibight(1:lbig3ges,i)
    all_loc_data(iadtdata(i))%long=tlong(i)
    all_loc_data(iadtdata(i))%latg=tlatg(i)
    all_loc_data(iadtdata(i))%tqm=tqm(i)
    all_loc_data(iadtdata(i))%label=itlabel(i)
   end do
  end if

return
end subroutine wrtemps
