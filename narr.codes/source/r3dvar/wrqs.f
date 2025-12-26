subroutine wrqs(qerr,qlon,qlat,qlong,qlatg,qpres,qobs,qges,qsatges, &
                qletaobs,bighq,ibighq, &
                qstaid,qtime,qelev,qtobs,qqm,qtype,qmaxerr,iqlabel,mqdata,lbig3ges)

!   transfer moisture data to common

  include 'types.h'
  include "PARMETA.comm"
  include "mpp.h"
  include "mpif.h"
         include "my_comm.h"
  include "r3dv_data.comm"

  real(4) qerr(*),qlon(*),qlat(*)
  real(4) qlong(*),qlatg(*)
  real(4) qpres(*),qobs(*)
  real(4) qges(*),qsatges(*)
  real(4) qletaobs(*),bighq(lbig3ges,*)
  integer(4) ibighq(lbig3ges,*)
  integer(8) iqlabel(*)
  character(8) qstaid(*)
  real(4) qtime(*),qtype(*),qmaxerr(*)
  real(4) qelev(*),qtobs(*),qqm(*)


  nqdatacom=mqdata
  if(mqdata.gt.0) then
   do i=1,mqdata
    all_loc_data(iadqdata(i))%type=qtype(i)
    all_loc_data(iadqdata(i))%staid=qstaid(i)
    all_loc_data(iadqdata(i))%error=qerr(i)
    all_loc_data(iadqdata(i))%lon=qlon(i)
    all_loc_data(iadqdata(i))%lat=qlat(i)
    all_loc_data(iadqdata(i))%pressure=qpres(i)
    all_loc_data(iadqdata(i))%qobs=qobs(i)
    all_loc_data(iadqdata(i))%qges=qges(i)
    all_loc_data(iadqdata(i))%qsatges=qsatges(i)
    all_loc_data(iadqdata(i))%rletaobs=qletaobs(i)
    all_loc_data(iadqdata(i))%bigh(1:lbig3ges)=bighq(1:lbig3ges,i)
    all_loc_data(iadqdata(i))%ibigh(1:lbig3ges)=ibighq(1:lbig3ges,i)
    all_loc_data(iadqdata(i))%time=qtime(i)
    all_loc_data(iadqdata(i))%tobs=qtobs(i)
    all_loc_data(iadqdata(i))%elevobs=qelev(i)
    all_loc_data(iadqdata(i))%maxerror=qmaxerr(i)
    all_loc_data(iadqdata(i))%long=qlong(i)
    all_loc_data(iadqdata(i))%latg=qlatg(i)
    all_loc_data(iadqdata(i))%qqm=qqm(i)
    all_loc_data(iadqdata(i))%label=iqlabel(i)
   end do
  end if
    
return
end subroutine wrqs
