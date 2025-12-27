       subroutine rdtemps(terr,tlon,tlat,tlong,tlatg,tpres,tobs,tges, &
                       tletaobs,bight,ibight, &
                tstaid,ttime,telev,tqm,ttype,iqtflg,itlabel,ntdata,lbig3ges)

!   transfer temperature data from common

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
         include "my_comm.h"
         include "r3dv_data.comm"

         real(4) terr(*),tlon(*)
         real(4) tlat(*)
         real(4) tlong(*),tlatg(*)
         real(4) tpres(*)
         real(4) tobs(*),telev(*)
         real(4) tges(*)
         real(4) tletaobs(*),bight(lbig3ges,*)
         integer(4) ibight(lbig3ges,*)
         real(4) ttime(*),ttype(*)
         real(4) tqm(*)
         integer(4) iqtflg(*)
         integer(8) itlabel(*)
         character(8) tstaid(*)

                   if(lbig3ges.gt.8) then
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        call mpi_finalize(ierr)
                        stop
                   end if

         if(ntdata.gt.0) then
          do i=1,ntdata
           ttype(i)=all_loc_data(iadtdata(i))%type
           tstaid(i)=all_loc_data(iadtdata(i))%staid
           terr(i)=all_loc_data(iadtdata(i))%error
           tlon(i)=all_loc_data(iadtdata(i))%lon
           tlat(i)=all_loc_data(iadtdata(i))%lat
           tpres(i)=all_loc_data(iadtdata(i))%pressure
           tobs(i)=all_loc_data(iadtdata(i))%tobs
           ttime(i)=all_loc_data(iadtdata(i))%time
           telev(i)=all_loc_data(iadtdata(i))%elevobs
           iqtflg(i)=nint(all_loc_data(iadtdata(i))%qtflag)
           tges(i)=all_loc_data(iadtdata(i))%tges
           tletaobs(i)=all_loc_data(iadtdata(i))%rletaobs
           bight(1:lbig3ges,i)=all_loc_data(iadtdata(i))%bigh(1:lbig3ges)
           ibight(1:lbig3ges,i)=all_loc_data(iadtdata(i))%ibigh(1:lbig3ges)
           tlong(i)=all_loc_data(iadtdata(i))%long
           tlatg(i)=all_loc_data(iadtdata(i))%latg
           tqm(i)=all_loc_data(iadtdata(i))%tqm
           itlabel(i)=all_loc_data(iadtdata(i))%label
          end do
         end if
           
       return
       end
