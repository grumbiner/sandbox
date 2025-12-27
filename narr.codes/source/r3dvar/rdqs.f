       subroutine rdqs(qerr,qlon,qlat,qlong,qlatg,qpres,qobs,qges,qsatges, &
                       qletaobs,bighq,ibighq, &
                       qstaid,qtime,qelev,qtobs,qqm,qtype,qmaxerr,iqlabel,nqdata,lbig3ges)

!   transfer moisture data from common

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

!        call mpi_comm_rank(my_comm,mype,ierr)

                   if(lbig3ges.gt.8) then
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        print *,' ILLEGAL VALUE OF IORDGES,LHALFGES,LBIG3GES'
                        call mpi_finalize(ierr)
                        stop
                   end if
         if(nqdata.gt.0) then
          do i=1,nqdata
           qtype(i)=all_loc_data(iadqdata(i))%type
           qstaid(i)=all_loc_data(iadqdata(i))%staid
           qerr(i)=all_loc_data(iadqdata(i))%error
           qlon(i)=all_loc_data(iadqdata(i))%lon
           qlat(i)=all_loc_data(iadqdata(i))%lat
           qpres(i)=all_loc_data(iadqdata(i))%pressure
           qobs(i)=all_loc_data(iadqdata(i))%qobs
           qges(i)=all_loc_data(iadqdata(i))%qges
           qsatges(i)=all_loc_data(iadqdata(i))%qsatges
           qletaobs(i)=all_loc_data(iadqdata(i))%rletaobs
           bighq(1:lbig3ges,i)=all_loc_data(iadqdata(i))%bigh(1:lbig3ges)
           ibighq(1:lbig3ges,i)=all_loc_data(iadqdata(i))%ibigh(1:lbig3ges)
           qtime(i)=all_loc_data(iadqdata(i))%time
           qtobs(i)=all_loc_data(iadqdata(i))%tobs
           qelev(i)=all_loc_data(iadqdata(i))%elevobs
           qmaxerr(i)=all_loc_data(iadqdata(i))%maxerror
           qlong(i)=all_loc_data(iadqdata(i))%long
           qlatg(i)=all_loc_data(iadqdata(i))%latg
           qqm(i)=all_loc_data(iadqdata(i))%qqm
           iqlabel(i)=all_loc_data(iadqdata(i))%label
          end do
         end if

       return
       end
