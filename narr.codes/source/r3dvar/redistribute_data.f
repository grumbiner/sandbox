       subroutine redistribute_data(mbufdat) 

!  using information about partition of eta model domain among pe's, 
!  redistribute data so appropriate data is on each pe.

!  redistributed data is contained in dynamic space referred to from pointers
!  in common/r3dv_data/

!  --> alldata0:   contains unorganized data
!  --> mbufdat:    number of observations in alldata0


         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
      include "my_comm.h"
         include "MAPOT1.comm"
         include "r3dv_data.comm"

  type(general_obs),pointer::alldata0(:)
  integer(2),pointer::lev_val(:),lev_max(:)
  common/databuf/lev_val,lev_max,alldata0

         integer(4),allocatable::nsend_data(:),ndsend_data(:), &
                                 nrecv_data(:),ndrecv_data(:), &
                                 indx(:)
         integer(8),allocatable::idest_data8(:)
         integer(4),allocatable::idest_data(:)
         type(general_obs),allocatable::sendbuf(:)
         integer(4),allocatable::my_n_table(:),my_s_table(:)
         integer(4),allocatable::my_e_table(:),my_w_table(:)

!  define mpi type for type "general_obs"

         call mpi_type_contiguous(len4_general_obs,mpi_integer4,mpi_general_obs,ierror)
         call mpi_type_commit(mpi_general_obs,ierror)

!  compute coordinate limits for local domain

         redata=wbd+2.*dlmd*(ie_glb_table(mype)-1)+1.5*dlmd
!-------------------different limit if at east boundary of global domain
         if(ircol.eq.1) &
             redata=wbd+2.*dlmd*(ie_glb_table(mype)-1)-1.0001*dlmd

         rwdata=wbd+2.*dlmd*(is_glb_table(mype)-1)-.5*dlmd
!-------------------different limit if at west boundary of global domain
         if(ilcol.eq.1) &
             rwdata=wbd+1.0001*dlmd
           
         rndata=sbd+(je_glb_table(mype)-1)*dphd+.5*dphd
!-------------------different limit if at north boundary of global domain
         if(itrow.eq.1) &
             rndata=sbd+(je_glb_table(mype)-1)*dphd-1.0001*dphd

         rsdata=sbd+(js_glb_table(mype)-1)*dphd-.5*dphd
!-------------------different limit if at south boundary of global domain
         if(ibrow.eq.1) &
             rsdata=sbd+1.0001*dphd

!   all pe's need to have domain limits for all pe's before we can redistribute data

         allocate(redata_table(0:npes-1))
         allocate(rwdata_table(0:npes-1))
         allocate(rsdata_table(0:npes-1))
         allocate(rndata_table(0:npes-1))
         redata_table(mype)=redata
         rwdata_table(mype)=rwdata
         rsdata_table(mype)=rsdata
         rndata_table(mype)=rndata

         do ipe=0,npes-1
          call mpi_bcast(redata_table(ipe),1,mpi_real,ipe,my_comm,ierr)
          call mpi_bcast(rwdata_table(ipe),1,mpi_real,ipe,my_comm,ierr)
          call mpi_bcast(rsdata_table(ipe),1,mpi_real,ipe,my_comm,ierr)
          call mpi_bcast(rndata_table(ipe),1,mpi_real,ipe,my_comm,ierr)
         end do

!     make sure that west boundary of one domain is exactly the same as east
!     boundary of neighbor to the west, etc.

         allocate(my_n_table(0:npes-1))
         allocate(my_s_table(0:npes-1))
         allocate(my_e_table(0:npes-1))
         allocate(my_w_table(0:npes-1))
         my_n_table(mype)=my_n
         my_s_table(mype)=my_s
         my_e_table(mype)=my_e
         my_w_table(mype)=my_w
         do ipe=0,npes-1
          call mpi_bcast(my_n_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
          call mpi_bcast(my_s_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
          call mpi_bcast(my_e_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
          call mpi_bcast(my_w_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
         end do
!              print *,' mype,my_n_table=',mype,my_n_table
!              print *,' mype,my_s_table=',mype,my_s_table
!              print *,' mype,my_e_table=',mype,my_e_table
!              print *,' mype,my_w_table=',mype,my_w_table
         do ipe=0,npes-1
          if(my_n_table(ipe).ge.0) then
           nebpe=my_n_table(ipe)
           rsdata_table(nebpe)=rndata_table(ipe)
          end if
          if(my_e_table(ipe).ge.0) then
           nebpe=my_e_table(ipe)
           rwdata_table(nebpe)=redata_table(ipe)
          end if
         end do
         deallocate(my_n_table)
         deallocate(my_s_table)
         deallocate(my_e_table)
         deallocate(my_w_table)


!                print *,' mype,rsdata,rndata,redata,rwdata=', &
!                          mype,rsdata,rndata,redata,rwdata
!              print *,' mype,redata_table=',mype,redata_table
!              print *,' mype,rwdata_table=',mype,rwdata_table
!              print *,' mype,rsdata_table=',mype,rsdata_table
!              print *,' mype,rndata_table=',mype,rndata_table


         allocate(idest_data8(max(1,mbufdat)))
         allocate(indx(max(1,mbufdat)))
         idest_data8=-1
         len_sendbuf=0
         num_outside=0
         if(mbufdat.gt.0) then

!            write(0,*)' in redistribute_data, mype,max,min long=', &
!                  maxval(alldata0(1:mbufdat)%long),minval(alldata0(1:mbufdat)%long)
!            write(0,*)' in redistribute_data, mype,max,min latg=', &
!                  maxval(alldata0(1:mbufdat)%latg),minval(alldata0(1:mbufdat)%latg)
!            print *,'  in redistribute_data, mype,max,min type before redistribution=', &
!                  mype,maxval(alldata0(1:mbufdat)%type),minval(alldata0(1:mbufdat)%type)
          do i=1,mbufdat
!                if(abs(nint(alldata0(i)%type)).gt.2299.and.mype.eq.24) &
!                    print *,' problem type at mype,i,mbufdat,itype,lon,lat=',mype,i,mbufdat, &
!                              alldata0(i)%type,alldata0(i)%lon,alldata0(i)%lat

!   convert lat-lon coordinates of data to rotated model lat-lon coordinates

           rlon=alldata0(i)%lon ; rlat=alldata0(i)%lat
           rlong=alldata0(i)%long ; rlatg=alldata0(i)%latg

!   assign destination pe number to each observation

           do ipe=0,npes-1
            if(rlong.ge.rwdata_table(ipe).and. &
               rlong.lt.redata_table(ipe).and. &
               rlatg.ge.rsdata_table(ipe).and. &
               rlatg.lt.rndata_table(ipe)) idest_data8(i)=ipe
           end do
          end do

!    count number of obs that fall outside global domain (idest_data=-1)

          num_outside=count(idest_data8.eq.-1)
          if(mbufdat.eq.0) num_outside=0
          len_sendbuf=mbufdat-num_outside

         end if
         call mpi_reduce(num_outside,num_outside_all,1,mpi_integer, &
                  mpi_sum,0,my_comm,ierr)
!        if(mype.eq.0) print *,' number of obs outside full domain = ', &
!                               num_outside_all

         allocate(sendbuf(max(1,len_sendbuf)))
         allocate(idest_data(max(1,len_sendbuf)))

         if(len_sendbuf.gt.0) then

!   sort data by order of increasing target pe number

          call indexxi8(mbufdat,idest_data8,indx)

          do i=1,len_sendbuf
           sendbuf(i)=alldata0(indx(i+num_outside))
           idest_data(i)=idest_data8(indx(i+num_outside))
          end do

         end if
         deallocate(idest_data8)
         deallocate(indx)

!  now get remaining info necessary for using alltoall command

         allocate(nsend_data(0:npes-1))
         allocate(ndsend_data(0:npes-1))
         nsend_data=0
         ndsend_data=0
         if(len_sendbuf.gt.0) then
          do ipe=0,npes-1
           nsend_data(ipe)=count(idest_data.eq.ipe)
           if(ipe.gt.0) ndsend_data(ipe)=ndsend_data(ipe-1)+nsend_data(ipe-1)
          end do

         end if
         deallocate(idest_data)

         allocate(nrecv_data(0:npes-1))
         nrecv_data=0
         do ipe=0,npes-1
          call mpi_gather(nsend_data(ipe),1,mpi_integer, &
                  nrecv_data,1,mpi_integer,ipe,my_comm,ierr)
         end do

         nlocdata=sum(nrecv_data)
                 call mpi_reduce(nlocdata,nglbdata,1,mpi_integer,mpi_sum,0,my_comm,ierr)
!                if(mype.eq.0) print *,' nglbdata=',nglbdata
         allocate(all_loc_data(max(1,nlocdata)))
         allocate(ndrecv_data(0:npes-1))
         ndrecv_data(0)=0
         do ipe=0,npes-1
          if(ipe.gt.0) ndrecv_data(ipe)=ndrecv_data(ipe-1)+nrecv_data(ipe-1)
         end do


!     redistribute data with alltoall command

         nsend_data=nsend_data
         ndsend_data=ndsend_data
         nrecv_data=nrecv_data
         ndrecv_data=ndrecv_data
         call mpi_alltoallv( &
                 sendbuf,nsend_data,ndsend_data,mpi_general_obs, &
                 all_loc_data,nrecv_data,ndrecv_data,mpi_general_obs, &
                       my_comm,ierr)
!           count up number of misplaced obs
                if(nlocdata.gt.0) then
                 notlost=0
                 do i=1,nlocdata
                  rlong=all_loc_data(i)%long
                  rlatg=all_loc_data(i)%latg
                  if(rlong.ge.rwdata_table(mype).and. &
                     rlong.lt.redata_table(mype).and. &
                     rlatg.ge.rsdata_table(mype).and. &
                     rlatg.lt.rndata_table(mype)) notlost=notlost+1
                 end do
!                print *,' mype,notlost,nlocdata=',mype,notlost,nlocdata
                end if
                
!            print *,' mype,ierr for alltoallv = ',mype,ierr
!                 print *,' mype,max,min lon=',mype, &
!                  maxval(all_loc_data%long), &
!                  minval(all_loc_data%long)
!                 print *,' mype,max,min lat=',mype, &
!                  maxval(all_loc_data%latg), &
!                  minval(all_loc_data%latg)
!             if(nlocdata.gt.0) &
!            print *,'  in redistribute_data, mype,max,min type after redistribution=', &
!                  mype,maxval(all_loc_data(1:nlocdata)%type),minval(all_loc_data(1:nlocdata)%type)

         deallocate(sendbuf)
         deallocate(nsend_data)
         deallocate(ndsend_data)
         deallocate(nrecv_data)
         deallocate(ndrecv_data)

! count up number of each type of data

         npdatacom=0 ; ntdatacom=0
         nqdatacom=0 ; npwdatacom=0
         nwdatacom=0
         if(nlocdata.gt.0) then
          do i=1,nlocdata
           igroup=nint(all_loc_data(i)%group)
           if(igroup.eq.1002) npdatacom=npdatacom+1
           if(igroup.eq.1004) ntdatacom=ntdatacom+1
           if(igroup.eq.1006) nqdatacom=nqdatacom+1
           if(igroup.eq.1008) npwdatacom=npwdatacom+1
           if(igroup.eq.2002) nwdatacom=nwdatacom+1
          end do
         end if
         allocate(iadpdata(max(1,npdatacom)))
         allocate(iadtdata(max(1,ntdatacom)))
         allocate(iadqdata(max(1,nqdatacom)))
         allocate(iadpwdata(max(1,npwdatacom)))
         allocate(iadwdata(max(1,nwdatacom)))
         if(nlocdata.gt.0) then
          ipdat=0 ; itdat=0 ; iqdat=0
          ipwdat=0 ; iwdat=0
          do i=1,nlocdata
           igroup=nint(all_loc_data(i)%group)
           if(igroup.eq.1002) then
            ipdat=ipdat+1
            iadpdata(ipdat)=i
           end if
           if(igroup.eq.1004) then
            itdat=itdat+1
            iadtdata(itdat)=i
           end if
           if(igroup.eq.1006) then
            iqdat=iqdat+1
            iadqdata(iqdat)=i
           end if
           if(igroup.eq.1008) then
            ipwdat=ipwdat+1
            iadpwdata(ipwdat)=i
           end if
           if(igroup.eq.2002) then
            iwdat=iwdat+1
            iadwdata(iwdat)=i
           end if
          end do
         end if

!    allocate sundry other arrays that will be needed later

         allocate(kx1_glb(0:npes-1,3))
         allocate(kx2_glb(0:npes-1,3))
         allocate(kx3_glb(0:npes-1,3))
         allocate(kx4_glb(0:npes-1,3))
         allocate(kx5_glb(0:npes-1,3))
         allocate(kx6_glb(0:npes-1,3))
         allocate(kx7_glb(0:npes-1,3))
         allocate(kx8_glb(0:npes-1,3))
         allocate(kx9_glb(0:npes-1,3))
         allocate(kx10_glb(0:npes-1,3))
         allocate(ky1_glb(0:npes-1,3))
         allocate(ky2_glb(0:npes-1,3))
         allocate(ky3_glb(0:npes-1,3))
         allocate(ky4_glb(0:npes-1,3))
         allocate(ky5_glb(0:npes-1,3))
         allocate(ky6_glb(0:npes-1,3))
         allocate(ky7_glb(0:npes-1,3))
         allocate(ky8_glb(0:npes-1,3))
         allocate(ky9_glb(0:npes-1,3))
         allocate(ky10_glb(0:npes-1,3))
         allocate(kx1_loc(0:npes-1,3))
         allocate(kx2_loc(0:npes-1,3))
         allocate(kx3_loc(0:npes-1,3))
         allocate(kx4_loc(0:npes-1,3))
         allocate(kx5_loc(0:npes-1,3))
         allocate(kx6_loc(0:npes-1,3))
         allocate(kx7_loc(0:npes-1,3))
         allocate(kx8_loc(0:npes-1,3))
         allocate(kx9_loc(0:npes-1,3))
         allocate(kx10_loc(0:npes-1,3))
         allocate(ky1_loc(0:npes-1,3))
         allocate(ky2_loc(0:npes-1,3))
         allocate(ky3_loc(0:npes-1,3))
         allocate(ky4_loc(0:npes-1,3))
         allocate(ky5_loc(0:npes-1,3))
         allocate(ky6_loc(0:npes-1,3))
         allocate(ky7_loc(0:npes-1,3))
         allocate(ky8_loc(0:npes-1,3))
         allocate(ky9_loc(0:npes-1,3))
         allocate(ky10_loc(0:npes-1,3))

         call mpi_type_free(mpi_general_obs,ierror)

       return
       end subroutine redistribute_data
