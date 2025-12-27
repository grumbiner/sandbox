       subroutine redistribute_rad(mbufrad,nsat,nrad_dat) 

!  using information about partition of eta model domain among pe's, 
!  redistribute data so appropriate data is on each pe.

!  redistributed data is contained in dynamic space referred to from pointers
!  in common/r3dv_data/

!  --> allrad0:   contains unorganized data
!  --> mbufrad:   number of observations in allrad0
!  --> nsat:      number of satellites


         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
      include "my_comm.h"
         include "MAPOT1.comm"
         include "r3dv_data.comm"

         real(4),pointer::allrad0(:,:)
         common/radbuf/allrad0

         integer(4),allocatable::nsend_data(:),ndsend_data(:), &
                                 nrecv_data(:),ndrecv_data(:), &
                                 indx(:)
         integer(8),allocatable::idest_data8(:)
         integer(4),allocatable::idest_data(:)
         real(4),allocatable::sendbuf(:,:)
         integer(4) icount(nsat)

!  define mpi type for type "rad_obs"

         len4rad=47
         call mpi_type_contiguous(len4rad,mpi_real4,mpi_rad_obs,ierror)
         call mpi_type_commit(mpi_rad_obs,ierror)


!           print *,' in redistribute_rad,mype=',mype
!           print *,' mype,redata_table=',mype,redata_table
!           print *,' mype,rwdata_table=',mype,rwdata_table
!           print *,' mype,rsdata_table=',mype,rsdata_table
!           print *,' mype,rndata_table=',mype,rndata_table


         allocate(idest_data8(max(1,mbufrad)))
         allocate(indx(max(1,mbufrad)))
         idest_data8=-1
         len_sendbuf=0
         num_outside=0
         if(mbufrad.gt.0) then

          do i=1,mbufrad

           rlatg=allrad0(44,i)
           rlong=allrad0(45,i)

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
!         print*,'mbufrad,num_outside=',mbufrad,num_outside
          len_sendbuf=mbufrad-num_outside

         end if
         call mpi_reduce(num_outside,num_outside_all,1,mpi_integer, &
                  mpi_sum,0,my_comm,ierr)
         if(mype.eq.0) print *, &
           ' in redistribute_rad, number of obs outside full domain = ', &
                                num_outside_all

         allocate(sendbuf(len4rad,max(1,len_sendbuf)))
         allocate(idest_data(max(1,len_sendbuf)))

         if(len_sendbuf.gt.0) then

!   sort data by order of increasing target pe number

          call indexxi8(mbufrad,idest_data8,indx)

!         print*,'len_sendbuf,len4rad=',len_sendbuf,len4rad
!         print*,'num_outside=',num_outside
          do i=1,len_sendbuf
           sendbuf(1:len4rad,i)=allrad0(1:len4rad,indx(i+num_outside))
!          if(mype.eq.4) then
!          print*,'igroup=',sendbuf(43,i)
!          print*,'rlat=',sendbuf(3,i)
!          print*,'rlon=',sendbuf(4,i)
!          endif
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
         do ipe=0,npes-1
          call mpi_gather(nsend_data(ipe),1,mpi_integer, &
                  nrecv_data,1,mpi_integer,ipe,my_comm,ierr)
         end do

         nlocrad=sum(nrecv_data)
         nrad_dat=nlocrad
                 call mpi_reduce(nlocrad,nglbdata,1,mpi_integer, &
                        mpi_sum,0,my_comm,ierr)
                 if(mype.eq.0) print *,' in redistribute_rad, nglbdata=',nglbdata
         allocate(all_loc_rad(len4rad,max(1,nlocrad)))
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
                 sendbuf,nsend_data,ndsend_data,mpi_rad_obs, &
                 all_loc_rad,nrecv_data,ndrecv_data,mpi_rad_obs, &
                       my_comm,ierr)
!           count up number of misplaced obs
                if(nlocrad.gt.0) then
                 notlost=0
                 do i=1,nlocrad
                  rlatg=all_loc_rad(44,i)
                  rlong=all_loc_rad(45,i)
                  if(rlong.ge.rwdata_table(mype).and. &
                     rlong.lt.redata_table(mype).and. &
                     rlatg.ge.rsdata_table(mype).and. &
                     rlatg.lt.rndata_table(mype)) notlost=notlost+1
                 end do
!                print *, &
!                 ' in redistribute_rad, mype,notlost,nlocrad=', &
!                 mype,notlost,nlocrad
                
!            print *, &
!              ' in redistribute_rad, mype,ierr for alltoallv = ', &
!                  mype,ierr
!                 print *,' mype,max,min lat=',mype, &
!                  maxval(all_loc_rad(44,:)), &
!                  minval(all_loc_rad(44,:))
!                 print *,' mype,max,min lon=',mype, &
!                  maxval(all_loc_rad(45,:)), &
!                  minval(all_loc_rad(45,:))
                end if

         deallocate(sendbuf)
         deallocate(nsend_data)
         deallocate(ndsend_data)
         deallocate(nrecv_data)
         deallocate(ndrecv_data)

! count up number of each type of data

         allocate(nrad_com(nsat))
         nrad_com(:)%count=0
         if(nlocrad.gt.0) then
          do i=1,nlocrad
           igroup=nint(all_loc_rad(43,i))
           nrad_com(igroup)%count=nrad_com(igroup)%count+1
          end do
          do k=1,nsat
           ndatthis=nrad_com(k)%count
           if(ndatthis.gt.0) allocate(nrad_com(k)%adlist(ndatthis))
          end do
          icount=0
          do i=1,nlocrad
           igroup=nint(all_loc_rad(43,i))
           icount(igroup)=icount(igroup)+1
           nrad_com(igroup)%adlist(icount(igroup))=i
          end do
         end if
!              print *,' in redistribute_rad, mype,nrad_com=', &
!                         mype,nrad_com(:)%count

         call mpi_type_free(mpi_rad_obs,ierror)

       return
       end subroutine redistribute_rad
