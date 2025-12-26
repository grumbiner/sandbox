       subroutine mpp_compgrid_init(rxglb,ryglb,halo_width,nxglb,nyglb,nx,ny,igrid)

!  set up various constants which define local computational grids
!    and halo exchange info.
!   halo exchange stuff is stored in r3dv_data.comm, and neighbor pe info and
!       other stuff is used from mpp.h, set up by eta model routine MPP_INIT.

!  --> rxglb:   input global coordinates in x (east-west) direction  (degrees)
!  --> ryglb:   input global coordinates in y (north-south) direction  (degrees)
!  --> halo_width: max expected halo width  (degrees)
!  --> nxglb:   x global dimension
!  --> nyglb:   y global dimension
! <--  nx:      x local dimension
! <--  ny:      y local dimension
!  --> igrid:   grid number

         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "mpif.h"
      include "my_comm.h"
         include "r3dv_data.comm"

         real(4) rxglb(nxglb),ryglb(nyglb)

         integer(4),allocatable::ilcol_table(:),ircol_table(:)
         integer(4),allocatable::ibrow_table(:),itrow_table(:)

!  find global limits for local domain

!   first, distribute flags which tell if subdomain is on edge of global domain

         allocate(ilcol_table(0:npes-1))
         allocate(ircol_table(0:npes-1))
         allocate(ibrow_table(0:npes-1))
         allocate(itrow_table(0:npes-1))
         ilcol_table(mype)=ilcol
         ircol_table(mype)=ircol
         ibrow_table(mype)=ibrow
         itrow_table(mype)=itrow

         do ipe=0,npes-1
          call mpi_bcast(ilcol_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
          call mpi_bcast(ircol_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
          call mpi_bcast(ibrow_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
          call mpi_bcast(itrow_table(ipe),1,mpi_integer,ipe,my_comm,ierr)
         end do
!             print *,' mype,ilcol,ircol,ibrow,itrow=', &
!                  mype,ilcol,ircol,ibrow,itrow
!             print *,' mype,ilcol_table=',mype,ilcol_table
!             print *,' mype,ircol_table=',mype,ircol_table
!             print *,' mype,ibrow_table=',mype,ibrow_table
!             print *,' mype,itrow_table=',mype,itrow_table

!     kx1--kx2  west halo range        ky1--ky2  south halo range
!     kx3--kx4  mirror interior halo   ky3--ky4  mirror interior halo
!     kx5--kx6  fully interior points  ky5--ky6  fully interior points
!     kx7--kx8  mirror interior halo   ky7--ky8  mirror interior halo
!     kx9--kx10 east halo range        ky9--ky10 north halo range

         nxmin=nxglb
         do ipe=0,npes-1
          if(ilcol_table(ipe).eq.1) then
           kx3_glb(ipe,igrid)=1
          else
           do i=1,nxglb
            if(rxglb(i).ge.rwdata_table(ipe)) then
             kx3_glb(ipe,igrid)=i
             exit
            end if
           end do
          end if
          if(ircol_table(ipe).eq.1) then
           kx8_glb(ipe,igrid)=nxglb
          else
           do i=nxglb,1,-1
            if(rxglb(i).lt.redata_table(ipe)) then
             kx8_glb(ipe,igrid)=i
             exit
            end if
           end do
          end if
          nxmin=min(kx8_glb(ipe,igrid)-kx3_glb(ipe,igrid)+1,nxmin)
         end do

         nymin=nyglb
         do ipe=0,npes-1
          if(ibrow_table(ipe).eq.1) then
           ky3_glb(ipe,igrid)=1
          else
           do i=1,nyglb
            if(ryglb(i).ge.rsdata_table(ipe)) then
             ky3_glb(ipe,igrid)=i
             exit
            end if
           end do
          end if
          if(itrow_table(ipe).eq.1) then
           ky8_glb(ipe,igrid)=nyglb
          else
           do i=nyglb,1,-1
            if(ryglb(i).lt.rndata_table(ipe)) then
             ky8_glb(ipe,igrid)=i
             exit
            end if
           end do
          end if
          nymin=min(ky8_glb(ipe,igrid)-ky3_glb(ipe,igrid)+1,nymin)
         end do

!  find halo information

         ixhalo=max(1,ceiling(halo_width/(rxglb(2)-rxglb(1))))
         iyhalo=max(1,ceiling(halo_width/(ryglb(2)-ryglb(1))))

!  check to see that halos are not too wide

         if(ixhalo.gt.nxmin.or.iyhalo.gt.nymin) then
          print *,' HALO AREA WIDER THAN SMALLEST SUBDOMAIN'
          call mpi_finalize(ierror)
          stop
         end if

         do ipe=0,npes-1
          ihthis=ixhalo
          if(ilcol_table(ipe).eq.1) ihthis=0
          kx1_glb(ipe,igrid)=kx3_glb(ipe,igrid)-ihthis
          kx2_glb(ipe,igrid)=kx3_glb(ipe,igrid)-1
          kx4_glb(ipe,igrid)=kx3_glb(ipe,igrid)+ihthis-1
          kx5_glb(ipe,igrid)=kx3_glb(ipe,igrid)+ihthis
          ihthis=ixhalo
          if(ircol_table(ipe).eq.1) ihthis=0
          kx10_glb(ipe,igrid)=kx8_glb(ipe,igrid)+ihthis
          kx9_glb(ipe,igrid)=kx8_glb(ipe,igrid)+1
          kx7_glb(ipe,igrid)=kx8_glb(ipe,igrid)-ihthis+1
          kx6_glb(ipe,igrid)=kx8_glb(ipe,igrid)-ihthis
          ihthis=iyhalo
          if(ibrow_table(ipe).eq.1) ihthis=0
          ky1_glb(ipe,igrid)=ky3_glb(ipe,igrid)-ihthis
          ky2_glb(ipe,igrid)=ky3_glb(ipe,igrid)-1
          ky4_glb(ipe,igrid)=ky3_glb(ipe,igrid)+ihthis-1
          ky5_glb(ipe,igrid)=ky3_glb(ipe,igrid)+ihthis
          ihthis=iyhalo
          if(itrow_table(ipe).eq.1) ihthis=0
          ky10_glb(ipe,igrid)=ky8_glb(ipe,igrid)+ihthis
          ky9_glb(ipe,igrid)=ky8_glb(ipe,igrid)+1
          ky7_glb(ipe,igrid)=ky8_glb(ipe,igrid)-ihthis+1
          ky6_glb(ipe,igrid)=ky8_glb(ipe,igrid)-ihthis
         end do

!    next get local numbers

         do ipe=0,npes-1
          kx1_loc(ipe,igrid)=1
          kx2_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx2_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx3_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx3_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx4_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx4_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx5_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx5_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx6_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx6_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx7_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx7_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx8_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx8_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx9_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx9_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          kx10_loc(ipe,igrid)=kx1_loc(ipe,igrid)+kx10_glb(ipe,igrid)-kx1_glb(ipe,igrid)
          ky1_loc(ipe,igrid)=1
          ky2_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky2_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky3_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky3_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky4_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky4_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky5_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky5_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky6_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky6_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky7_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky7_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky8_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky8_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky9_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky9_glb(ipe,igrid)-ky1_glb(ipe,igrid)
          ky10_loc(ipe,igrid)=ky1_loc(ipe,igrid)+ky10_glb(ipe,igrid)-ky1_glb(ipe,igrid)
         end do

         nx=kx10_loc(mype,igrid)
         ny=ky10_loc(mype,igrid)

!        if(mype.eq.0) then
!         print *,' kx1_glb=',kx1_glb(:,igrid)
!         print *,' kx2_glb=',kx2_glb(:,igrid)
!         print *,' kx3_glb=',kx3_glb(:,igrid)
!         print *,' kx4_glb=',kx4_glb(:,igrid)
!         print *,' kx5_glb=',kx5_glb(:,igrid)
!         print *,' kx6_glb=',kx6_glb(:,igrid)
!         print *,' kx7_glb=',kx7_glb(:,igrid)
!         print *,' kx8_glb=',kx8_glb(:,igrid)
!         print *,' kx9_glb=',kx9_glb(:,igrid)
!         print *,' kx10_glb=',kx10_glb(:,igrid)
!         print *,' kx1_loc=',kx1_loc(:,igrid)
!         print *,' kx2_loc=',kx2_loc(:,igrid)
!         print *,' kx3_loc=',kx3_loc(:,igrid)
!         print *,' kx4_loc=',kx4_loc(:,igrid)
!         print *,' kx5_loc=',kx5_loc(:,igrid)
!         print *,' kx6_loc=',kx6_loc(:,igrid)
!         print *,' kx7_loc=',kx7_loc(:,igrid)
!         print *,' kx8_loc=',kx8_loc(:,igrid)
!         print *,' kx9_loc=',kx9_loc(:,igrid)
!         print *,' kx10_loc=',kx10_loc(:,igrid)
!         print *,' ky1_glb=',ky1_glb(:,igrid)
!         print *,' ky2_glb=',ky2_glb(:,igrid)
!         print *,' ky3_glb=',ky3_glb(:,igrid)
!         print *,' ky4_glb=',ky4_glb(:,igrid)
!         print *,' ky5_glb=',ky5_glb(:,igrid)
!         print *,' ky6_glb=',ky6_glb(:,igrid)
!         print *,' ky7_glb=',ky7_glb(:,igrid)
!         print *,' ky8_glb=',ky8_glb(:,igrid)
!         print *,' ky9_glb=',ky9_glb(:,igrid)
!         print *,' ky10_glb=',ky10_glb(:,igrid)
!         print *,' ky1_loc=',ky1_loc(:,igrid)
!         print *,' ky2_loc=',ky2_loc(:,igrid)
!         print *,' ky3_loc=',ky3_loc(:,igrid)
!         print *,' ky4_loc=',ky4_loc(:,igrid)
!         print *,' ky5_loc=',ky5_loc(:,igrid)
!         print *,' ky6_loc=',ky6_loc(:,igrid)
!         print *,' ky7_loc=',ky7_loc(:,igrid)
!         print *,' ky8_loc=',ky8_loc(:,igrid)
!         print *,' ky9_loc=',ky9_loc(:,igrid)
!         print *,' ky10_loc=',ky10_loc(:,igrid)
!        end if
           
         deallocate(ilcol_table)
         deallocate(ircol_table)
         deallocate(ibrow_table)
         deallocate(itrow_table)

!  store rxglb and ryglb in common

         deallocate(gridcom(igrid)%rxglbcom,stat=ier)
         allocate(gridcom(igrid)%rxglbcom(nxglb))
         gridcom(igrid)%nxglbcom=nxglb
         gridcom(igrid)%rxglbcom=rxglb

         deallocate(gridcom(igrid)%ryglbcom,stat=ier)
         allocate(gridcom(igrid)%ryglbcom(nxglb))
         gridcom(igrid)%nyglbcom=nyglb
         gridcom(igrid)%ryglbcom=ryglb
         
!  store rx and ry in common

         deallocate(gridcom(igrid)%rxcom,stat=ier)
         allocate(gridcom(igrid)%rxcom(nx))
         gridcom(igrid)%nxcom=nx
         gridcom(igrid)%rxcom=rxglb(kx1_glb(mype,igrid):kx10_glb(mype,igrid))

         deallocate(gridcom(igrid)%rycom,stat=ier)
         allocate(gridcom(igrid)%rycom(ny))
         gridcom(igrid)%nycom=ny
         gridcom(igrid)%rycom=ryglb(ky1_glb(mype,igrid):ky10_glb(mype,igrid))


       return
       end subroutine mpp_compgrid_init
