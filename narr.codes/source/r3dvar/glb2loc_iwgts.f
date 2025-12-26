       subroutine glb2loc_iwgts(iwgts,ndata,lbig,nxglb,nyglb,nx,ny,igrid)

!   convert addresses iwgts from global to local coordinates

!  <--> iwgts:              input global, output local addresses
!   --> ndata:              number of observations
!   --> lbig:               number of interpolating points
!   --> imetaglb,jmetaglb:  global eta grid dimensions
!   --> imeta,jmeta:        local eta grid dimensions

         include 'types.h'
         include 'mpif.h'
      include "my_comm.h"
         include 'r3dv_data.comm'

         integer(4) iwgts(ndata,lbig)

         call mpi_comm_rank(my_comm,mype,ier)

         do l=1,lbig
          do n=1,ndata
           call get_ijk(iwgts(n,l),iglb,jglb,k,1,nxglb,1,nyglb,1)
           i=iglb-kx3_glb(mype,igrid)+kx3_loc(mype,igrid)
           j=jglb-ky3_glb(mype,igrid)+ky3_loc(mype,igrid)
           iwgts(n,l)=max(1,i+nx*((j-1)+ny*(k-1)))
          end do
         end do

       return
       end subroutine glb2loc_iwgts
