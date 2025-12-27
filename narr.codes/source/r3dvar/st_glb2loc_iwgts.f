subroutine st_glb2loc_iwgts(iwgts,ndata,lbig,imetaglb,jmetaglb,imeta,jmeta)

!   convert addresses iwgts from global to local coordinates

!  <--> iwgts:              input global, output local addresses
!   --> ndata:              number of observations
!   --> lbig:               number of interpolating points
!   --> imetaglb,jmetaglb:  global eta grid dimensions
!   --> imeta,jmeta:        local eta grid dimensions

         include 'PARMETA.comm'
         include 'mpif.h'
         include "my_comm.h"
         include 'mpp.h'

         integer(4) iwgts(ndata,lbig)

         if(idim2-idim1+1.ne.imeta.or.jdim2-jdim1+1.ne.jmeta) then
          print *,' incorrect local dimensions in transfer_etages'
          stop
         end if

         do l=1,lbig
          do n=1,ndata
           call get_ijk(iwgts(n,l),iglb,jglb,k,1,imetaglb,1,jmetaglb,1)
           i=iglb-my_is_glb+my_is_loc
           j=jglb-my_js_glb+my_js_loc
           iwgts(n,l)=max(1,i-idim1+1+(idim2-idim1+1)*((j-jdim1)+(jdim2-jdim1+1)*(k-1)))
          end do
         end do

       return
       end subroutine st_glb2loc_iwgts
