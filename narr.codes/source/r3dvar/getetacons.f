       subroutine getetacons(lmeta,imeta,jmeta,erlon0,erlat0, &
         dlon0,dlat0,ptop,wb8,sb8,wbglb,sbglb,istagh,istagv,eta1,aeta1)

!-------- now that we know something about eta parameters, get remaining
!-------- parameters from eta model common/MAPOT/ which have been initialized
!-------- from the restart and constants files.

         include "mpif.h"
      include "my_comm.h"
         include "PARMETA.comm"
         include "MAPOT.comm"
         include "mpp.h"

         real(4) aeta1(lmeta),eta1(lmeta+1)
         real(8) wb8,sb8

         call mpi_comm_rank(my_comm,mype,ierr)
         iroot=0


         imeta=idim2-idim1+1
         jmeta=jdim2-jdim1+1
         erlon0=tlm0d
         erlat0=tph0d
         dlon0=dlmd
         dlat0=dphd
         ptop=.01*pt
         wbglb=wbd
         sbglb=sbd
         i0glb=my_is_glb-(my_is_loc-idim1)
         j0glb=my_js_glb-(my_js_loc-jdim1)
         wb8=wbglb+(i0glb-1._8)*2._8*dlon0
         sb8=sbglb+(j0glb-1._8)*dlat0
         istagh=0 ; istagv=1
         if(mod(j0glb,2).eq.0) then
          istagh=1 ; istagv=0
         end if
         eta1=eta
         aeta1=aeta
         if(mype.eq.iroot) then
          print *,' in getetacons'
          print *,' imeta,jmeta=',imeta,jmeta
          print *,' erlon0,erlat0=',erlon0,erlat0
          print *,' dlon0,dlat0=',dlon0,dlat0
          print *,' ptop=',ptop
          print *,' wb,sb=',wb8,sb8
          print *,' wbglb,sbglb=',wbglb,sbglb
          print *,' eta1(1)=',eta1(1)
          do k=1,lmeta
           kp=k+1
           print *,'   aeta1(',k,')=',aeta1(k)
           print *,' eta1(',kp,')=',eta1(kp)
          end do
         end if


       return
       end subroutine getetacons
