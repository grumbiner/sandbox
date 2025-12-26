       subroutine getshift(ishifth,ishiftv)

!   obtain variables ishifth and ishiftv, which indicate how to skip extra points 
!      on the global east boundary.

         INCLUDE "PARMETA.comm"
         INCLUDE "mpp.h"
         INCLUDE "mpif.h"
         include "my_comm.h"
         integer(4) ishifth(jdim1:jdim2),ishiftv(jdim1:jdim2)

         ishifth=0 ; ishiftv=0
         if(ircol.eq.1) then

!   we are at right global boundary, so now ishifth, ishiftv have meaning

          jloc=my_js_loc-1
          do jglb=my_js_glb,my_je_glb
           jloc=jloc+1
           ishifth(jloc)=mod(jglb-1,2)
           ishiftv(jloc)=mod(jglb,2)
          end do
          do j=my_js_loc-1,jdim1,-1
           jp1=j+1
           ishifth(j)=1-ishifth(jp1)
           ishiftv(j)=1-ishiftv(jp1)
          end do
          do j=my_je_loc+1,jdim2
           jm1=j-1
           ishifth(j)=1-ishifth(jm1)
           ishiftv(j)=1-ishiftv(jm1)
          end do

         end if
     

       RETURN
       END subroutine getshift

