subroutine someof_etages(test_value, &
           runout,idatout,ihrstout,ntsdout, &
           htmout,vtmout,resout,u00out,imeta,jmeta,myisout,myieout,myjsout,myjeout)

! transfer eta guess fields from model common blocks to arrays that
! will be used to interpolate the guess to observation locations

         include "PARMETA.comm"
         include "mpif.h"
         include "my_comm.h"
         include "mpp.h"

         LOGICAL runout,RUN,FIRST,RESTRT,SIGMA

         include "LOOPS.comm"
         include "MASKS.comm"
         include "PVRBLS.comm"
         include "VRBLS.comm"
         include "CTLBLK.comm"
         include "CLDWTR.comm"

         real(4) htmout(imeta,jmeta,*),vtmout(imeta,jmeta,*)
         real(4) resout(imeta,jmeta)
         real(4) u00out(imeta,jmeta)
         integer(4) idatout(3)

         if(idim2-idim1+1.ne.imeta.or.jdim2-jdim1+1.ne.jmeta) then
          print *,' incorrect local dimensions in transfer_etages'
          stop
         end if

         runout=run
         idatout=idat
         ihrstout=ihrst
         ntsdout=ntsd

!  first, fill points outside subdomain with special values

         special_value=1.e20
         test_value=.98*special_value
         do j=jdim1,jdim2
          do i=idim1,myis-1
           res(i,j)=special_value
           u00(i,j)=special_value
          end do
          do i=myie+1,idim2
           res(i,j)=special_value
           u00(i,j)=special_value
          end do
         end do
         do j=jdim1,myjs-1
          do i=idim1,idim2
           res(i,j)=special_value
           u00(i,j)=special_value
          end do
         end do
         do j=myje+1,jdim2
          do i=idim1,idim2
           res(i,j)=special_value
           u00(i,j)=special_value
          end do
         end do

!  now, do halo exchange on variables

         call exch(res,1,5,5)
         call exch(u00,1,5,5)
         call exch(htm,lm,5,5)
         call exch(vtm,lm,5,5)

!  finally, transfer to output arrays

         do j=jdim1,jdim2
          jj=j-jdim1+1
          do i=idim1,idim2
           ii=i-idim1+1
           resout(ii,jj)=res(i,j)
           u00out(ii,jj)=u00(i,j)
          end do
         end do

         do k=1,lm
          do j=jdim1,jdim2
           jj=j-jdim1+1
           do i=idim1,idim2
            ii=i-idim1+1
            htmout(ii,jj,k)=htm(i,j,k)
            vtmout(ii,jj,k)=vtm(i,j,k)
           end do
          end do
         end do

         myisout=myis-idim1+1
         myieout=myie-idim1+1
         myjsout=myjs-jdim1+1
         myjeout=myje-jdim1+1

       return
       end subroutine someof_etages
