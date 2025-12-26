subroutine transfer_etages(test_value, &
           runout,idatout,ihrstout,ntsdout, &
           tges,qges,q2ges,uges,vges,cwmout,lmhout,lmvout,htmout,vtmout, &
           pdout,resout,pdres01,w10ges,sfctout,smout,siceout,snoout, &
           veg_type,veg_frac,soil_type,soil_temp,soil_moi, &
           zges,u00out,imeta,jmeta,myisout,myieout,myjsout,myjeout)

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
         include "SOIL.comm"

         real(4) tges(imeta,jmeta,*),qges(imeta,jmeta,*)
         real(4) uges(imeta,jmeta,*),vges(imeta,jmeta,*)
         real(4) q2ges(imeta,jmeta,*),cwmout(imeta,jmeta,*)
         real(4) htmout(imeta,jmeta,*),vtmout(imeta,jmeta,*)
         real(4) pdout(imeta,jmeta),resout(imeta,jmeta)
         real(4) pdres01(imeta,jmeta),w10ges(imeta,jmeta)
         real(4) sfctout(imeta,jmeta),smout(imeta,jmeta)
         real(4) siceout(imeta,jmeta),snoout(imeta,jmeta)
         real(4) veg_type(imeta,jmeta)
         real(4) veg_frac(imeta,jmeta)
         real(4) soil_type(imeta,jmeta)
         real(4) soil_temp(imeta,jmeta)
         real(4) soil_moi(imeta,jmeta)
         real(4) zges(imeta,jmeta),u00out(imeta,jmeta)
         integer(4) lmhout(imeta,jmeta),lmvout(imeta,jmeta)
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
           u10(i,j)=special_value
           v10(i,j)=special_value
           ths(i,j)=special_value
           sm(i,j)=special_value
           sice(i,j)=special_value
           sno(i,j)=special_value
           pd(i,j)=special_value
           res(i,j)=special_value
           fis(i,j)=special_value
           u00(i,j)=special_value
           lmh(i,j)=lm
           lmv(i,j)=lm
          end do
          do i=myie+1,idim2
           u10(i,j)=special_value
           v10(i,j)=special_value
           ths(i,j)=special_value
           sm(i,j)=special_value
           sice(i,j)=special_value
           sno(i,j)=special_value
           pd(i,j)=special_value
           res(i,j)=special_value
           fis(i,j)=special_value
           u00(i,j)=special_value
           lmh(i,j)=lm
           lmv(i,j)=lm
          end do
         end do
         do j=jdim1,myjs-1
          do i=idim1,idim2
           u10(i,j)=special_value
           v10(i,j)=special_value
           ths(i,j)=special_value
           sm(i,j)=special_value
           sice(i,j)=special_value
           sno(i,j)=special_value
           pd(i,j)=special_value
           res(i,j)=special_value
           fis(i,j)=special_value
           u00(i,j)=special_value
           lmh(i,j)=lm
           lmv(i,j)=lm
          end do
         end do
         do j=myje+1,jdim2
          do i=idim1,idim2
           u10(i,j)=special_value
           v10(i,j)=special_value
           ths(i,j)=special_value
           sm(i,j)=special_value
           sice(i,j)=special_value
           sno(i,j)=special_value
           pd(i,j)=special_value
           res(i,j)=special_value
           fis(i,j)=special_value
           u00(i,j)=special_value
           lmh(i,j)=lm
           lmv(i,j)=lm
          end do
         end do
         do k=1,lm
          do j=jdim1,jdim2
           do i=idim1,myis-1
            t(i,j,k)=special_value
            q(i,j,k)=special_value
            q2(i,j,k)=special_value
            u(i,j,k)=special_value
            v(i,j,k)=special_value
            cwm(i,j,k)=special_value
           end do
           do i=myie+1,idim2
            t(i,j,k)=special_value
            q(i,j,k)=special_value
            q2(i,j,k)=special_value
            u(i,j,k)=special_value
            v(i,j,k)=special_value
            cwm(i,j,k)=special_value
           end do
          end do
          do j=jdim1,myjs-1
           do i=idim1,idim2
            t(i,j,k)=special_value
            q(i,j,k)=special_value
            q2(i,j,k)=special_value
            u(i,j,k)=special_value
            v(i,j,k)=special_value
            cwm(i,j,k)=special_value
           end do
          end do
          do j=myje+1,jdim2
           do i=idim1,idim2
            t(i,j,k)=special_value
            q(i,j,k)=special_value
            q2(i,j,k)=special_value
            u(i,j,k)=special_value
            v(i,j,k)=special_value
            cwm(i,j,k)=special_value
           end do
          end do
         end do

!  now, do halo exchange on variables

         call exch(u10,1,5,5)
         call exch(v10,1,5,5)
         call exch(ths,1,5,5)
         call exch(sm,1,5,5)
         call exch(sice,1,5,5)
         call exch(sno,1,5,5)
         call exch(pd,1,5,5)
         call exch(res,1,5,5)
         call exch(fis,1,5,5)
         call exch(u00,1,5,5)
         call exch(t,lm,5,5)
         call exch(q,lm,5,5)
         call exch(q2,lm,5,5)
         call exch(u,lm,5,5)
         call exch(v,lm,5,5)
         call exch(cwm,lm,5,5)
         call exch(htm,lm,5,5)
         call exch(vtm,lm,5,5)
         call iexch(lmh,1,5,5)
         call iexch(lmv,1,5,5)

!  finally, transfer to output arrays

         do j=jdim1,jdim2
          jj=j-jdim1+1
          do i=idim1,idim2
           ii=i-idim1+1
           lmhout(ii,jj)=lmh(i,j)
           lmvout(ii,jj)=lmv(i,j)
           pdout(ii,jj)=pd(i,j)
           resout(ii,jj)=res(i,j)
           if(pd(i,j).lt.test_value) then
            pdres01(ii,jj)=.01*pd(i,j)*res(i,j)
           else
            pdres01(ii,jj)=special_value
           end if
           if(u10(i,j).lt.test_value.and.v10(i,j).lt.test_value) then
            w10ges(ii,jj)=sqrt(u10(i,j)**2+v10(i,j)**2)
           else
            w10ges(ii,jj)=special_value
           end if
           sfctout(ii,jj)=ths(i,j)
           smout(ii,jj)=sm(i,j)
           siceout(ii,jj)=sice(i,j)
           snoout(ii,jj)=sno(i,j)
           veg_type(ii,jj)=ivgtyp(i,j)
           veg_frac(ii,jj)=vegfrc(i,j)
           soil_type(ii,jj)=isltyp(i,j)
           soil_temp(ii,jj)=stc(i,j,1)
           soil_moi(ii,jj)=smc(i,j,1)
           zges(ii,jj)=fis(i,j)
           u00out(ii,jj)=u00(i,j)
          end do
         end do

         do k=1,lm
          do j=jdim1,jdim2
           jj=j-jdim1+1
           do i=idim1,idim2
            ii=i-idim1+1
            tges(ii,jj,k)=t(i,j,k)
            qges(ii,jj,k)=q(i,j,k)
            q2ges(ii,jj,k)=q2(i,j,k)
            uges(ii,jj,k)=u(i,j,k)
            vges(ii,jj,k)=v(i,j,k)
            cwmout(ii,jj,k)=cwm(i,j,k)
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
       end subroutine transfer_etages
