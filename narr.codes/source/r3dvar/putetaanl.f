subroutine putetaanl(tetaanl,qetaanl,q2etaanl,cwmetaanl,uetaanl,vetaanl,pdetaanl,imeta,jmeta,lmeta)

! reset current contents of eta model common block variables to input arrays

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

         real(4) tetaanl(imeta,jmeta,lmeta),qetaanl(imeta,jmeta,lmeta)
         real(4) q2etaanl(imeta,jmeta,lmeta),cwmetaanl(imeta,jmeta,lmeta)
         real(4) uetaanl(imeta,jmeta,lmeta),vetaanl(imeta,jmeta,lmeta)
         real(4) pdetaanl(imeta,jmeta)

         if(idim2-idim1+1.ne.imeta.or.jdim2-jdim1+1.ne.jmeta) then
          print *,' incorrect local dimensions in putetaanl'
          stop
         end if

!  first, fill points outside subdomain with special values

         special_value=1.e20
         test_value=.98*special_value
         do j=jdim1,jdim2
          do i=idim1,myis-1
           pd(i,j)=special_value
          end do
          do i=myie+1,idim2
           pd(i,j)=special_value
          end do
         end do
         do j=jdim1,myjs-1
          do i=idim1,idim2
           pd(i,j)=special_value
          end do
         end do
         do j=myje+1,jdim2
          do i=idim1,idim2
           pd(i,j)=special_value
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

!  next, transfer input arrays to common variables

         do j=jdim1,jdim2
          jj=j-jdim1+1
          do i=idim1,idim2
           ii=i-idim1+1
           pd(i,j)=pdetaanl(ii,jj)/(.01*res(i,j))
          end do
         end do

         do k=1,lm
          do j=jdim1,jdim2
           jj=j-jdim1+1
           do i=idim1,idim2
            ii=i-idim1+1
            t(i,j,k)=tetaanl(ii,jj,k)/(1.+.608*qetaanl(ii,jj,k))  ! convert from Tv to T 
            q(i,j,k)=qetaanl(ii,jj,k)
            q2(i,j,k)=q2etaanl(ii,jj,k)
            cwm(i,j,k)=cwmetaanl(ii,jj,k)
            u(i,j,k)=uetaanl(ii,jj,k)
            v(i,j,k)=vetaanl(ii,jj,k)
           end do
          end do
         end do

!  now, do halo exchange on variables

         call exch(pd,1,5,5)
         call exch(t,lm,5,5)
         call exch(q,lm,5,5)
         call exch(q2,lm,5,5)
         call exch(cwm,lm,5,5)
         call exch(u,lm,5,5)
         call exch(v,lm,5,5)


       return
       end subroutine putetaanl
