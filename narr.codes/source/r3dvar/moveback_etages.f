       subroutine moveback_etages(runin,idatin,ihrstin,ntsdin, &
            tges,qges,q2ges,uges,vges,cwmin,pdin,imeta,jmeta,ishifth,ishiftv)

! move eta fields back to model common blocks, in preparation for output,
!       using eta model subroutine CHKOUT

         include "PARMETA.comm"
         include "mpif.h"
      include "my_comm.h"
         include "mpp.h"

         LOGICAL runin,RUN,FIRST,RESTRT,SIGMA

         include "LOOPS.comm"
         include "MASKS.comm"
         include "PVRBLS.comm"
         include "VRBLS.comm"
         include "CTLBLK.comm"
         include "CLDWTR.comm"

         real(4) tges(imeta,jmeta,*),qges(imeta,jmeta,*)
         real(4) q2ges(imeta,jmeta,*)
         real(4) uges(imeta,jmeta,*),vges(imeta,jmeta,*)
         real(4) cwmin(imeta,jmeta,*)
         real(4) pdin(imeta,jmeta)
         integer(4) idatin(3),ishifth(jmeta),ishiftv(jmeta)
         real(4),allocatable::work(:,:)
         integer(8) isum_8,isumall_8,icount_8,icountall_8,iproduct_8
         real(8) scale_8,product_8,fraction_8

         if(idim2-idim1+1.ne.imeta.or.jdim2-jdim1+1.ne.jmeta) then
          print *,' incorrect local dimensions in moveback_etages'
          stop
         end if

         run=runin
         idat=idatin
         ihrst=ihrstin
         ntsd=ntsdin

         call exch(pd,1,5,5)
         call exch(t,lm,5,5)
         call exch(q,lm,5,5)
         call exch(u,lm,5,5)
         call exch(v,lm,5,5)
         call exch(cwm,lm,5,5)

!  first, move fields from input arrays to common

         allocate(work(imeta,jmeta))
         pdincmax=0.
         do j=my_js_loc,my_je_loc
          jj=j-jdim1+1
          do i=my_is_loc,my_ie_loc-ishifth(jj)
           ii=i-idim1+1
           work(ii,jj)=abs(pdin(ii,jj)-pd(i,j))
           pdincmax=max(pdincmax,work(ii,jj))
           pd(i,j)=pdin(ii,jj)
          end do

!--------------------fill phantom points
          if(ishifth(jj).eq.1) pd(my_ie_loc,j)=pd(my_ie_loc-1,j)
         end do
             call mpi_allreduce(pdincmax,pdincmaxall,1,mpi_real,mpi_max,my_comm,ierr)
             if(pdincmaxall.eq.0.) pdincmaxall=1.
             scale_8=2._8**(digits(r4)+8)/pdincmaxall**2
             isum_8=0_8
             icount_8=0_8
             do j=my_js_loc,my_je_loc
              jj=j-jdim1+1
              do i=my_is_loc,my_ie_loc-ishifth(jj)
               ii=i-idim1+1
               product_8=scale_8*work(ii,jj)**2
               iproduct_8=product_8
               fraction_8=product_8-iproduct_8
               if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
               isum_8=isum_8+iproduct_8
               icount_8=icount_8+1_8
              end do
             end do
             call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             call mpi_allreduce(icount_8,icountall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             pdincrmsall=sqrt(isumall_8/(scale_8*icountall_8))
             if(mype.eq.0) print *,' pdr3dv-pdges max,rms=',pdincmaxall,pdincrmsall

         do k=1,lm

          tincmax=0.
          do j=my_js_loc,my_je_loc
           jj=j-jdim1+1
           do i=my_is_loc,my_ie_loc-ishifth(jj)
            ii=i-idim1+1
            if(htm(i,j,k).gt..5) then
             work(ii,jj)=abs(tges(ii,jj,k)-t(i,j,k))
             tincmax=max(tincmax,work(ii,jj))
             t(i,j,k)=tges(ii,jj,k)
            end if
           end do
!---------------fill phantom t points
           if(ishifth(jj).eq.1) then
            t(my_ie_loc,j,k)=t(my_ie_loc-1,j,k)
           end if
          end do
             call mpi_allreduce(tincmax,tincmaxall,1,mpi_real,mpi_max,my_comm,ierr)
             if(tincmaxall.eq.0.) tincmaxall=1.
             scale_8=2._8**(digits(r4)+8)/tincmaxall**2
             isum_8=0_8
             icount_8=0_8
             do j=my_js_loc,my_je_loc
              jj=j-jdim1+1
              do i=my_is_loc,my_ie_loc-ishifth(jj)
               if(htm(i,j,k).gt..5) then
                ii=i-idim1+1
                product_8=scale_8*work(ii,jj)**2
                iproduct_8=product_8
                fraction_8=product_8-iproduct_8
                if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
                isum_8=isum_8+iproduct_8
                icount_8=icount_8+1_8
               end if
              end do
             end do
             call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             call mpi_allreduce(icount_8,icountall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             tincrmsall=sqrt(isumall_8/(scale_8*icountall_8))
             if(mype.eq.0) print *,' tges-tr3dv max,rms=',tincmaxall,tincrmsall

          qincmax=0.
          do j=my_js_loc,my_je_loc
           jj=j-jdim1+1
           do i=my_is_loc,my_ie_loc-ishifth(jj)
            ii=i-idim1+1
            if(htm(i,j,k).gt..5) then
             work(ii,jj)=abs(qges(ii,jj,k)-q(i,j,k))
             qincmax=max(qincmax,work(ii,jj))
             q(i,j,k)=qges(ii,jj,k)
            end if
           end do
!---------------fill phantom q points
           if(ishifth(jj).eq.1) then
            q(my_ie_loc,j,k)=q(my_ie_loc-1,j,k)
           end if
          end do
             call mpi_allreduce(qincmax,qincmaxall,1,mpi_real,mpi_max,my_comm,ierr)
             if(qincmaxall.eq.0.) qincmaxall=1.
             scale_8=2._8**(digits(r4)+8)/qincmaxall**2
             isum_8=0_8
             icount_8=0_8
             do j=my_js_loc,my_je_loc
              jj=j-jdim1+1
              do i=my_is_loc,my_ie_loc-ishifth(jj)
               if(htm(i,j,k).gt..5) then
                ii=i-idim1+1
                product_8=scale_8*work(ii,jj)**2
                iproduct_8=product_8
                fraction_8=product_8-iproduct_8
                if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
                isum_8=isum_8+iproduct_8
                icount_8=icount_8+1_8
               end if
              end do
             end do
             call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             call mpi_allreduce(icount_8,icountall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             qincrmsall=sqrt(isumall_8/(scale_8*icountall_8))
             if(mype.eq.0) print *,' qges-qr3dv max,rms=',qincmaxall,qincrmsall

          q2incmax=0.
          do j=my_js_loc,my_je_loc
           jj=j-jdim1+1
           do i=my_is_loc,my_ie_loc-ishifth(jj)
            ii=i-idim1+1
            if(htm(i,j,k).gt..5) then
             work(ii,jj)=abs(q2ges(ii,jj,k)-q2(i,j,k))
             q2incmax=max(q2incmax,work(ii,jj))
             q2(i,j,k)=q2ges(ii,jj,k)
            end if
           end do
!---------------fill phantom q2 points
           if(ishifth(jj).eq.1) then
            q2(my_ie_loc,j,k)=q2(my_ie_loc-1,j,k)
           end if
          end do
             call mpi_allreduce(q2incmax,q2incmaxall,1,mpi_real,mpi_max,my_comm,ierr)
             if(q2incmaxall.eq.0.) q2incmaxall=1.
             scale_8=2._8**(digits(r4)+8)/q2incmaxall**2
             isum_8=0_8
             icount_8=0_8
             do j=my_js_loc,my_je_loc
              jj=j-jdim1+1
              do i=my_is_loc,my_ie_loc-ishifth(jj)
               if(htm(i,j,k).gt..5) then
                ii=i-idim1+1
                product_8=scale_8*work(ii,jj)**2
                iproduct_8=product_8
                fraction_8=product_8-iproduct_8
                if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
                isum_8=isum_8+iproduct_8
                icount_8=icount_8+1_8
               end if
              end do
             end do
             call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             call mpi_allreduce(icount_8,icountall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             q2incrmsall=sqrt(isumall_8/(scale_8*icountall_8))
             if(mype.eq.0) print *,' q2ges-q2r3dv max,rms=',q2incmaxall,q2incrmsall

          uincmax=0.
          do j=my_js_loc,my_je_loc
           jj=j-jdim1+1
           do i=my_is_loc,my_ie_loc-ishiftv(jj)
            ii=i-idim1+1
            if(vtm(i,j,k).gt..5) then
             work(ii,jj)=abs(uges(ii,jj,k)-u(i,j,k))
             uincmax=max(uincmax,work(ii,jj))
             u(i,j,k)=uges(ii,jj,k)
            end if
           end do
!---------------fill phantom u points
           if(ishiftv(jj).eq.1) then
            u(my_ie_loc,j,k)=u(my_ie_loc-1,j,k)
           end if
          end do
             call mpi_allreduce(uincmax,uincmaxall,1,mpi_real,mpi_max,my_comm,ierr)
             if(uincmaxall.eq.0.) uincmaxall=1.
             scale_8=2._8**(digits(r4)+8)/uincmaxall**2
             isum_8=0_8
             icount_8=0_8
             do j=my_js_loc,my_je_loc
              jj=j-jdim1+1
              do i=my_is_loc,my_ie_loc-ishifth(jj)
               if(vtm(i,j,k).gt..5) then
                ii=i-idim1+1
                product_8=scale_8*work(ii,jj)**2
                iproduct_8=product_8
                fraction_8=product_8-iproduct_8
                if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
                isum_8=isum_8+iproduct_8
                icount_8=icount_8+1_8
               end if
              end do
             end do
             call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8, mpi_sum,my_comm,ierr)
             call mpi_allreduce(icount_8,icountall_8,1,mpi_integer8, mpi_sum,my_comm,ierr)
             uincrmsall=sqrt(isumall_8/(scale_8*icountall_8))
             if(mype.eq.0) print *,' uges-ur3dv max,rms=',uincmaxall,uincrmsall

          vincmax=0.
          do j=my_js_loc,my_je_loc
           jj=j-jdim1+1
           do i=my_is_loc,my_ie_loc-ishiftv(jj)
            ii=i-idim1+1
            if(vtm(i,j,k).gt..5) then
             work(ii,jj)=abs(vges(ii,jj,k)-v(i,j,k))
             vincmax=max(vincmax,work(ii,jj))
             v(i,j,k)=vges(ii,jj,k)
            end if
           end do
!---------------fill phantom v points
           if(ishiftv(jj).eq.1) then
            v(my_ie_loc,j,k)=v(my_ie_loc-1,j,k)
           end if
          end do
             call mpi_allreduce(vincmax,vincmaxall,1,mpi_real,mpi_max,my_comm,ierr)
             if(vincmaxall.eq.0.) vincmaxall=1.
             scale_8=2._8**(digits(r4)+8)/vincmaxall**2
             isum_8=0_8
             icount_8=0_8
             do j=my_js_loc,my_je_loc
              jj=j-jdim1+1
              do i=my_is_loc,my_ie_loc-ishifth(jj)
               if(vtm(i,j,k).gt..5) then
                ii=i-idim1+1
                product_8=scale_8*work(ii,jj)**2
                iproduct_8=product_8
                fraction_8=product_8-iproduct_8
                if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
                isum_8=isum_8+iproduct_8
                icount_8=icount_8+1_8
               end if
              end do
             end do
             call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             call mpi_allreduce(icount_8,icountall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             vincrmsall=sqrt(isumall_8/(scale_8*icountall_8))
             if(mype.eq.0) print *,' vges-vr3dv max,rms=',vincmaxall,vincrmsall

          cwmincmax=0.
          do j=my_js_loc,my_je_loc
           jj=j-jdim1+1
           do i=my_is_loc,my_ie_loc-ishifth(jj)
            ii=i-idim1+1
            work(ii,jj)=abs(cwmin(ii,jj,k)-cwm(i,j,k))
            cwmincmax=max(cwmincmax,work(ii,jj))
            cwm(i,j,k)=cwmin(ii,jj,k)
           end do
!---------------fill phantom q points
           if(ishifth(jj).eq.1) then
            cwm(my_ie_loc,j,k)=cwm(my_ie_loc-1,j,k)
           end if
          end do
             call mpi_allreduce(cwmincmax,cwmincmaxall,1,mpi_real,mpi_max,my_comm,ierr)
             if(cwmincmaxall.eq.0.) cwmincmaxall=1.
             scale_8=2._8**(digits(r4)+8)/cwmincmaxall**2
             isum_8=0_8
             do j=my_js_loc,my_je_loc
              jj=j-jdim1+1
              do i=my_is_loc,my_ie_loc-ishifth(jj)
               ii=i-idim1+1
               product_8=scale_8*work(ii,jj)**2
               iproduct_8=product_8
               fraction_8=product_8-iproduct_8
               if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
               isum_8=isum_8+iproduct_8
              end do
             end do
             call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
             cwmincrmsall=sqrt(isumall_8/(scale_8*icountall_8))
             if(mype.eq.0) print *,' cwmges-cwmr3dv max,rms=',cwmincmaxall,cwmincrmsall

         end do

!  now, do halo exchange on variables

         call exch(pd,1,5,5)
         call exch(t,lm,5,5)
         call exch(q,lm,5,5)
         call exch(u,lm,5,5)
         call exch(v,lm,5,5)
         call exch(cwm,lm,5,5)


       return
       end subroutine moveback_etages
