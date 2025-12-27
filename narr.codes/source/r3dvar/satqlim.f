       subroutine satqlim(q,t,cwm,u00,etamk,imeta,jmeta,pd,ptop,res,tmask, &
                          myis2,myie2,myjs2,myje2,ishifth,mype,cwm_adjust)

!-------- limit q to 0 < q < qsat

  include 'mpif.h'
      include "my_comm.h"

  logical cwm_adjust
  real(4) q(imeta,jmeta),t(imeta,jmeta),cwm(imeta,jmeta),u00(imeta,jmeta)
  real(4) pd(imeta,jmeta),res(imeta,jmeta),tmask(imeta,jmeta)
  integer(4) ishifth(jmeta)

  real(4),allocatable::p(:,:),qsat(:,:)

  allocate(p(imeta,jmeta))
        do j=myjs2,myje2
         do i=myis2,myie2-ishifth(j)
          if(tmask(i,j).lt..5) t(i,j)=273.16
          p(i,j)=etamk*.01*pd(i,j)*res(i,j)+ptop
         end do
        end do
  allocate(qsat(imeta,jmeta))
        do j=myjs2,myje2
         call genqsat(t(myis2,j),qsat(myis2,j),myie2-ishifth(j)-myis2+1,p(myis2,j))
        end do
         ineg=0
         isat=0
        do j=myjs2,myje2
         do i=myis2,myie2-ishifth(j)
          if(tmask(i,j).gt..5) then
           if(q(i,j).lt.0.) then
            ineg=ineg+1
            q(i,j)=0.
           end if
           if(q(i,j).gt.qsat(i,j)) then
            isat=isat+1
            q(i,j)=qsat(i,j)
           end if
          end if
         end do
        end do

  icwm=0
  icwm6=0
  u00max=-huge(u00max)
  u00min=huge(u00max)
  if(cwm_adjust) then

!          adjust cloudwater, by removing it if moisture is below threshold for cloud

   do j=myjs2,myje2
    do i=myis2,myie2-ishifth(j)
     if(tmask(i,j).gt..5) then
      u00max=max(u00(i,j),u00max)
      u00min=min(u00(i,j),u00min)
      if(q(i,j).lt.qsat(i,j)*u00(i,j).and.p(i,j).le.700.) then
       if(cwm(i,j).gt.1.e-6) icwm6=icwm6+1
       icwm=icwm+1
       cwm(i,j)=1.e-20
      end if
     end if
    end do
   end do
  end if
!          print *,' mype,ineg,isat,myis2,ie2,js2,je2,i,jmeta=', &
!                 mype,ineg,isat,myis2,myie2,myjs2,myje2,imeta,jmeta
        call mpi_barrier(my_comm,ierror)
        call mpi_reduce(ineg,inegall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
        call mpi_reduce(isat,isatall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
        if(cwm_adjust) then
         call mpi_reduce(icwm,icwmall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
         call mpi_reduce(icwm6,icwm6all,1,mpi_integer,mpi_sum,0,my_comm,ierror)
         call mpi_reduce(u00max,u00maxall,1,mpi_real,mpi_max,0,my_comm,ierror)
         call mpi_reduce(u00min,u00minall,1,mpi_real,mpi_min,0,my_comm,ierror)
        end if
        if(mype.eq.0.and.cwm_adjust) then
         print *,' for eta lev ',etamk,' num super=',isatall,' num neg=',inegall, &
                                 ' num cwm_adjust=',icwm6all,icwmall
         print *,'          u00max,min=',u00maxall,u00minall
        end if
        if(mype.eq.0.and..not.cwm_adjust) then
         print *,' for eta lev ',etamk,' num super=',isatall,' num neg=',inegall
        end if

  deallocate(qsat)
  deallocate(p)

       return
       end subroutine satqlim
