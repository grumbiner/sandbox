subroutine inetages(lmh,lmv,etai,etam,tges,uges,vges,qges,qsatges,q2ges,cwmges,pdres01, &
                               tges0,uges0,vges0,qges0,q2ges0,cwmges0,pdres010, &
       pbot,ptop,imeta,jmeta,lmeta,lmetaex,hges,w10ges,sfct,sm,sice,sno,zges,tsfcges,res, &
              veg_type,veg_frac,soil_type,soil_temp,soil_moi, &
       tlapseges,myis2,myie2,myjs2,myje2,imetaglb,jmetaglb)

!--------  below ground, to aid in interpolating to below model ground observations

!--------   note that here, imeta,jmeta are not dimensions of full eta grid, but only
!--------    of one subdomain
!-------:    in terms of the variables in PARMETA.comm, we have
!                    imeta = idim2-idim1+1
!                    jmeta = jdim2-jdim1+1

  include 'mpif.h'
      include "my_comm.h"
  integer(4) lmh(imeta,jmeta),lmv(imeta,jmeta)
  real(4) tges(imeta,jmeta,lmetaex)
  real(4) uges(imeta,jmeta,lmetaex),vges(imeta,jmeta,lmetaex),qges(imeta,jmeta,lmetaex)
  real(4) qsatges(imeta,jmeta,lmetaex)
  real(4) q2ges(imeta,jmeta,lmetaex),cwmges(imeta,jmeta,lmetaex)
  real(4) pdres01(imeta,jmeta),etai(lmetaex+1),etam(lmetaex)
  real(4) tges0(imeta,jmeta,lmeta)
  real(4) uges0(imeta,jmeta,lmeta),vges0(imeta,jmeta,lmeta),qges0(imeta,jmeta,lmeta)
  real(4) q2ges0(imeta,jmeta,lmeta),cwmges0(imeta,jmeta,lmeta)
  real(4) pdres010(imeta,jmeta)
  real(4) hges(imeta,jmeta,lmetaex+1)
  real(4) w10ges(imeta,jmeta),sfct(imeta,jmeta),sm(imeta,jmeta)
  real(4) sice(imeta,jmeta),sno(imeta,jmeta),zges(imeta,jmeta),tsfcges(imeta,jmeta),res(imeta,jmeta)
  real(4) veg_type(imeta,jmeta)
  real(4) veg_frac(imeta,jmeta)
  real(4) soil_type(imeta,jmeta)
  real(4) soil_temp(imeta,jmeta)
  real(4) soil_moi(imeta,jmeta)
  real(4) tlapseges(imeta,jmeta)

  real(4),allocatable::zcor(:,:),pd(:,:)
  logical run,first
  dimension idat(3)

  real(4),allocatable::work1(:,:),work2(:,:),work3(:,:)
  integer(4),allocatable::iwork2(:,:)
  integer(4),allocatable::ishifth(:),ishiftv(:)
  real(8) pdrbar8,pdrsig8
  real(4),allocatable::htm(:,:,:)
  real(4),allocatable::vtm(:,:,:),u00(:,:)

  call mpi_comm_rank(my_comm,mype,ierr)
  special_value=1.e20
  test_value=.98*special_value

!   get ishifth,ishiftv, which define which points are dummy points (on east global boundary only)

  allocate(ishifth(jmeta)) ; allocate(ishiftv(jmeta))
  call getshift(ishifth,ishiftv)
  deallocate(ishiftv)

  if(mype.eq.0) print *,' transfer ges restart file for use in getting residuals'

  allocate(pd(imeta,jmeta))
  allocate(htm(imeta,jmeta,lmeta))
  allocate(vtm(imeta,jmeta,lmeta))
  allocate(u00(imeta,jmeta))
            if(mype.eq.0) write(0,*)' before transfer_etages in inetages'
  call transfer_etages(test_value,run,idat,ihrst,ntsd, &
     tges0,qges0,q2ges0,uges0,vges0,cwmges0,lmh,lmv,htm,vtm,pdres010,res,pdres01, &
             w10ges,sfct,sm,sice,sno,veg_type,veg_frac,soil_type,soil_temp,soil_moi,zges, &
        u00,imeta,jmeta,myis2,myie2,myjs2,myje2)
            if(mype.eq.0) write(0,*)' before 2nd transfer_etages in inetages'
  call transfer_etages(test_value,run,idat,ihrst,ntsd, &
        tges,qges,q2ges,uges,vges,cwmges,lmh,lmv,htm,vtm,pd,res,pdres01, &
             w10ges,sfct,sm,sice,sno,veg_type,veg_frac,soil_type,soil_temp,soil_moi,zges, &
        u00,imeta,jmeta,myis2,myie2,myjs2,myje2)
  sfctmax=0.
  do j=1,jmeta
   do i=1,imeta
    if(sfct(i,j).lt.test_value) sfctmax=max(sfct(i,j),sfctmax)
   end do
  end do
         call mpi_allreduce(sfctmax,sfctmaxall,1,mpi_real4,mpi_max,my_comm,ierr)
         if(sfctmaxall.lt.100.) then
             if(mype.eq.0) print *,' SFCT NOT AVAILABLE--replace with T of 1st level above ground'
             if(mype.eq.0) print *,' SFCT NOT AVAILABLE--replace with T of 1st level above ground'
             if(mype.eq.0) print *,' SFCT NOT AVAILABLE--replace with T of 1st level above ground'
             if(mype.eq.0) print *,' SFCT NOT AVAILABLE--replace with T of 1st level above ground'
             if(mype.eq.0) print *,' SFCT NOT AVAILABLE--replace with T of 1st level above ground'
             if(mype.eq.0) print *,' SFCT NOT AVAILABLE--replace with T of 1st level above ground'
         end if
  do j=1,jmeta
   do i=1,imeta
    tsfcges(i,j)=special_value
    if(res(i,j).lt.test_value) then
     tsfcges(i,j)=tges(i,j,lmh(i,j))
    end if
    if(sfctmaxall.lt.100.) sfct(i,j)=tsfcges(i,j)
   end do
  end do
  deallocate(u00)
  deallocate(pd)
  deallocate(htm)
  deallocate(vtm)
        if(mype.eq.0) write(0,*)' at 2 in inetages'
!   print *,' myis2,myie2,myjs2,myje2=',myis2,myie2,myjs2,myje2
    if(mype.eq.0) print *,' guess run,idat,ihrst,ntsd=',run,idat,ihrst,ntsd

  call findbadpd(pdres01,imeta,jmeta,test_value,pdrbar8,pdrsig8,f95,myis2,myie2,myjs2,myje2,ishifth)
        if(mype.eq.0) write(0,*)' at 4 in inetages'
  pbot95loc=ptop
  pbotmaxloc=ptop
  do j=myjs2,myje2
   do i=myis2,myie2-ishifth(j)
    if(pdres01(i,j).lt.test_value) then
     pbotmaxloc=max(ptop+pdres01(i,j),pbotmaxloc)
     if(abs(pdres01(i,j)-pdrbar8).lt.f95*pdrsig8) &
             pbot95loc=max(ptop+pdres01(i,j),pbot95loc)
    end if
   end do
  end do
  deallocate(ishifth)
  call mpi_barrier(my_comm,ierr)
  call mpi_allreduce(pbot95loc,pbot95,1,mpi_real,mpi_max,my_comm,ierr)
  call mpi_allreduce(pbotmaxloc,pbotmax,1,mpi_real,mpi_max,my_comm,ierr)
  if(mype.eq.0) print *,' pbot95,pbotmax=',pbot95,pbotmax
  pbot=pbot95
  if(mype.eq.0) print *,' pbot as computed in inetages is ',pbot

!---  If we are running from the GDAS first guess qges in the top 5 layers
!---  will = 0. Set q to 1.E-7 so code won't blow up.

  do l=1,lmeta
      if(l .le. 5) then
         do jer = 1, jmeta
          do ier = 1, imeta
           if(qges(ier,jer,l).lt.test_value.and.qges(ier,jer,l) .le. 0.0) then
              qges(ier,jer,l) = 1.0e-7
           endif
          enddo
         enddo
      endif

      qgesmaxloc=-huge(qgesmaxloc)
      qgesminloc=huge(qgesminloc)
      do j=1,jmeta
       do i=1,imeta
        if(qges(i,j,l).lt.test_value) then
         qgesmaxloc=max(qges(i,j,l),qgesmaxloc)
         qgesminloc=min(qges(i,j,l),qgesminloc)
        end if
       end do
      end do
      call mpi_barrier(my_comm,ierr)
      call mpi_allreduce(qgesmaxloc,qgesmax,1,mpi_real,mpi_max,my_comm,ierr)
      call mpi_allreduce(qgesminloc,qgesmin,1,mpi_real,mpi_min,my_comm,ierr)
      preseta=etam(l)*(1000.-ptop)+ptop
      if(mype.eq.0) print *,' p,qgesmax,min=',preseta,qgesmax,qgesmin

  end do
        if(mype.eq.0) write(0,*)' at 6 in inetages'

!-------- divide sfc geopotential by g to get sfc elevation in meters

  do j=1,jmeta
   do i=1,imeta
    if(zges(i,j).lt.test_value) zges(i,j)=zges(i,j)/conmc('g$')
   end do
  end do

        if(mype.eq.0) write(0,*)' at 8 in inetages'


!--------   extend tges below ground using std atm lapse rate as bounding value

  do k=lmetaex,2,-1
   do j=1,jmeta
    do i=1,imeta
     if(k.gt.lmh(i,j)) then
      if(pdres01(i,j).gt.test_value) then
       tges(i,j,k)=special_value
      else
       pref=etam(lmh(i,j))*pdres01(i,j)+ptop
       pthis=etam(k)*pdres01(i,j)+ptop
       call w3fa03(pref,dumelev,trefstd,dumtheta)
       call w3fa03(pthis,dumelev,tthisstd,dumtheta)
       gradtstd=(tthisstd-trefstd)/(pthis-pref)
       tges(i,j,k)=tges(i,j,lmh(i,j))+(pthis-pref)*gradtstd
      end if
     end if
    end do
   end do
  end do

!-------------obtain guess saturation vapor pressure

  qsatges=special_value
  do k=1,lmetaex
   do j=1,jmeta
    do i=1,imeta
     if(pdres01(i,j).lt.test_value.and.tges(i,j,k).lt.test_value) then
      pthis=etam(k)*pdres01(i,j)+ptop
      call genqsat(tges(i,j,k),qsatges(i,j,k),1,pthis)
     end if
    end do
   end do
  end do

!------- extend qges below ground
!????????????modify below ground extension of qges to preserve rh

  do k=lmetaex,2,-1
   do j=1,jmeta
    do i=1,imeta
     if(k.gt.lmh(i,j)) then
      if(pdres01(i,j).gt.test_value) then
       qges(i,j,k)=special_value
      else
       relhumref=qges(i,j,lmh(i,j))/qsatges(i,j,lmh(i,j))
       qges(i,j,k)=relhumref*qsatges(i,j,k)
      end if
     end if
    end do
   end do
  end do

!-------- extend uges,vges using lowest available value

  do k=lmetaex,2,-1
   do j=1,jmeta
    do i=1,imeta
     if(k.gt.lmv(i,j)) then
      if(uges(i,j,1).gt.test_value) then
       uges(i,j,k)=special_value
       vges(i,j,k)=special_value
      else
       uges(i,j,k)=uges(i,j,lmv(i,j))
       vges(i,j,k)=vges(i,j,lmv(i,j))
      end if
     end if
    end do
   end do
  end do

!--------- convert ges temp to virtual temp

  do l=1,lmetaex
   do j=1,jmeta
    do i=1,imeta
     if(tges(i,j,l).lt.test_value.and.qges(i,j,l).lt.test_value) &
               tges(i,j,l)=tges(i,j,l)*(1.+.608*qges(i,j,l))
    end do
   end do
  end do

!-------- compute guess heights

  trog=2.*conmc('rd$')/conmc('g$')
  hges=0.
  do k=lmetaex+1,2,-1
   km=k-1
   do j=1,jmeta
    do i=1,imeta
     if(pdres01(i,j).lt.test_value) then
      pk=etai(k)*pdres01(i,j)+ptop
      pkm=etai(km)*pdres01(i,j)+ptop
      hges(i,j,km)=hges(i,j,k)+trog*tges(i,j,km)*(pk-pkm)/(pk+pkm)
     end if
    end do
   end do
  end do
        if(mype.eq.0) write(0,*)' at 10 in inetages'
  allocate(zcor(imeta,jmeta))
  zcor=0.
  do j=1,jmeta
   do i=1,imeta
    if(zges(i,j).lt.test_value) zcor(i,j)=zges(i,j)-hges(i,j,lmh(i,j)+1)
   end do
  end do
  do k=1,lmetaex+1
   do j=1,jmeta
    do i=1,imeta
     if(zges(i,j).lt.test_value) then
      hges(i,j,k)=hges(i,j,k)+zcor(i,j)
     else
      hges(i,j,k)=special_value
     end if
    end do
   end do
  end do
  deallocate(zcor)

        if(mype.eq.0) write(0,*)' at 12 in inetages'

!    compute ave lapse rate over 1000 meters

  do j=1,jmeta
   do i=1,imeta
    tlapseges(i,j)=special_value
    if(res(i,j).lt.test_value) then
     kbm=lmh(i,j)+1
     kbp=lmh(i,j)
     hbp=hges(i,j,kbp)
     kap=1
     do k=kbp,1,-1
      if(hges(i,j,k)-hbp.gt.1000.) then
       kap=k
       exit
      end if
     end do
     kam=k+1
     tlapseges(i,j)=2.*(tges(i,j,kap)-tges(i,j,kbp))/(hges(i,j,kap)+hges(i,j,kam) &
                                                     -hges(i,j,kbp)-hges(i,j,kbm))
    end if
   end do
  end do

!   for diagnostic purposes, make grads file of guess

   if(mype.eq.0) print *,' before call to outgrad3 in inetages, lmeta,lmetaex=', &
                 lmeta,lmetaex

!call outgrad3(pdres01,tges,qges,uges,vges,zges,hges,imeta,jmeta,lmetaex, &
!              imetaglb,jmetaglb,etam,ptop,mype)

return
end subroutine inetages

subroutine findbadpd(pdres01,im,jm,test_value,pdrbarall8,pdrsigall8, &
                     f95all,myis2,myie2,myjs2,myje2,ishifth)

  include 'mpif.h'
      include "my_comm.h"

  real(4) pdres01(im,jm)
  real(8) pdrbarall8,pdrsigall8
  integer(4) ishifth(jm)

  real(4) bin(40),binall(40)
  real(8) pdrbar8,pdrsig8,sum8,sumall8
  integer(8) isum_8,isumall_8,ipdrbar_8,ipdrsig_8,iproduct_8
  integer(8) ipdrbarall_8,ipdrsigall_8
  real(8) scale_8,pdmax_8,pdmaxall_8,product_8,fraction_8

  call mpi_comm_rank(my_comm,mype,ierr)

  if(mype.eq.0) print *,' in findbadpd, test_value=',test_value
  pdmax_8=0._8
  isum_8=0_8
  do j=myjs2,myje2
   do i=myis2,myie2-ishifth(j)
    if(pdres01(i,j).lt.test_value) then
     pdmax_8=max(abs(1._8*pdres01(i,j)),pdmax_8)
     isum_8=isum_8+1_8
    end if
   end do
  end do
  call mpi_allreduce(pdmax_8,pdmaxall_8,1,mpi_real8,mpi_max,my_comm,ierr)
  call mpi_allreduce(isum_8,isumall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
  sumall8=isumall_8
  if(pdmaxall_8.eq.0._8) pdmaxall_8=1._8
  scale_8=2._8**(digits(r4)+8)/pdmaxall_8
  ipdrbar_8=0_8
  do j=myjs2,myje2
   do i=myis2,myie2-ishifth(j)
    if(pdres01(i,j).lt.test_value) then
     product_8=pdres01(i,j)*scale_8
     iproduct_8=product_8
     fraction_8=product_8-iproduct_8
     if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
     ipdrbar_8=ipdrbar_8+iproduct_8
    end if
   end do
  end do
  call mpi_allreduce(ipdrbar_8,ipdrbarall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
  pdrbarall8=ipdrbarall_8/(sumall8*scale_8)
  pdmax_8=0._8
  do j=myjs2,myje2
   do i=myis2,myie2-ishifth(j)
    if(pdres01(i,j).lt.test_value) pdmax_8=max((pdres01(i,j)-pdrbarall8)**2,pdmax_8)
   end do
  end do
  call mpi_allreduce(pdmax_8,pdmaxall_8,1,mpi_real8,mpi_max,my_comm,ierr)
  if(pdmaxall_8.eq.0._8) pdmaxall_8=1._8
  scale_8=2._8**(digits(r4)+8)/pdmaxall_8
  ipdrsig_8=0_8
  do j=myjs2,myje2
   do i=myis2,myie2-ishifth(j)
    if(pdres01(i,j).lt.test_value) then
     product_8=scale_8*(pdres01(i,j)-pdrbarall8)**2
     iproduct_8=product_8
     fraction_8=product_8-iproduct_8
     if(fraction_8.gt..5_8) iproduct_8=iproduct_8+1_8
     ipdrsig_8=ipdrsig_8+iproduct_8
    end if
   end do
  end do
  call mpi_allreduce(ipdrsig_8,ipdrsigall_8,1,mpi_integer8,mpi_sum,my_comm,ierr)
  pdrsigall8=sqrt(ipdrsigall_8/(sumall8*scale_8))
  if(mype.eq.0) then
   print *,' for pdres01, pdrbar,pdrsig=',pdrbarall8,pdrsigall8
   print *,' number of points=',sumall8
  end if
  bin=0.
  do ifact=1,40
   factor=ifact
   do j=myjs2,myje2
    do i=myis2,myie2-ishifth(j)
     if(pdres01(i,j).lt.test_value.and. &
        abs(pdres01(i,j)-pdrbarall8).gt.factor*pdrsigall8) bin(ifact)=bin(ifact)+1.
    end do
   end do
  end do
  call mpi_barrier(my_comm,ierr)
  call mpi_allreduce(bin,binall,40,mpi_real,mpi_sum,my_comm,ierr)
  if(mype.eq.0) print *,' for pdres01, bin=',binall
  f95all=5.
  do i=1,40
   if(binall(i).lt..05*sumall8) then
    f95all=i
    go to 100
   end if
  end do
  100 continue
  if(mype.eq.0) print *,' f95=',f95all

return
end subroutine findbadpd
