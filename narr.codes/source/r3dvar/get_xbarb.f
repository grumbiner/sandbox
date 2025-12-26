subroutine get_xbarb(imeta,jmeta,imetaglb,jmetaglb,lmeta,lmh,etaiex,etamex, &
      tges,qges,qsatges,q2ges,cwmges,uges,vges,pdr01ges, &
      tges0,qges0,q2ges0,cwmges0,uges0,vges0,pdr01ges0, &
      myis2,myie2,myjs2,myje2, &
      dlon0,dlat0,wbglb,sbglb,istagh,istagv,ptop,pbot,lmetaex,erlat0,erlon0, &
      iayear,iamonth,iaday,iahour,delhour,grosst,grossw,grossp,grosspw,grossq, &
      ingesglob,jcapglob,nsigglob,nlonglob,nlathglob,userad, &
      iordges,lbig2ges,lbig3ges,lhalfges, &
      rad_dat,nrad_dat,mrad_dat,npred,nsat,dlon8,dlat8,wb8,sb8,saterr_inflate,saterr_runfact, &
      iuniterr,mpwdata,idiag_rad,iout_rad,sattype,isatid,nelesat,jppf,jpch,jpchus, &
      nangs_max,nbias_loop,nbias_loop1,mbias_loop,model_id, &
      tbias,tcbias,dt_assimilation,cbias_step_clen,allbias_old,allbias_new,npes)

!-------- transfer model guess from restart common blocks, then
!-------- interpolate to observation locations.

  include 'mpif.h'
      include "my_comm.h"
  include 'types.h'
  include 'types0.h'
  include 'satbias_type.h'

  type(satbias_stuff) allbias_old(jpch),allbias_new(jpch)
  integer(8) model_id
  integer(4) lmh(imeta*jmeta)
  real(4) tges(imeta*jmeta,lmetaex)
  real(4) qges(imeta*jmeta,lmetaex),q2ges(imeta*jmeta,lmetaex)
  real(4) qsatges(imeta*jmeta,lmetaex)
  real(4) cwmges(imeta*jmeta,lmetaex)
  real(4) uges(imeta*jmeta,lmetaex),vges(imeta*jmeta,lmetaex)
  real(4) pdr01ges(imeta*jmeta)
  real(4) tges0(imeta*jmeta,lmeta)
  real(4) qges0(imeta*jmeta,lmeta),q2ges0(imeta*jmeta,lmeta)
  real(4) cwmges0(imeta*jmeta,lmeta)
  real(4) uges0(imeta*jmeta,lmeta),vges0(imeta*jmeta,lmeta)
  real(4) pdr01ges0(imeta*jmeta)
  logical userad,idiag_rad
  real(4) etaiex(lmetaex+1),etamex(lmetaex)
  type(rad_obs) rad_dat(max(1,nrad_dat))
  real(8) dlon8,dlat8,wb8,sb8

  type(rad_obs0),allocatable::rad_dat0(:)
  integer(4),allocatable::lmv(:)
  real(4),allocatable::res(:)
  real(4),allocatable::w10ges(:),sfct(:),sm(:),sice(:),sno(:),zsfcges(:),tsfcges(:)
  real(4),allocatable::tlapseges(:)
  real(4),allocatable::hges(:,:)
  real(4),allocatable::tgese(:,:),qgese(:,:),ozgese(:,:),pgese(:,:),pgesei(:,:)
  real(4),allocatable::veg_type(:),veg_frac(:),soil_type(:),soil_temp(:),soil_moi(:)
!                real(4),allocatable::sfcglb(:)
  integer(4),allocatable::kpgesbot(:)
  real(4),allocatable::varst(:),varprd(:)
  integer(4) idate5(5)
  character(12) diag_rad
  character(10) sattype(50)
  integer(4) isatid(50),nelesat(50)
  integer(4),dimension(jpch):: nuchan,nusat
  integer(4),dimension(0:jpch):: iuse
  real(4),dimension(jpch):: varch0

  call mpi_comm_rank(my_comm,mype,ierr) 


!-------- bring in guess, putting it on unstaggered grid for now
!--------      (we throw away every other row)
!--------  (note that uges,vges are shifted on analysis grid,
!--------     1/2 x grid increment in the positive x direction)

!-------- note: pbot is changed by inetages, based on highest guess
!--------        surface pressure.   this is used by getgrid to get
!--------        vertical analysis coordinate.

  allocate(lmv(imeta*jmeta))
  allocate(w10ges(imeta*jmeta))
  allocate(sfct(imeta*jmeta))
  allocate(sm(imeta*jmeta))
  allocate(sice(imeta*jmeta))
  allocate(sno(imeta*jmeta))
  allocate(zsfcges(imeta*jmeta))
  allocate(tsfcges(imeta*jmeta))
  allocate(tlapseges(imeta*jmeta))
  allocate(res(imeta*jmeta))
  allocate(hges(imeta*jmeta,lmetaex+1))
  allocate(veg_type(imeta*jmeta))
  allocate(veg_frac(imeta*jmeta))
  allocate(soil_type(imeta*jmeta))
  allocate(soil_temp(imeta*jmeta))
  allocate(soil_moi(imeta*jmeta))
     if(mype.eq.0) write(0,*)' get_xbarb--before inetages'
  call inetages(lmh,lmv,etaiex,etamex,tges,uges,vges,qges,qsatges,q2ges,cwmges,pdr01ges, &
                          tges0,uges0,vges0,qges0,q2ges0,cwmges0,pdr01ges0, &
       pbot,ptop,imeta,jmeta,lmeta,lmetaex,hges,w10ges,sfct,sm,sice,sno,zsfcges,tsfcges,res, &
              veg_type,veg_frac,soil_type,soil_temp,soil_moi, &
       tlapseges,myis2,myie2,myjs2,myje2,imetaglb,jmetaglb)
!          allocate(sfcglb(imetaglb*jmetaglb))
!          call loc2glb(sno,sfcglb)
!          if(mype.eq.0) then
!           call outgradsfcflds(sfcglb,imetaglb,jmetaglb,'snoxx')
!          end if
!          deallocate(sfcglb)
!                        if(mype.gt.-1000) then
!                           call mpi_finalize(ierr)
!                           stop
!                        end if

!          obtain global fields, interpolated in horizontal
!          and vertical to eta analysis domain.
!          merge with eta background
!             (we want temperature, moisture, ozone.  extend in
!               vertical above top of eta model, using global sigma
!               layer values)

  pradmin=ptop         !   don*t use pradmin anymore
  np=lmetaex+10
  if(jcapglob.le.0) np=lmetaex
  if(userad) then
   allocate(varst(imeta*jmeta))
   allocate(varprd(jpch*npred))
   allocate(tgese(imeta*jmeta,np))
   allocate(qgese(imeta*jmeta,np))
   allocate(ozgese(imeta*jmeta,np))
   allocate(pgese(imeta*jmeta,np))
   allocate(pgesei(imeta*jmeta,np+1))
   allocate(kpgesbot(imeta*jmeta))
   call get_global_ges(mype,ingesglob,jcapglob,nsigglob,np, &
               nlonglob,nlathglob,erlon0,erlat0, &
               imeta,jmeta,dlon8,dlat8,wb8,sb8,istagh, &
               tges,qges,pdr01ges,etaiex,etamex,ptop,lmetaex,lmh, &
               tgese,qgese,ozgese,pgese,pgesei,kpgesbot,npes)

!                 print out a few sample profiles to see if it looks ok

!             lmhmin=minval(lmh)
!             call mpi_allreduce(lmhmin,lmhminall,1,mpi_integer,mpi_min,my_comm,ierr)
!             mypemin=0
!             if(lmhmin.eq.lmhminall) mypemin=mype
!             call mpi_allreduce(mypemin,mypemax,1,mpi_integer,mpi_max,my_comm,ierr)
!               if(mype.eq.0) print *,' lmhminall,mypemax=',lmhminall,mypemax
!             if(mype.eq.mypemax) then
!              do i=1,imeta*jmeta
!               if(lmh(i).eq.lmhminall) then
!                print *,' t        q        oz      p    pi '
!                do k=1,np
!                 write(6,1191)tgese(i,k),qgese(i,k),ozgese(i,k),pgese(i,k),pgesei(i,k)
!                end do
!                k=np+1
!                write(6,1192)pgesei(i,k)
!1191             format(' ',5e15.4)
!1192             format(' ',60x,e15.4)
!                  if(mype.gt.-10000) go to 1193
!               end if
!              end do
!1193         continue
!             end if

!  process radiance data

    idate5(1)=iayear
    idate5(2)=iamonth
    idate5(3)=iaday
    idate5(4)=iahour
    idate5(5)=0
    call w3fs21(idate5,nming)
    gsstm=float(nming)
    if(mype.eq.0) print *,' get_xbarb--before setuprad,iayear,month,day,hour,gsstm=', &
             iayear,iamonth,iaday,iahour,gsstm

   if(idiag_rad) then
    write(diag_rad,1004) mype+1000
    open(7,file=diag_rad,form='unformatted')
    rewind 7
    close(7)
   endif
1004 format('diag_rad',i4)

!      first do initial processing of radiance data--do all computations for guess bt.

    allocate(rad_dat0(max(1,nrad_dat)))
    do i=1,max(1,nrad_dat)
     allocate(rad_dat0(i)%pressure(np))
     allocate(rad_dat0(i)%iwgts(lbig2ges))
     allocate(rad_dat0(i)%wgts(lbig2ges))
     allocate(rad_dat0(i)%icx(jpchus))
     allocate(rad_dat0(i)%var(jpchus))
     allocate(rad_dat0(i)%pred(npred+jpchus-1))
     allocate(rad_dat0(i)%obsbt(jpchus))
     allocate(rad_dat0(i)%gesbt(jpchus))
     allocate(rad_dat0(i)%emissav(jpchus))
     allocate(rad_dat0(i)%tlapchn(jpchus))
     allocate(rad_dat0(i)%pems(jpchus))
     allocate(rad_dat0(i)%htlto(3*np+1,jpchus))
    end do
    call setuprad_1(rad_dat0,nrad_dat,mrad_dat,tgese,qgese,ozgese,pgese,pgesei,kpgesbot,w10ges, &
      sfct,sm,sice,sno,zsfcges,veg_type,veg_frac,soil_type,soil_temp,soil_moi,imeta,jmeta,np, &
      npred,mype,npes,nsat,sattype,isatid, &
      iordges,lbig2ges,lhalfges,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb, &
      myis2,myie2,myjs2,myje2,jppf,jpch,jpchus, &
              nusat,nuchan,iuse,varch0)
          write(0,*)' after setuprad_1,mype,mrad_dat,nrad_dat=',mype,mrad_dat,nrad_dat
    call setuprad_2(np,lbig2ges,imeta,jmeta,rad_dat0,rad_dat,nrad_dat,mrad_dat, &
              varst,varprd,npred,mype,npes,nsat,diag_rad,idiag_rad,iout_rad,sattype, &
              saterr_inflate,saterr_runfact,jpch,jpchus, &
              idate5,nangs_max,nbias_loop,nbias_loop1,mbias_loop, &
              model_id,tbias,tcbias,dt_assimilation,cbias_step_clen,allbias_old,allbias_new, &
              nusat,nuchan,iuse,varch0)
          write(0,*)' after setuprad_2,mype,mrad_dat,nrad_dat=',mype,mrad_dat,nrad_dat
    do i=1,max(1,nrad_dat)
     deallocate(rad_dat0(i)%pressure)
     deallocate(rad_dat0(i)%iwgts)
     deallocate(rad_dat0(i)%wgts)
     deallocate(rad_dat0(i)%icx)
     deallocate(rad_dat0(i)%var)
     deallocate(rad_dat0(i)%pred)
     deallocate(rad_dat0(i)%obsbt)
     deallocate(rad_dat0(i)%gesbt)
     deallocate(rad_dat0(i)%emissav)
     deallocate(rad_dat0(i)%tlapchn)
     deallocate(rad_dat0(i)%pems)
     deallocate(rad_dat0(i)%htlto)
    end do
    deallocate(rad_dat0)

               call mpi_reduce(mrad_dat,mrad_datall,1,mpi_integer,mpi_sum,0,my_comm,ierr)
               call mpi_reduce(nrad_dat,nrad_datall,1,mpi_integer,mpi_sum,0,my_comm,ierr)
                if(mype.eq.0) print *,' mrad_datall,nrad_datall=',mrad_datall,nrad_datall
                 
   deallocate(tgese)
   deallocate(qgese)
   deallocate(ozgese)
   deallocate(pgese)
   deallocate(pgesei)
   deallocate(varst)
   deallocate(varprd)
   deallocate(kpgesbot)
  end if
  deallocate(w10ges)
  deallocate(sfct)
  deallocate(sm)
  deallocate(sice)
  deallocate(sno)
  deallocate(veg_type)
  deallocate(veg_frac)
  deallocate(soil_type)
  deallocate(soil_temp)
  deallocate(soil_moi)

!-------- now do sorting of observations and computation of residuals

        if(mype.eq.0) write(0,*)' at 1 in get_xbarb'
  call sortps(iordges,lbig2ges,lhalfges, &
          pdr01ges,zsfcges,tsfcges,res,tlapseges,imeta,jmeta, &
          ptop,delhour,grossp,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)
  deallocate(zsfcges)
  deallocate(tsfcges)
  deallocate(res)

       if(mype.eq.0) write(0,*)' get_xbarb--before sorttemps'
  call sorttemps(erlon0,erlat0,iordges,lbig2ges,lbig3ges,lhalfges, &
          pdr01ges,tges,qges,hges,imeta,jmeta,lmetaex,lmh, &
         etamex,etaiex,ptop,delhour,grosst,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)

       if(mype.eq.0) write(0,*)' get_xbarb--before sortwinds'
  call sortwinds(erlon0,erlat0,iordges,lbig2ges,lbig3ges,lhalfges, &
          pdr01ges,uges,vges,hges,imeta,jmeta,lmetaex, &
         etamex,etaiex,ptop,lmh,delhour,grossw,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)
       if(mype.eq.0) write(0,*)' get_xbarb--before sort_radar_winds'
  call sort_radar_winds(erlon0,erlat0,iordges,lbig2ges,lbig3ges,lhalfges, &
          uges,vges,hges,imeta,jmeta,lmetaex, &
         lmh,lmv,delhour,grossw,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)
  deallocate(lmv)

          if(mype.eq.0) write(0,*)' get_xbarb--before sortqs'
  call sortqs(iordges,lbig2ges,lbig3ges,lhalfges, &
          pdr01ges,tges,qges,hges,imeta,jmeta,lmetaex,lmh, &
          etamex,etaiex,ptop,delhour,grossq,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)

          if(mype.eq.0) write(0,*)' get_xbarb--before sortpws'
  call sortpws(mpwdata,iordges,lbig2ges,lhalfges, &
         pdr01ges,qges,imeta,jmeta,lmetaex,lmh,etaiex,ptop,delhour,grosspw, &
         wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)


return
end subroutine get_xbarb
