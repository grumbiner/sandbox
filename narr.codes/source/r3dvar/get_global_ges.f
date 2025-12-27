       subroutine get_global_ges(mype,inges,jcap,nsig,np,nlon,nlath,erlon0,erlat0, &
                     imeta,jmeta,dlon8,dlat8,wb8,sb8,istagh, &
              tges,qges,pdres01,etai,etam,ptop,lmeta,lmh, &
              tgese,qgese,ozgese,pgese,pgesei,kpgesbot,npes)

!  obtain global guess t, q, ozone on eta grid
!  then blend with t and q from eta model background
!  above top of eta model and compress layers below eta steps
!  to equally spaced layers inside 1st layer above eta step.
!  this is so that we can call radiative transfer program as
!  is.  it expects the same number of levels for each observation
!  location, and each profile must go from the model surface to
!  the top of the atmosphere.

!    if global model is not available (jcap=0), then use climatological ozone
!       obtained from eta model subroutine OZON2D (rewritten as ozon3d),
!       and don't bother with extending fields above top of eta model.

!      each pe reads one level of coefficients, reconstructs it on the
!      global grid, and then distributes fragments for interpolation to local domains.
!      this is moderately scalable.

!  --> mype:    working pe
!  --> inges:   unit number of spectral sigma coefs of global guess
!  --> jcap:    spectral truncation of global guess
!  --> nsig:    number of sigma layers for global guess
!  --> np:      vertical dimension of analysis grid
!  --> nlon:    number of gaussian longitudes for global guess
!  --> nlath:   half number of gaussian lats for global guess (includes
!                   pole)
!  --> erlon0,erlat0:  earth coordinates of origin of rotated analysis
!                            grid (degrees)
!  --> imeta,jmeta,dlon,dlat,wb,sb,istagh: specs for eta grid
!  --> tges,qges: input ges from eta model
!  --> pdres01:   input pressure thickness from eta model (mb)
!  --> etai,etam: eta coordinates
!  --> ptop:      top pressure of eta model (mb)
!                     pres = ptop+pdres01*etam
!  --> lmeta:    number of eta model levels
!  --> lmh:       number of eta levels above eta step for each eta point
!  <-- tgese,qgese,ozgese:  augmented and modified output variables
!  <-- pgese,pgesei:       more of same
!  

  include 'mpif.h'
      include "my_comm.h"
  include 'PARMETA.comm'
      INCLUDE "CTLBLK.comm"
  real(4) tges(imeta*jmeta,lmeta),qges(imeta*jmeta,lmeta)
  real(4) pdres01(imeta*jmeta),etai(lmeta+1),etam(lmeta)
  integer(4) lmh(imeta*jmeta)
  real(4) tgese(imeta*jmeta,np),qgese(imeta*jmeta,np)
  real(4) ozgese(imeta*jmeta,np),pgese(imeta*jmeta,np),pgesei(imeta*jmeta,np+1)
  integer(4) kpgesbot(imeta*jmeta)
  real(8) dlon8,dlat8,wb8,sb8

  integer(4) idateg(4)
  real(4),allocatable::sigi(:),sigl(:),z(:),rc(:),rg(:),rgloc(:,:)
  real(4),allocatable::sendbuf(:)
  integer(4),allocatable::isendcounts(:),idispls(:)

  real(4),allocatable::pln(:,:),qln(:,:),rln(:,:)
  integer(4),allocatable::mlad(:,:),lmad(:,:),ml2lm(:),lm2ml(:)
  real(4),allocatable::factslm(:),factvlm(:),factsml(:),factvml(:)
  real(4),allocatable::del2(:),rlons(:),trigs(:),rlats(:),wgts(:)
  integer(4) ifax(10)
  integer(4),allocatable::i00eta(:),i10eta(:),i01eta(:),i11eta(:)
  real(4),allocatable::w00eta(:),w10eta(:),w01eta(:),w11eta(:),worketa(:,:)
  integer(4),allocatable::lons_loc_tab(:),lone_loc_tab(:),ng_loc_tab(:)
  integer(4),allocatable::lats_loc_tab(:),late_loc_tab(:)
  real(4) rlpg(np+1)
  real(4),allocatable::aetalon(:),aetalat(:),betalon(:),betalat(:)
  real(8) relat8


  special_value=1.e20
  test_value=.98*special_value
  imjmeta=imeta*jmeta
  allocate(aetalon(imjmeta))
  allocate(aetalat(imjmeta))
  allocate(betalon(imjmeta))
  allocate(betalat(imjmeta))

!   get lat, lon of eta grid points

  ii=0
  relat8=sb8-.5_8*dlat8
  do j=1,jmeta,2
   relat8=relat8+.5_8*dlat8
   do i=1,imeta
    ii=i+(j-1)*imeta
    aetalat(ii)=relat8
    aetalon(ii)=wb8+(i-1._8+.5_8*istagh)*dlon8
   end do
   if(j.ne.jmeta) then
    relat8=relat8+.5_8*dlat8
    do i=1,imeta
     ii=i+j*imeta
     aetalat(ii)=relat8
     aetalon(ii)=wb8+(i-1._8+.5_8*(1-istagh))*dlon8
    end do
   end if
  end do
  dg2rad=atan(1.)/45.
  cerlat0=cos(dg2rad*erlat0)
  serlat0=sin(dg2rad*erlat0)
  call invtllv(aetalon,aetalat,erlon0,dg2rad,cerlat0,serlat0, &
                 betalon,betalat,imjmeta)

 if(jcap.gt.0) then
         nc=(jcap+1)*(jcap+2)
         rewind inges
         read(inges)
  allocate(sigi(nsig+1)) ; allocate(sigl(nsig))
         read(inges)hourg,idateg,sigi,sigl
          if(mype.eq.0) then
           print *,' reconstruct global t, q, ozone for'
           print *,' hourg,idateg=',hourg,idateg
           print *,' sigi=',sigi
           print *,' sigl=',sigl
          end if
  deallocate(sigi)

!       initialize constants for s2angrid
  allocate(pln((jcap+1)*(jcap+2),nlath))
  allocate(qln((jcap+1)*(jcap+2),nlath))
  allocate(rln((jcap+1)*(jcap+2),nlath))
  allocate(mlad(0:jcap,0:jcap)) ; allocate(ml2lm((jcap+1)*(jcap+2)))
  allocate(factslm((jcap+1)*(jcap+2))) ; allocate(factvlm((jcap+1)*(jcap+2)))
  allocate(lmad(0:jcap,0:jcap))
  allocate(lm2ml((jcap+1)*(jcap+2)))
  allocate(factsml((jcap+1)*(jcap+2)))
  allocate(factvml((jcap+1)*(jcap+2)))
  allocate(del2((jcap+1)*(jcap+2)))
  allocate(rlons(nlon))
  allocate(trigs(nlon*2))
  allocate(rlats(nlath*2))
  allocate(wgts(nlath*2))
  allocate(i00eta(imjmeta))
  allocate(i10eta(imjmeta))
  allocate(i01eta(imjmeta))
  allocate(i11eta(imjmeta))
  allocate(w00eta(imjmeta))
  allocate(w10eta(imjmeta))
  allocate(w01eta(imjmeta))
  allocate(w11eta(imjmeta))
  allocate(lons_loc_tab(0:npes-1)) ; allocate(lone_loc_tab(0:npes-1))
  allocate(ng_loc_tab(0:npes-1))
  allocate(lats_loc_tab(0:npes-1)) ; allocate(late_loc_tab(0:npes-1))
       if(mype.eq.0) write(0,*)' at 1 in get_global_ges'
  call s2angrid0(jcap,nlath,nlon,erlon0,erlat0, &
                 pln,qln,rln,mlad,ml2lm,factslm,factvlm,lmad,lm2ml, &
                 factsml,factvml,del2,rlons,trigs,ifax,rlats,wgts, &
                 i00eta,i10eta,i01eta,i11eta,w00eta,w10eta,w01eta,w11eta, &
                     imeta,jmeta,imjmeta,dlon8,dlat8,wb8,sb8,istagh, &
                 lons_loc_tab,lone_loc_tab,lats_loc_tab,late_loc_tab,ng_loc_tab,mype,npes, &
                  aetalon,aetalat,betalon,betalat)
       if(mype.eq.0) write(0,*)' at 2 in get_global_ges'
  ng_loc=ng_loc_tab(mype)
  deallocate(qln) ; deallocate(rln) ; deallocate(mlad) ; deallocate(factvlm)
  deallocate(lmad) ; deallocate(lm2ml) ; deallocate(factsml) ; deallocate(factvml)
  deallocate(del2) ; deallocate(rlons) ; deallocate(rlats) ; deallocate(wgts)


!                        
!      terrain--not used
!      sfcp               1
!      t                  2:nsig+1
!      div,vort--not used
!      q                  nsig+2:2*nsig+1
!      ozone              2*nsig+2:3*nsig+1

!   terrain
         read(inges)

  allocate(z((jcap+1)*(jcap+2)))
  allocate(rc((jcap+1)*(jcap+2)))
  allocate(rg((2*nlath+1)*(nlon+2)))
  allocate(rgloc(ng_loc,3*nsig+1))
  allocate(idispls(0:npes-1))
  idispls(0)=0
  if(npes.gt.1) then
   do ipe=1,npes-1
    idispls(ipe)=idispls(ipe-1)+ng_loc_tab(ipe-1)
   end do
  end if
  isendsum=idispls(npes-1)+ng_loc_tab(npes-1)
  allocate(sendbuf(isendsum))
  
  kk0=0
  do loop=1,2+(3*nsig+1)/npes
   kk=kk0
   do ipe=0,npes-1
    kk=kk+1
       if(mype.eq.0) write(0,*)' at 3 in get_global_ges, kk=',kk
    if(kk.gt.3*nsig+1) exit
    if(kk.eq.nsig+2) then
!------------------------------skip past div and vort records
     do k=1,nsig
      read(inges)
      read(inges)
     end do
    end if
    if(ipe.ne.mype) then
     read(inges)
    else
     read(inges)z
     do i=1,nc
      rc(i)=factslm(i)*z(ml2lm(i))
     end do
     call s2g0(rc,rg,jcap,nlon,nlath,pln,trigs,ifax)
    end if
   end do
   kk=kk0
   do ipe=0,npes-1
    kk=kk+1
    if(kk.gt.3*nsig+1) exit
    do jpe=0,npes-1
     ii=idispls(jpe)
     do i=lons_loc_tab(jpe),lone_loc_tab(jpe)
      do j=lats_loc_tab(jpe),late_loc_tab(jpe)
       ii=ii+1
       ji=j+(i-1)*(2*nlath+1)
       sendbuf(ii)=rg(ji)
      end do
     end do
    end do
    call mpi_scatterv(sendbuf,ng_loc_tab,idispls,mpi_real,rgloc(1,kk),ng_loc,mpi_real, &
                      ipe,my_comm,ierror)
   end do
   kk0=kk
  end do
  deallocate(lons_loc_tab)
  deallocate(lone_loc_tab)
  deallocate(lats_loc_tab)
  deallocate(late_loc_tab)
  deallocate(ng_loc_tab)
  deallocate(idispls)
  close(inges)
  deallocate(z) ; deallocate(rc) ; deallocate(rg) ; deallocate(pln) ; deallocate(ml2lm)
  deallocate(factslm) ; deallocate(trigs)
  deallocate(sendbuf)
   
!---------- do horizontal interpolation

  allocate(worketa(imjmeta,3*nsig+1))
  do kk=1,3*nsig+1
   do i=1,imjmeta
    worketa(i,kk)=w00eta(i)*rgloc(i00eta(i),kk)+w10eta(i)*rgloc(i10eta(i),kk) &
               +w01eta(i)*rgloc(i01eta(i),kk)+w11eta(i)*rgloc(i11eta(i),kk)
   end do
!   convert from cb to mb
   if(kk.eq.1) worketa(:,kk)=10.*exp(worketa(:,kk))
  end do

  deallocate(rgloc)
  deallocate(i00eta) ; deallocate(i10eta)
  deallocate(i01eta) ; deallocate(i11eta) ; deallocate(w00eta) ; deallocate(w10eta)
  deallocate(w01eta) ; deallocate(w11eta)

 end if
   

!        compute modified vertical pressure coordinate
!        for eta model

  tgese=special_value
  qgese=special_value
  ozgese=special_value
  pgese=special_value
  pgesei=special_value
  do i=1,imjmeta
   if(pdres01(i).lt.test_value) then
    kbot=lmh(i)
    kpgesbot(i)=lmeta+1-kbot
    do k=1,kbot
     kr=lmeta+1-k
     pgese(i,kr)=etam(k)*pdres01(i)+ptop
     pgesei(i,kr+1)=etai(k)*pdres01(i)+ptop
     tgese(i,kr)=tges(i,k)
     qgese(i,kr)=qges(i,k)
    end do
    pgesei(i,1)=etai(kbot+1)*pdres01(i)+ptop
    krbotm=lmeta+2-kbot
    delpbot=(pgesei(i,1)-pgesei(i,krbotm))/(krbotm-1.)
    do kr=2,krbotm
     pgesei(i,kr)=pgesei(i,1)-(kr-1.)*delpbot
     pgese(i,kr-1)=.5*(pgesei(i,kr-1)+pgesei(i,kr))
     tgese(i,kr-1)=tges(i,kbot)               ! this is most simpleminded.
     qgese(i,kr-1)=qges(i,kbot)              !  later might consider some
    end do                                   !    profile of t,q.
    if(np.gt.lmeta) then
     krtopm=lmeta
!    delptop=pgesei(i,krtopm)/(np+1-krtopm)
!    do kr=krtopm+1,np+1
!     pgesei(i,kr)=pgesei(i,krtopm)-(kr-krtopm)*delptop
!     pgese(i,kr-1)=.5*(pgesei(i,kr-1)+pgesei(i,kr))
!    end do
         rlpg(krtopm)=log(pgesei(i,krtopm))
         rlpg(krtopm-1)=log(pgesei(i,krtopm-1))
         drlpg=rlpg(krtopm)-rlpg(krtopm-1)
         do kr=krtopm+1,np+1
          rlpg(kr)=rlpg(kr-1)+drlpg
         end do
         do kr=krtopm-1,np+1
          pgesei(i,kr)=exp(rlpg(kr))
          pgese(i,kr-1)=.5*(pgesei(i,kr)+pgesei(i,kr-1))
         end do
!    pgesei(i,np+1)=0.
    end if
   end if
  end do


!  do vertical interpolation of global temps, where regional temps not available

 if(jcap.gt.0) then
     icount_t=0
  do i=1,imjmeta
   do k=1,np
    if(tgese(i,k).gt.test_value.and.pgese(i,k).lt.test_value) then
           icount_t=icount_t+1
     ptarget=pgese(i,k)
     do l=1,nsig-1
      ll=l+1
      p1=sigl(l)*worketa(i,1)
      p2=sigl(l+1)*worketa(i,1)
      if(ptarget.le.p1.and.ptarget.gt.p2) &
         tgese(i,k)=(worketa(i,ll+1)*(ptarget-p2) &
                +worketa(i,ll)*(p1-ptarget))/(p1-p2)
     end do
    end if
   end do
  end do
!             print *,' in get_global_ges, mype,icount_t=',mype,icount_t

!  fill in any t holes

         icount_t_bholes=0
         icount_t_tholes=0
  do k=np/2,2,-1
   do i=1,imjmeta
            if(tgese(i,k).lt.test_value.and.tgese(i,k-1).gt.test_value) icount_t_bholes=icount_t_bholes+1
    if(tgese(i,k-1).gt.test_value) then
     tgese(i,k-1)=tgese(i,k)
    end if
   end do
  end do
  do k=np/2,np-1
   do i=1,imjmeta
            if(tgese(i,k).lt.test_value.and.tgese(i,k+1).gt.test_value) icount_t_tholes=icount_t_tholes+1
    if(tgese(i,k+1).gt.test_value) then
     tgese(i,k+1)=tgese(i,k)
    end if
   end do
  end do
!             print *,' in get_global_ges, mype,icount_t_b,tholes=', &
!                        mype,icount_t_bholes,icount_t_tholes

!  do vertical interpolation of global q, where regional q not available

     icount_q=0
  do i=1,imjmeta
   do k=1,np
    if(qgese(i,k).gt.test_value.and.pgese(i,k).lt.test_value) then
           icount_q=icount_q+1
     ptarget=pgese(i,k)
     do l=1,nsig-1
      ll=l+1+nsig
      p1=sigl(l)*worketa(i,1)
      p2=sigl(l+1)*worketa(i,1)
      if(ptarget.le.p1.and.ptarget.gt.p2) &
         qgese(i,k)=(worketa(i,ll)*(ptarget-p2) &
                +worketa(i,ll+1)*(p1-ptarget))/(p1-p2)
     end do
    end if
   end do
  end do
!             print *,' in get_global_ges, mype,icount_q=',mype,icount_q

!  fill in any q holes

         icount_q_holes=0
  do k=np/2,2,-1
   do i=1,imjmeta
    if(qgese(i,k-1).gt.test_value) then
           icount_q_holes=icount_q_holes+1
     qgese(i,k-1)=qgese(i,k)
    end if
   end do
  end do
  do k=np/2,np-1
   do i=1,imjmeta
    if(qgese(i,k+1).gt.test_value) then
           icount_q_holes=icount_q_holes+1
     qgese(i,k+1)=qgese(i,k)
    end if
   end do
  end do
!             print *,' in get_global_ges, mype,icount_q_holes=',mype,icount_q_holes

!  do vertical interpolation of global ozone

     icount_o=0
  ptargetmax=-huge(ptargetmax)
  ptargetmin=huge(ptargetmin)
  do i=1,imjmeta
   do k=1,np
    if(pgese(i,k).lt.test_value) then
           icount_o=icount_o+1
     ptarget=pgese(i,k)
     ptargetmax=max(ptarget,ptargetmax)
     ptargetmin=min(ptarget,ptargetmin)
     do l=1,nsig-1
      ll=l+1+2*nsig
      p1=sigl(l)*worketa(i,1)
      p2=sigl(l+1)*worketa(i,1)
      if(ptarget.le.p1.and.ptarget.gt.p2) then
       ozgese(i,k)=(worketa(i,ll)*(ptarget-p2) &
               +worketa(i,ll+1)*(p1-ptarget))/(p1-p2)
      end if
     end do
    end if
   end do
  end do
!             print *,' in get_global_ges, mype,icount_o=',mype,icount_o
  deallocate(sigl) ; deallocate(worketa)

!  fill in any ozone holes

         icount_o_holes=0
  do k=np/2,2,-1
   do i=1,imjmeta
    if(ozgese(i,k-1).gt.test_value) then
           icount_o_holes=icount_o_holes+1
     ozgese(i,k-1)=ozgese(i,k)
    end if
   end do
  end do
  do k=np/2,np-1
   do i=1,imjmeta
    if(ozgese(i,k+1).gt.test_value) then
           icount_o_holes=icount_o_holes+1
     ozgese(i,k+1)=ozgese(i,k)
    end if
   end do
  end do

!             print *,' in get_global_ges, mype,icount_o_holes=',mype,icount_o_holes
 end if

!    if no global guess, then get climatological ozone here

 if(jcap.le.0) then

!***
!***  FIND THE MEAN COSINE OF THE SOLAR ZENITH ANGLE
!***  BETWEEN THE CURRENT TIME AND THE NEXT TIME RADIATION IS
!***  CALLED.  ONLY AVERAGE IF THE SUN IS ABOVE THE HORIZON.
!***
       rlag=14.8125
       pi2=2.*3.14159265
      TIME=(NTSD-1)*DT
      CALL ZENITH(TIME,DAYI,HOUR)
      JD=INT(DAYI+0.50)
      ADDL=0.
      IF(MOD(IDAT(3),4).EQ.0)ADDL=1.
      RANG=PI2*(DAYI-RLAG)/(365.25+ADDL)
      RSIN1=SIN(RANG)
      RCOS1=COS(RANG)
      RCOS2=COS(2.*RANG)
!             if(mype.eq.5) print *,' before ozon3d, imeta,idim2-idim1+1=',imeta,idim2-idim1+1
!             if(mype.eq.5) print *,' before ozon3d, jmeta,jdim2-jdim1+1=',jmeta,jdim2-jdim1+1
      call ozon3d(test_value,idim1,idim2,jdim1,jdim2,lmeta, &
                  pgese,betalat,rsin1,rcos1,rcos2,ozgese)

 end if

  deallocate(aetalon)
  deallocate(aetalat)
  deallocate(betalon)
  deallocate(betalat)

return
end subroutine get_global_ges

subroutine s2angrid0(jcap,nlath,nlon,erlon0,erlat0, &
          pln,qln,rln,mlad,ml2lm,factslm,factvlm,lmad,lm2ml, &
          factsml,factvml,del2,rlons,trigs,ifax,rlats,wgts, &
          i00eta,i10eta,i01eta,i11eta,w00eta,w10eta,w01eta,w11eta, &
          imeta,jmeta,imjmeta,dlon8,dlat8,wb8,sb8,istagh, &
          lons_loc_tab,lone_loc_tab,lats_loc_tab,late_loc_tab,ng_loc_tab,mype,npes, &
          aetalon,aetalat,betalon0,betalat0)

!    setup necessary constants for s2angrid, which converts spectral
!     coefs to analysis grid.

!  --> jcap:    spectral resolution
!  --> nlath:   half number of gaussian lats (including pole)
!  --> nlon:    number of gaussian lons
!  --> erlon0,erlat0:  origin of rotated eta grid

  include 'mpif.h'
      include "my_comm.h"


  real(4) pln((jcap+1)*(jcap+2),nlath),qln((jcap+1)*(jcap+2),nlath)
  real(4) rln((jcap+1)*(jcap+2),nlath)
  integer(4) mlad(0:jcap,0:jcap),ml2lm((jcap+1)*(jcap+2))
  real(4) factslm((jcap+1)*(jcap+2)),factvlm((jcap+1)*(jcap+2))
  integer(4) lmad(0:jcap,0:jcap),lm2ml((jcap+1)*(jcap+2))
  real(4) factsml((jcap+1)*(jcap+2)),factvml((jcap+1)*(jcap+2))
  real(4) del2((jcap+1)*(jcap+2)),rlons(nlon)
  real(4) trigs(nlon*2),rlats(nlath*2),wgts(nlath*2)
  integer(4) ifax(10)
  integer(4) i00eta(imjmeta),i10eta(imjmeta),i01eta(imjmeta),i11eta(imjmeta)
  real(4) w00eta(imjmeta),w10eta(imjmeta),w01eta(imjmeta),w11eta(imjmeta)
  real(4) aetalon(imjmeta),aetalat(imjmeta),betalon0(imjmeta),betalat0(imjmeta)

  real(4),allocatable::betalon(:),betalat(:)
  real(8) dlon8,dlat8,wb8,sb8
  integer(4) lons_loc_tab(0:npes-1),lone_loc_tab(0:npes-1)
  integer(4) lats_loc_tab(0:npes-1),late_loc_tab(0:npes-1)
  integer(4) ng_loc_tab(0:npes-1)

  allocate(betalon(imjmeta))
  allocate(betalat(imjmeta))
  betalon=betalon0
  betalat=betalat0

!-------- set up index arrays to convert coefs to internal format

         if(mype.eq.0) write(0,*)' at 1 in s2angrid0'
  ii=-1
  do l=0,jcap
   do m=0,jcap-l
    ii=ii+2
    mlad(m,l)=ii
   end do
  end do
  ii=-1
  do m=0,jcap
   do l=0,jcap-m
    ii=ii+2
    ml2lm(ii)=mlad(m,l)
    ml2lm(ii+1)=ml2lm(ii)+1
    lmad(m,l)=ii
   end do
  end do
  ii=-1
  do l=0,jcap
   do m=0,jcap-l
    ii=ii+2
    lm2ml(ii)=lmad(m,l)
    lm2ml(ii+1)=lm2ml(ii)+1
   end do
  end do
  ii=-1
  do m=0,jcap
   ii=ii+2
   factslm(ii)=1.
   factslm(ii+1)=0.
   if(m.lt.jcap) then
    do l=1,jcap-m
     ii=ii+2
     factslm(ii)=1.
     factslm(ii+1)=1.
    end do
   end if
  end do
  factvlm=factslm
  factvlm(1)=0.
  ii=-1
  do l=0,jcap
   one=1.
   zero=min(1,l)
   do m=0,jcap-l
    ii=ii+2
    factsml(ii)=one
    factsml(ii+1)=zero
   end do
  end do
  factvml=factsml
  factvml(1)=0.

!-------- initialize various transform constants

!        if(mype.eq.0) write(0,*)' at 2 in s2angrid0'
  call getlalo(rlats,rlons,wgts,jcap,nlon,nlath,del2,trigs,ifax,pln,qln,rln)

!    initialize interpolation constants


  rad2dg=45./atan(1.)
  rlats=rad2dg*rlats
  rlons=rad2dg*rlons
     do i=1,imjmeta
      if(betalon(i).le.0.) betalon(i)=betalon(i)+360.
     end do

  call gdcrdp(betalat,imjmeta,rlats,2*nlath)
  call gdcrdp(betalon,imjmeta,rlons,nlon)
!        if(mype.eq.0) write(0,*)' at 4 in s2angrid0'

  i0max=-huge(i0max)
  i0min=huge(i0min)
  j0max=-huge(j0max)
  j0min=huge(j0min)
  do i=1,imjmeta
   i0=betalon(i)
   delx=betalon(i)-i0
   if(i0.lt.1) i0=i0+nlon
   if(i0.gt.nlon) i0=i0-nlon
   i1=i0+1
   if(i1.lt.1) i1=i1+nlon
   if(i1.gt.nlon) i1=i1-nlon
   j0=betalat(i)
   dely=betalat(i)-j0
   j0=max(1,min(2*nlath-1,j0))
   j1=j0+1
   i00eta(i)=(i0-1)*(2*nlath+1)+j0
   i10eta(i)=(i1-1)*(2*nlath+1)+j0
   i01eta(i)=(i0-1)*(2*nlath+1)+j1
   i11eta(i)=(i1-1)*(2*nlath+1)+j1
   w00eta(i)=(1.-delx)*(1.-dely)
   w10eta(i)=(1.-delx)*dely
   w01eta(i)=delx*(1.-dely)
   w11eta(i)=delx*dely
   i0max=max(i0,i1,i0max)
   i0min=min(i0,i1,i0min)
   j0max=max(j0,j1,j0max)
   j0min=min(j0,j1,j0min)
  end do

!------ distribute index ranges to all pe's

  lons_loc_tab(mype)=i0min
  lone_loc_tab(mype)=i0max
  lats_loc_tab(mype)=j0min
  late_loc_tab(mype)=j0max
  ng_loc_tab(mype)=(j0max-j0min+1)*(i0max-i0min+1)
  do ipe=0,npes-1
   call mpi_bcast(lons_loc_tab(ipe),1,mpi_integer,ipe,my_comm,ierr)
   call mpi_bcast(lone_loc_tab(ipe),1,mpi_integer,ipe,my_comm,ierr)
   call mpi_bcast(lats_loc_tab(ipe),1,mpi_integer,ipe,my_comm,ierr)
   call mpi_bcast(late_loc_tab(ipe),1,mpi_integer,ipe,my_comm,ierr)
   call mpi_bcast(ng_loc_tab(ipe),1,mpi_integer,ipe,my_comm,ierr)
  end do
!        if(mype.eq.0) write(0,*)' at 5 in s2angrid0'

!---- convert addresses to local grid

  ibad=0
  do i=1,imjmeta
   call get_ijk(i00eta(i),jglb,iglb,kglb,1,2*nlath+1,1,nlon,1)
   iloc=iglb-i0min+1
   jloc=jglb-j0min+1
   if(iglb.lt.i0min.or.iglb.gt.i0max.or.jglb.lt.j0min.or.jglb.gt.j0max) then
         print *,' i00eta,jglb,iglb,jloc,iloc=',i00eta(i),jglb,iglb,jloc,iloc
         print *,' nlath,nlon,i0min,i0max,j0min,j0max=',nlath,nlon,i0min,i0max,j0min,j0max
     ibad=ibad+1
          stop
   end if

   i00eta(i)=jloc+(j0max-j0min+1)*(iloc-1)
   call get_ijk(i10eta(i),jglb,iglb,kglb,1,2*nlath+1,1,nlon,1)
   iloc=iglb-i0min+1
   jloc=jglb-j0min+1
   if(iglb.lt.i0min.or.iglb.gt.i0max.or.jglb.lt.j0min.or.jglb.gt.j0max) ibad=ibad+1
   i10eta(i)=jloc+(j0max-j0min+1)*(iloc-1)
   call get_ijk(i01eta(i),jglb,iglb,kglb,1,2*nlath+1,1,nlon,1)
   iloc=iglb-i0min+1
   jloc=jglb-j0min+1
   if(iglb.lt.i0min.or.iglb.gt.i0max.or.jglb.lt.j0min.or.jglb.gt.j0max) ibad=ibad+1
   i01eta(i)=jloc+(j0max-j0min+1)*(iloc-1)
   call get_ijk(i11eta(i),jglb,iglb,kglb,1,2*nlath+1,1,nlon,1)
   iloc=iglb-i0min+1
   jloc=jglb-j0min+1
   if(iglb.lt.i0min.or.iglb.gt.i0max.or.jglb.lt.j0min.or.jglb.gt.j0max) ibad=ibad+1
   i11eta(i)=jloc+(j0max-j0min+1)*(iloc-1)
  end do
!        if(mype.eq.0) write(0,*)' at 6 in s2angrid0'
!      write(0,*)' at 7 in s2angrid0, ibad=',ibad
     if(ibad.gt.0) then
      print *,' PROBLEM IN S2ANGRID0, INCONSISTENT INTERPOLATION ADDRESSES'
      call mpi_finalize(ierror)
      stop
     end if

  deallocate(betalon)
  deallocate(betalat)

return
end subroutine s2angrid0

       subroutine s2g0(ts,t,jcap,nlon,nlath,pln,trigs,ifax)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    s2g0       inverse of g2s0.
!   prgmmr: parrish          org: w/nmc22    date: 90-09-21
!
! abstract: summation of scalar spherical harmonic series.
!
! program history log:
!   90-09-21  parrish
!
!   input argument list:
!     ts       - spectral coefs
!     jcap     - triangular truncation
!     nlon     - number of longitudes
!     nlath    - number of gaussian lats in one hemisphere
!     pln      - spherical harmonics
!     trigs,ifax - used by fft
!
!   output argument list:
!     t        - values of desired field on gaussian grid
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$


  real(4) ts((jcap+1)*(jcap+2)),t(2*nlath+1,nlon+2)
  real(4) trigs(nlon*2),pln((jcap+1)*(jcap+2),nlath)
  integer(4) ifax(10)
  real(4) work(2*(2*nlath+1)*(nlon+2)),te(2*jcap+2),to(2*jcap+2)


         t=0.
         to=0.
         do j=1,nlath
          jr=2*nlath+1-j
          do ll=1,2*(jcap+1)
           te(ll)=pln(ll,j)*ts(ll)
          end do
          ii0=2*(jcap+1)
          do ll=1,2*jcap
           to(ll)=pln(ii0+ll,j)*ts(ii0+ll)
          end do
          ii0=ii0+2*(jcap)

!---------- now combine even and odd parts
          do m=2,jcap,2
           do ll=1,2*(jcap+1-m)
            te(ll)=te(ll)+pln(ii0+ll,j)*ts(ii0+ll)
           end do
           if(m.lt.jcap) then
            ii0=ii0+2*(jcap+1-m)
            do ll=1,2*(jcap-m)
             to(ll)=to(ll)+pln(ii0+ll,j)*ts(ii0+ll)
            end do
            ii0=ii0+2*(jcap-m)
           end if
          end do

!---------- now combine even and odd parts

          do ll=1,2*(jcap+1)
           t(j,ll)=te(ll)+to(ll)
           t(jr,ll)=te(ll)-to(ll)
          end do
         end do

!-------- finally do fourier sums in longitude

         lot=nlath*2
         nlax=lot+1
         call rfftmlt(t,work,trigs,ifax,nlax,1,nlon,lot,1)
       return
       end subroutine s2g0

      subroutine getlalo(rlatsout,rlonsout,wgtsout,jcap,nlon,nlath, &
          del2out,trigsout,ifax,pln,qln,rln)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getlalo    return gaussian lats and lons and trancons
!   prgmmr: parrish          org: w/nmc22    date: 90-09-21
!
! abstract: return gaussian lats, lons, and transform stuff.
!
! program history log:
!   90-09-21  parrish
!
!   input argument list:
!     jcap     - triangular truncation
!     nlon     - number of longitudes
!     nlath    - number of gaussian lats in one hemisphere
!
!   output argument list:
!     rlats,rlons    - grid latitudes, longitudes (radians)
!     wgts     - gaussian integration weights
!     pln,qln,rln - spherical harmonics.
!     del2     - n*(n+1)/(a**2)
!     trigs,ifax - used by fft
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$


  integer(4) ifax(10)
  real(4) rlatsout(2*nlath),wgtsout(2*nlath),rlonsout(nlon),trigsout(nlon*2)
  real(4) pln((jcap+1)*(jcap+2),nlath),qln((jcap+1)*(jcap+2),nlath)
  real(4) rln((jcap+1)*(jcap+2),nlath),del2out((jcap+1)*(jcap+2))

  real(8) pih,dlon
  real(8) rlats(2*nlath),wgts(2*nlath),rlons(nlon)
  real(8) ap(0:jcap,0:jcap),bp(0:jcap,0:jcap)
  real(8) aqr(0:jcap,0:jcap),bqr(0:jcap,0:jcap)
  real(8) gr(0:jcap,0:jcap),del2(0:jcap,0:jcap)
  real(8) slat(nlath),clat(nlath)
  real(8) pe0(nlath,0:jcap),qe0(nlath,0:jcap),ro0(nlath,0:jcap)
  real(8) w1(nlath-1),w2(nlath-1),w3(nlath-1),w4(nlath-1)

!-------- get stuff for fft*s

         call fftfax(nlon,ifax,trigsout)
! trigsout=trigs       !    don't need.  trigs is already r*32

!-------- compute recursion constants and initialize legendre functions

      call m1rcons(ap,bp,aqr,bqr,gr,del2,jcap)

!-------- get latitudes, longitudes, and integration weights.

      pih=2._8*atan(1._8)
      call m1glat(nlath-1,w1,w2,w3,w4)
      rlats(1)=-pih
      rlatsout(1)=rlats(1)
      wgts(1)=0.
      wgtsout(1)=wgts(1)
!dir$ ivdep
      do 100 i=1,nlath-1
        rlats(i+1)=w1(i)-pih
        rlatsout(i+1)=rlats(i+1)
        wgts(i+1)=w2(i)
        wgtsout(i+1)=wgts(i+1)
100   continue
!dir$ ivdep
      do 200 i=1,nlath
        rlats(nlath*2+1-i)=-rlats(i)
        rlatsout(nlath*2+1-i)=rlats(nlath*2+1-i)
        wgts(nlath*2+1-i)=wgts(i)
        wgtsout(nlath*2+1-i)=wgts(nlath*2+1-i)
        slat(i)=sin(rlats(i))
        clat(i)=cos(rlats(i))
200   continue
      dlon=8._8*atan(1._8)/nlon
      do 300 i=1,nlon
        rlons(i)=(i-1._8)*dlon
        rlonsout(i)=rlons(i)
300   continue

!-------- next get initial p,q,r.

      call m1ipqr(pe0,qe0,ro0,slat,clat,nlath,jcap)
!-------- finally get pln,qln,rln
      call getpln(pln,qln,rln,jcap,nlath,ap,bp,slat,pe0, &
                qe0,ro0,aqr,bqr,gr,clat,del2,del2out)
      return
      end subroutine getlalo 

       subroutine getpln(pln,qln,rln,jcap,nlath,ap,bp,slat,pe0, &
                qe0,ro0,aqr,bqr,gr,clat,del2,del2out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getpln     generate legendre polynomials
!   prgmmr: parrish          org: w/nmc22    date: 90-09-21
!
! abstract: summation of scalar spherical harmonic series.
!
! program history log:
!   90-09-21  parrish
!
!   input argument list:
!     jcap     - triangular truncation
!     nlath    - number of gaussian lats in one hemisphere
!     ap,bp    - recursion constants for spherical harmonics
!     slat     - sin(gaussian latitudes)
!     pe0,qe0,ro0      - starting functions for spherical harmonics
!
!   output argument list:
!     pln,qln,rln      - legendre polynomials
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$
!????????????????????????????
!??parallel section in this??
!??subroutine may be of??????
!??questionable value????????
!?? but everything still ????
!??works so leave for now????
!????????????????????????????

  include 'mpif.h'
      include "my_comm.h"

  real(8) ap(0:jcap,0:jcap),aqr(0:jcap,0:jcap)
  real(8) bp(0:jcap,0:jcap),bqr(0:jcap,0:jcap)
  real(8) gr(0:jcap,0:jcap)
  real(8) del2(0:jcap,0:jcap)
  real(8) slat(nlath),clat(nlath)
  real(8) pe0(nlath,0:jcap)
  real(8) qe0(nlath,0:jcap)
  real(8) ro0(nlath,0:jcap)
  real(4) pln((jcap+1)*(jcap+2),nlath)
  real(4) qln((jcap+1)*(jcap+2),nlath)
  real(4) rln((jcap+1)*(jcap+2),nlath)
  real(4) del2out((jcap+1)*(jcap+2))

!-------- internal scratch dynamic space follows:

  integer(4) iadr(0:jcap,0:jcap)
  real(8) pe(nlath,0:jcap),po(nlath,0:jcap)
  real(8) qe(nlath,0:jcap),qo(nlath,0:jcap)
  real(8) re(nlath,0:jcap),ro(nlath,0:jcap)
  real(4) pqrbuf((jcap+1)*(jcap+2),3)

  call mpi_comm_rank(my_comm,mype,ierror)
         ii=-1
         do m=0,jcap
          do l=0,jcap-m
           ii=ii+2
           iadr(l,m)=ii
          end do
         end do
         do m=0,jcap
          del2out(iadr(0,m))=del2(m,0)
          del2out(iadr(0,m)+1)=0.
          if(m.lt.jcap) then
           do l=1,jcap-m
            del2out(iadr(l,m))=del2(m,l)
            del2out(iadr(l,m)+1)=del2(m,l)
           end do
          end if
         end do
  j0=0
  do loop=1,2+nlath/npes
   j=j0
   do ipe=0,npes-1
    j=j+1
    if(j.gt.nlath) exit
    if(ipe.eq.mype) then
          pln(:,j)=0.
          qln(:,j)=0.
          rln(:,j)=0.
          po(j,:)=0._8
          pe(j,:)=pe0(j,:)
          qo(j,:)=0._8
          qe(j,:)=qe0(j,:)
          re(j,:)=0._8
          ro(j,:)=ro0(j,:)
          do l=0,jcap
           do m=0,jcap-l,2
!------------ first even terms (m=0,2,...)
            pln(iadr(l,m),j)=pe(j,l)
            qln(iadr(l,m),j)=qe(j,l)
            rln(iadr(l,m),j)=ro(j,l)
!------------ now do odd  (m=1,3,...)
            if(m+1.le.jcap-l) then
             mp=m+1
             po(j,l)=ap(m,l)*slat(j)*pe(j,l)+bp(m,l)*po(j,l)
             qo(j,l)=aqr(m,l)*slat(j)*qe(j,l)+bqr(m,l)*qo(j,l)
             re(j,l)=aqr(m,l)*slat(j)*ro(j,l)+bqr(m,l)*re(j,l)+gr(m,l)*pe(j,l)*clat(j)
             pln(iadr(l,mp),j)=po(j,l)
             qln(iadr(l,mp),j)=qo(j,l)
             rln(iadr(l,mp),j)=re(j,l)
!-------------- get next pe
             pe(j,l)=ap(mp,l)*slat(j)*po(j,l)+bp(mp,l)*pe(j,l)
             qe(j,l)=aqr(mp,l)*slat(j)*qo(j,l)+bqr(mp,l)*qe(j,l)
             ro(j,l)=aqr(mp,l)*slat(j)*re(j,l)+bqr(mp,l)*ro(j,l)+gr(mp,l)*po(j,l)*clat(j)
            end if
           end do
          end do
          do i=1,(jcap+1)*(jcap+2),2
           pln(i+1,j)=pln(i,j)
           qln(i+1,j)=qln(i,j)
           rln(i+1,j)=rln(i,j)
          end do
    end if
   end do
   j=j0
   do ipe=0,npes-1
    j=j+1
    if(j.gt.nlath) exit
    pqrbuf(:,1)=pln(:,j)
    pqrbuf(:,2)=qln(:,j)
    pqrbuf(:,3)=rln(:,j)
    call mpi_bcast(pqrbuf,(jcap+1)*(jcap+2)*3,mpi_real,ipe,my_comm,ierror)
    pln(:,j)=pqrbuf(:,1)
    qln(:,j)=pqrbuf(:,2)
    rln(:,j)=pqrbuf(:,3)
   end do
   j0=j
  end do

       return
       end subroutine getpln

      subroutine m1glat(khalf,colrad,wgt,wgtcs,rcs2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1glat      compute gaussian lats and weights
!   prgmmr: sela             org: w/nmc22    date: 79-03-03
!
! abstract: compute gaussian latitudes and weights. see p887, 25.4.29,
!   handbook of math functions, abramowitz and stegun, for details.
!
! program history log:
!   79-03-03  sela
!   88-04-08  parrish   add docblock
!
!   input argument list:
!     khalf    - number of gaussian latitudes to compute (pole to
!              - equator)
!
!   output argument list:
!     colrad   - gaussian colatitudes in radians (full precision)
!     wgt      - integration weights (full precision)
!     wgtcs    - wgt/(cos(lat))**2
!     rcs2     - 1/(cos(lat))**2
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$


  real(8) colrad(*),wgt(*),wgtcs(*),rcs2(*)

  real(8) eps,si,rk2,scale,pi,dradz,rad,drad,p1,p2,phi,x,w,sn,rc,prphi,prcol
  real(8) one,ten,two,four,threesixo,zero,oneeighto,fourth

      one=1._8 ; ten=10._8 ; two=2._8 ; four=4._8 ; threesixo=360._8 ; zero=0._8
      fourth=.25_8
      oneeighto=180._8
      eps=ten**(-12)
      si=one
      k2=2*khalf
      rk2=k2
      scale=two/(rk2*rk2)
      k1=k2-1
      pi=atan(si)*four
      dradz=pi/threesixo
      rad=zero
      do 1000 k=1,khalf
        iter=0
        drad=dradz
1       call m1poly(k2,rad,p2)
2     p1 =p2
      iter=iter+1
      rad=rad+drad
      call m1poly(k2,rad,p2)
      if(sign(si,p1).eq.sign(si,p2))go to 2
      if(drad.lt.eps)go to 3
      rad=rad-drad
      drad=drad*fourth
      go to 1
3     continue
      colrad(k)=rad
      phi=rad   *oneeighto/pi
      call m1poly(k1,rad,p1)
      x=  cos(rad)
      w=scale*(one-x*x)/(p1*p1)
      wgt(k)=    w
      sn=  sin(rad)
      w=w/(sn*sn)
      wgtcs(k)=    w
      rc=one/(sn*sn)
      rcs2(k)=   rc
      call m1poly(k2,rad,p1)
      prphi =    phi
      prcol =    colrad(k)
1000  continue
      return
      end subroutine m1glat

      subroutine m1ipqr(pe,qe,ro,slat,clat,nlath,jcap)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1ipqr     initialize legendre recursions
!   prgmmr: parrish          org: w/nmc22    date: 90-09-21
!
! abstract: initialize legendre recursions.
!
! program history log:
!   90-09-21  parrish
!
!   input argument list:
!     slat,clat - sin and cos of latitudes where functions are given
!     nlath    - number of latitudes where functions are evaluated
!     jcap     - triangular truncation
!
!   output argument list:
!     pe,qe,ro - starting functions for spherical harmonic recursions
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$


  real(8) pe(nlath,0:jcap),qe(nlath,0:jcap),ro(nlath,0:jcap)
  real(8) slat(nlath),clat(nlath)

  real(8) rerth,one,two,three,sixteen,zero

  one=1._8 ; two=2._8 ; three=3._8 ; sixteen=16._8 ; zero=0._8

        rerth=conmc('rerth$')
        do 60 j=1,nlath
          pe(j,0)=one/sqrt(two)
          qe(j,0)=zero
          qe(j,1)=rerth*sqrt(three/sixteen)
          do 30 l=1,jcap
            pe(j,l)=sqrt((two*l+one)/(two*l))*clat(j)*pe(j,l-1)
30        continue
          do 40 l=2,jcap
            qe(j,l)=sqrt((two*l+one)/(two*l))*(l/(l+one))*clat(j)*qe(j,l-1)
 40       continue
          do 50 l=0,jcap
            ro(j,l)=-qe(j,l)*slat(j)
 50       continue
60      continue
      return
      end subroutine m1ipqr

      subroutine m1poly(n,rad,p)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1poly      used for computing gaussian lats
!   prgmmr: sela             org: w/nmc22    date: 79-03-03
!
! abstract: compute legendre polynomial of order n
!
! program history log:
!   79-03-03  sela
!   88-04-08  parrish   add docblock
!
!   input argument list:
!     n,rad    - order and argument of desired legendre polynomial
!
!   output argument list:
!     p        - value of desired legendre polynomial
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$


  real(8) rad,p

  real(8) x,y1,y2,g,y3,one

      one=1._8
      x=  cos(rad)
      y1=one
      y2=x
      do 1 i=2,n
        g=x*y2
        y3=g-y1+g-(g-y1)/float(i)
        y1=y2
        y2=y3
1     continue
      p=y3
      return
      end subroutine m1poly

      subroutine m1rcons(ap,bp,aqr,bqr,gr,del2,jcap)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1rcons    compute legendre generator constants
!   prgmmr: parrish          org: w/nmc22    date: 90-09-21
!
! abstract: get generator constants needed for legendre transforms
!
! program history log:
!   90-09-21  parrish
!
!   input argument list:
!     jcap     - triangular truncation
!
!   output argument list:
!     ap,bp,aqr,bqr,gr - various recursion constants
!     del2     - n*(n+1)/(a**2)
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$


  real(8) ap(0:jcap,0:jcap),bp(0:jcap,0:jcap),aqr(0:jcap,0:jcap)
  real(8) bqr(0:jcap,0:jcap),gr(0:jcap,0:jcap),del2(0:jcap,0:jcap)

  real(8) rerth,two,three,one

  two=2._8 ; three=3._8 ; one=1._8
      rerth=conmc('rerth$')
      ap=0.
      bp=0.
      aqr=0.
      bqr=0.
      gr=0.
      del2=0.
      do 20 m=0,jcap
        do 10 l=0,jcap-m
          n=m+l
          ap(m,l)=sqrt((two*n+one)*(two*n+three)/ ((n-l+one)*(n+l+one)))
          bp(m,l)=-sqrt((n-l)*(n+l)*(two*n+three)/ & 
                           ((n-l+one)*(n+l+one)*max(one,two*n-one)))
          aqr(m,l)=ap(m,l)*n/(n+two)
          bqr(m,l)=bp(m,l)*n*(n-one)/((n+one)*(n+two))
          gr(m,l)=rerth*ap(m,l)/((n+one)*(n+two))           ! mistake here corrected ??????
          del2(m,l)=n*(n+one)/(rerth*rerth)
10      continue
20    continue
      do 40 m=0,jcap-1
        mii=jcap-m
        do 30 l=1,jcap-m
          lii=jcap+1-l
          del2(mii,lii)=del2(m,l)
30      continue
40    continue
      return
      end subroutine m1rcons

      subroutine ozon3d(test_value,idim1,idim2,jdim1,jdim2,lk, &
                  pozn,xlat,rsin1,rcos1,rcos2,qo3)

!       adapted from OZON2D

!     pozn in mb (but OZON2D uses HPa--100*mb )
!     xlat in degrees

  parameter (nl=81,nlp1=nl+1,lngth=37*nl,rtd=57.2957795)
  include 'mpp.h'
  include 'SEASO3.comm'

  dimension qo3(idim1:idim2,jdim1:jdim2,lk),pozn(idim1:idim2,jdim1:jdim2,lk)
  dimension xlat(idim1:idim2,jdim1:jdim2)

  dimension qo3o3(idim1:idim2,nl),jjrow(idim1:idim2),tthan(idim1:idim2)

  do j=jdim1,jdim2

      DO I=idim1,idim2
        TH2=0.2*XLAT(I,j)
        JJROW(I)=19.001-TH2
        TTHAN(I)=(19-JJROW(I))-TH2
      ENDDO
!
!***  SEASONAL AND SPATIAL INTERPOLATION DONE BELOW.
!
      DO K=1,NL
      DO I=idim1,idim2
        DO3V=XDUO3N(JJROW(I),K)+RSIN1*XDO3N2(JJROW(I),K) &
                   +RCOS1*XDO3N3(JJROW(I),K) &
                   +RCOS2*XDO3N4(JJROW(I),K)
        DO3VP=XDUO3N(JJROW(I)+1,K)+RSIN1*XDO3N2(JJROW(I)+1,K) &
                    +RCOS1*XDO3N3(JJROW(I)+1,K) &
                    +RCOS2*XDO3N4(JJROW(I)+1,K)
!
!***  NOW LATITUDINAL INTERPOLATION
!***  AND CONVERT O3 INTO MASS MIXING RATIO (ORIG DATA MPY BY 1.E4)
!
        QO3O3(I,K)=1.E-4*(DO3V+TTHAN(I)*(DO3VP-DO3V))
      ENDDO
      ENDDO
!***
!***  VERTICAL INTERPOLATION FOR EACH GRIDPOINT (LINEAR IN LN P)
!***
      NUMITR=0
      ILOG=NL
   20 CONTINUE
      ILOG=(ILOG+1)/2
        IF(ILOG.EQ.1)GO TO 25
        NUMITR=NUMITR+1
        GO TO 20
   25 CONTINUE
!
      DO 60 K=1,LK
!
      NHALF=(NL+1)/2
      DO I=idim1,idim2
        JJROW(I)=NHALF
      ENDDO
!
      DO 40 IT=1,NUMITR
      NHALF=(NHALF+1)/2
      DO I=idim1,idim2
       if(pozn(i,j,k).lt.test_value) then
        IF(100.*POZN(I,j,K).LT.PRGFDL(JJROW(I)-1))THEN
          JJROW(I)=JJROW(I)-NHALF
        ELSEIF(100.*POZN(I,j,K).GE.PRGFDL(JJROW(I)))THEN
          JJROW(I)=JJROW(I)+NHALF
        ENDIF
        JJROW(I)=MIN(JJROW(I),NL)
        JJROW(I)=MAX(JJROW(I),2)
       end if
      ENDDO
   40 CONTINUE
!
      DO 50 I=idim1,idim2
     if(pozn(i,j,k).lt.test_value) then
      IF(100.*POZN(I,j,K).LT.PRGFDL(1))THEN
        QO3(I,j,K)=QO3O3(I,1)
      ELSE IF(100.*POZN(I,j,K).GT.PRGFDL(NL))THEN
        QO3(I,j,K)=QO3O3(I,NL)
      ELSE
        APLO=ALOG(PRGFDL(JJROW(I)-1))
        APHI=ALOG(PRGFDL(JJROW(I)))
        QO3(I,j,K)=QO3O3(I,JJROW(I))+(ALOG(100.*POZN(I,j,K))-APHI)/ &
                   (APLO-APHI)* &
                   (QO3O3(I,JJROW(I)-1)-QO3O3(I,JJROW(I)))
      ENDIF
     end if
   50 CONTINUE
!
   60 CONTINUE

  end do
!----------------------------------------------------------------------
      RETURN
      END
