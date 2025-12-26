subroutine lastgetps(yop,eyop00,xbarbp,bighp,ibighp,mpdata,lbig2ges,psdata,pstaid,ivarp, &
                     varlats,nvarlats,npes)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator

  real(4) yop(max(1,mpdata)),xbarbp(max(1,mpdata)),bighp(lbig2ges,max(1,mpdata))
  real(4) eyop00(max(1,mpdata))
  integer(4) ibighp(lbig2ges,max(1,mpdata))
  character(8) pstaid(max(1,mpdata))
  integer(4) ivarp(nvarlats)
  real(4) varlats(nvarlats)

  integer(8),allocatable::iplabel(:)
  real(4),allocatable::plon(:),plat(:)
  real(4),allocatable::plone(:),plate(:),ptobs(:)
  real(4),allocatable::ptime(:),ptype(:),zqm(:)
  real(4),allocatable::hsfc(:),etaobssfc(:),pelev(:)
  real(4) psdata(max(1,mpdata),6)

  allocate(plon(max(1,mpdata))) ; allocate(plat(max(1,mpdata)))
  allocate(plone(max(1,mpdata))) ; allocate(plate(max(1,mpdata)))
  allocate(ptobs(max(1,mpdata)))
  allocate(ptime(max(1,mpdata)))
  allocate(ptype(max(1,mpdata))) ; allocate(zqm(max(1,mpdata)))
  allocate(iplabel(max(1,mpdata)))
  allocate(hsfc(max(1,mpdata)))
  allocate(etaobssfc(max(1,mpdata)))
  allocate(pelev(max(1,mpdata)))

  call rdsfcps(eyop00,plone,plate,plon,plat,yop,pelev,xbarbp, &
               hsfc,etaobssfc,bighp,ibighp, &
               ptobs,ptime,zqm,ptype,pstaid,iplabel,mpdata,lbig2ges)
! call var_2d_locs(plone,plate,mpdata,ivarp,varlats,nvarlats,npes)
  deallocate(iplabel)
  if(mpdata.gt.0) then

   do i=1,mpdata
    psdata(i,1) = pelev(i)
    psdata(i,2) = yop(i)
    psdata(i,3) = xbarbp(i)
    psdata(i,4) = ptime(i)
    psdata(i,5) = ptype(i)
    psdata(i,6) = zqm(i)
   end do
  end if

  deallocate(hsfc)
  deallocate(etaobssfc)
  deallocate(ptime) ; deallocate(ptype)
  deallocate(plone) ; deallocate(plate)
  deallocate(plon) ; deallocate(plat)
  deallocate(pelev)
  deallocate(ptobs) ; deallocate(zqm)
return
end subroutine lastgetps
