subroutine lastevents_p(wtp,mpdata,lbig2ges,mype)

!-------- last write to events file.  write all data, with flag of
!--------  zero if successfully used, and 1 if rejected by non-lin qc
!--------

  real(4) wtp(4,max(1,mpdata))
  real(4) yop(max(1,mpdata)),xbarbp(max(1,mpdata)),bighp(lbig2ges,max(1,mpdata))
  real(4) eyop00(max(1,mpdata))
  integer(4) ibighp(lbig2ges,max(1,mpdata))
  character(8) pstaid(max(1,mpdata))

  integer(8),allocatable::iplabel(:)
  real(4),allocatable::plon(:),plat(:)
  real(4),allocatable::plone(:),plate(:),ptobs(:)
  real(4),allocatable::ptime(:),ptype(:),zqm(:)
  real(4),allocatable::hsfc(:),etaobssfc(:),pelev(:)
  character(10)eventfile

  if(mpdata.le.0) return
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
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

  wtlim=.1
  do i=1,mpdata
   panl=wtp(4,i)*eyop00(i)+xbarbp(i)
   icode=0
   if(abs(wtp(1,i)).lt.wtlim) icode=1
   write(ievout,'("PP.",i3.3,11e13.5,2x,a8)')icode, &
     yop(i),xbarbp(i),panl,plone(i),plate(i),pelev(i), &
     ptime(i),ptobs(i),zqm(i),eyop00(i),ptype(i),pstaid(i)
  end do
  close(ievout)
     print *,' in lastevents_p, mype,mpdata=',mype,mpdata

  deallocate(iplabel)

  deallocate(hsfc)
  deallocate(etaobssfc)
  deallocate(ptime) ; deallocate(ptype)
  deallocate(plone) ; deallocate(plate)
  deallocate(plon) ; deallocate(plat)
  deallocate(pelev)
  deallocate(ptobs) ; deallocate(zqm)

return
end subroutine lastevents_p
