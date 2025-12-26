subroutine lastevents_w(wtw,mwdata,lbig3ges,mype)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator

  real(4) wtw(4,max(1,mwdata))
  real(4) yow(max(1,mwdata)),xbarbw(max(1,mwdata))
  real(4) bighw(lbig3ges,2,max(1,mwdata))
  real(4) eyow00(max(1,mwdata))
  integer(4) ibighw(lbig3ges,max(1,mwdata))
  character(8) wstaid(max(1,mwdata))

  integer(8),allocatable::iwlabel(:)
  real(4),allocatable::wlon(:),wlat(:),wpres(:),wletaobs(:)
  real(4),allocatable::wlone(:),wlate(:),welev(:),etheta(:)
  real(4),allocatable::wtime(:),wtype(:),delta(:),epsilnw(:),wqm(:)
  real(4),allocatable::bigh(:,:)
  character(10)eventfile

  if(mwdata.le.0) return
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  allocate(wlone(max(1,mwdata))) ; allocate(wlate(max(1,mwdata)))
  allocate(wlon(max(1,mwdata))) ; allocate(wlat(max(1,mwdata)))
  allocate(wpres(max(1,mwdata))) ; allocate(wqm(max(1,mwdata)))
  allocate(wtime(max(1,mwdata))) ; allocate(wtype(max(1,mwdata)))
  allocate(welev(max(1,mwdata))) ; allocate(etheta(max(1,mwdata)))
  allocate(delta(max(1,mwdata))) ; allocate(epsilnw(max(1,mwdata)))
  allocate(iwlabel(max(1,mwdata)))
  allocate(wletaobs(max(1,mwdata)))
  allocate(bigh(lbig3ges,max(1,mwdata)))
  call rd_conventional_winds(eyow00,wlone,wlate,wlon,wlat,wpres,etheta,delta,epsilnw,yow,xbarbw, &
               wletaobs,bigh,ibighw, &
               wstaid,wtime,welev,wqm,wtype,iwlabel,mwdata,lbig3ges)
  deallocate(wletaobs)

  wtlim=.1
  do i=1,mwdata
   wanl=wtw(4,i)*eyow00(i)+xbarbw(i)
   icode=0
   if(abs(wtw(1,i)).lt.wtlim) icode=1
   if(abs(etheta(i)).lt..1) then
    write(ievout,'("UU.",i3.3,11e13.5,2x,a8)')icode, &
     yow(i),xbarbw(i),wanl,wlone(i),wlate(i),exp(wpres(i)),welev(i), &
     wtime(i),wqm(i),eyow00(i),wtype(i),wstaid(i)
   else
    write(ievout,'("VV.",i3.3,11e13.5,2x,a8)')icode, &
     yow(i),xbarbw(i),wanl,wlone(i),wlate(i),exp(wpres(i)),welev(i), &
     wtime(i),wqm(i),eyow00(i),wtype(i),wstaid(i)
   end if
  end do
  close(ievout)
     print *,' in lastevents_w, mype,mwdata=',mype,mwdata

  deallocate(bigh)

  deallocate(wlone) ; deallocate(wlate)
  deallocate(wtime) ; deallocate(wqm)
  deallocate(wtype) ; deallocate(welev) ; deallocate(etheta)

  deallocate(iwlabel)
  deallocate(wpres)



  deallocate(delta) ; deallocate(epsilnw)
  deallocate(wlon) ; deallocate(wlat)

return
end subroutine lastevents_w
