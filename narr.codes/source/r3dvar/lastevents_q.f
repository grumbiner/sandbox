subroutine lastevents_q(wtq,mqdata,lbig3ges,mype)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator

  real(4) wtq(4,max(1,mqdata))
  real(4) yoq(max(1,mqdata)),xbarbq(max(1,mqdata)),bighq(lbig3ges,max(1,mqdata))
  real(4) eyoq00(max(1,mqdata)),qsatges(max(1,mqdata))
  integer(4) ibighq(lbig3ges,max(1,mqdata))
  character(8) qstaid(max(1,mqdata))

  integer(8),allocatable::iqlabel(:)
  real(4),allocatable::qlon(:),qlat(:),qpres(:),qletaobs(:)
  real(4),allocatable::qlone(:),qlate(:),qelev(:)
  real(4),allocatable::qtobs(:)
  real(4),allocatable::qtime(:),qtype(:),qmaxerr(:),qqm(:)
  character(10)eventfile

  if(mqdata.le.0) return
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  allocate(qlon(max(1,mqdata))) ; allocate(qlat(max(1,mqdata)))
  allocate(qlone(max(1,mqdata))) ; allocate(qlate(max(1,mqdata)))
  allocate(qpres(max(1,mqdata))) ; allocate(qtobs(max(1,mqdata)))
  allocate(qelev(max(1,mqdata)))
  allocate(qtime(max(1,mqdata)))
  allocate(qtype(max(1,mqdata)))
  allocate(qqm(max(1,mqdata)))
  allocate(qmaxerr(max(1,mqdata)))
  allocate(iqlabel(max(1,mqdata)))
  allocate(qletaobs(max(1,mqdata)))
  call rdqs(eyoq00,qlone,qlate,qlon,qlat,qpres,yoq,xbarbq,qsatges, &
            qletaobs,bighq,ibighq, &
            qstaid,qtime,qelev,qtobs,qqm,qtype,qmaxerr,iqlabel,mqdata,lbig3ges)

  wtlim=.1
  do i=1,mqdata
   qanl=wtq(4,i)*eyoq00(i)+xbarbq(i)
   icode=0
   if(abs(wtq(1,i)).lt.wtlim) icode=1
   write(ievout,'("QQ.",i3.3,11e13.5,2x,a8)')icode, &
     yoq(i),xbarbq(i),qanl,qlone(i),qlate(i),exp(qpres(i)),qelev(i), &
     qtime(i),qqm(i),eyoq00(i),qtype(i),qstaid(i)
  end do
  close(ievout)
     print *,' in lastevents_q, mype,mqdata=',mype,mqdata

  deallocate(qlone) ; deallocate(qlate)
  deallocate(qelev) ; deallocate(qtobs)
  deallocate(qtime) ; deallocate(qqm)
  deallocate(qtype) ; deallocate(qmaxerr)
  deallocate(qletaobs)

  deallocate(qpres)

  deallocate(qlon) ; deallocate(qlat)
  deallocate(iqlabel)

return
end subroutine lastevents_q
