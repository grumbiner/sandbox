subroutine sortqs(iordges,lbig2ges,lbig3ges,lhalfges, &
           pdres01,tgges,qgges,hgges,imeta,jmeta,lmetaex,lmh, &
           etamex,etaiex,ptop,delhour,grossq,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)


!-------- evaluate guess at each obs point. then write out obs to temp
!-------- storage.

  include 'mpif.h'
      include "my_comm.h"
  include 'writges.h'

!-------- external arrays

  real(4) pdres01(imeta*jmeta),tgges(imeta*jmeta,lmetaex),qgges(imeta*jmeta,lmetaex)
  real(4) hgges(imeta*jmeta,lmetaex+1)
  integer(4) lmh(imeta*jmeta)
  real(4) etamex(lmetaex),etaiex(lmetaex+1)

!--------  internal work space

  real(4),allocatable::qerr(:),qlon(:),qlat(:),qlong(:),qlatg(:)
  real(4),allocatable::qpres(:),qobs(:)
  real(4),allocatable::qletaobs(:),bighq(:,:)
  integer(4),allocatable::icode(:)
  integer(4),allocatable::ibighq(:,:)
  integer(8),allocatable::iqlabel(:)
  character(8),allocatable::qstaid(:)
  real(4),allocatable::qges(:),qges0(:),qsatges(:)
  real(4),allocatable::qtime(:),qtype(:),qmaxerr(:),qqm(:)
  real(4),allocatable::qelev(:),qtobs(:)
  integer(4),allocatable::ngross_table(:),ngross_disp(:)
  character(90),allocatable::jwrite(:),jwrite0(:)
  real(8) qtrms_8,qtbar_8,count_8
  real(8) qtrmsall_8,qtbarall_8,countall_8
  character(8),allocatable::qestaid(:)
  real(4),allocatable::qepres(:),qetime(:),qetype(:),qeobs(:)
  real(4),allocatable::wgts3(:,:)
  integer(4),allocatable::iwgts3(:,:)
  character*16 filename,filenameg
  real(4) xmsg
  character(10)eventfile

  call mpi_comm_rank(my_comm,mype,ierr)
  xmsg=9999.

  if(mype.eq.0) write(0,*)' sortqs--at 1, mype=',mype

!-------- open dataset to hold diagnostic statistics output

  iwrite=154
  if(mype.eq.0) open(iwrite,file='fitq',form='formatted')

!-------- bring in all qs from disk

  call count_qs(nqdata)
  call mpi_allreduce(nqdata,nqdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  if(nqdataall.le.0) return    ! no data to process

  allocate(qerr(max(1,nqdata))) ; allocate(qlon(max(1,nqdata)))
  allocate(qlat(max(1,nqdata)))
  allocate(qlong(max(1,nqdata))) ; allocate(qlatg(max(1,nqdata)))
  allocate(qpres(max(1,nqdata))) ; allocate(qobs(max(1,nqdata)))
  allocate(qletaobs(max(1,nqdata)))
  allocate(bighq(lbig3ges,max(1,nqdata)))
  allocate(ibighq(lbig3ges,max(1,nqdata)))
  allocate(qstaid(max(1,nqdata)))
  allocate(qtime(max(1,nqdata))) ; allocate(qtype(max(1,nqdata)))
  allocate(qmaxerr(max(1,nqdata))) ; allocate(qqm(max(1,nqdata)))
  allocate(qelev(max(1,nqdata))) ; allocate(qtobs(max(1,nqdata)))
  allocate(qges(max(1,nqdata))) ; allocate(qsatges(max(1,nqdata)))
  allocate(qges0(max(1,nqdata)))
  allocate(qepres(max(1,nqdata))); allocate(qestaid(max(1,nqdata)))
  allocate(qetime(max(1,nqdata))); allocate(qetype(max(1,nqdata)))
  allocate(qeobs(max(1,nqdata)))
  allocate(iqlabel(max(1,nqdata)))
  allocate(icode(max(1,nqdata)))
  icode=0
  call rdqs(qerr,qlon,qlat,qlong,qlatg,qpres,qobs,qges,qsatges, &
              qletaobs,bighq,ibighq, &
            qstaid,qtime,qelev,qtobs,qqm,qtype,qmaxerr,iqlabel,nqdata,lbig3ges)
  qges0=qges
  qtype = nint(qtype)
       qerrmax=-huge(qerrmax)
       qerrmin=huge(qerrmax)
       if(nqdata.gt.0) then
        do i=1,nqdata
         qerrmax=max(qerr(i),qerrmax)
         qerrmin=min(qerr(i),qerrmin)
        end do
       end if
       call mpi_barrier(my_comm,ierror)
       call mpi_reduce(qerrmax,qerrmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(qerrmin,qerrminall,1,mpi_real,mpi_min,0,my_comm,ierror)
       if(mype.eq.0) print *,' after rdqs in sortqs, qerrmax,min=',qerrmaxall,qerrminall

  if(mype.eq.0) write(0,*)' sortqs--at 2, mype=',mype

!-------- evaluate guess q and guess sat q at obs locations

!-------- do conversion from elevation to pressure for sfc obs
!--------   (convert to guess pressure of station elevation for obs
!--------     with missing surface pressure)
!--------  make error proportional to local uncertainty of surface
!--------  elevation (presumably large when difference between obs
!--------  and model terrain is large and/or difference between
!--------  height of adajacent local steps is large)

  istaghglb=0
  allocate(wgts3(max(1,nqdata),lbig3ges))
  allocate(iwgts3(max(1,nqdata),lbig3ges))
  call getqges(qges,qsatges,qlong,qlatg,qpres,qelev,qstaid,qtobs,nqdata, &
               iordges,lbig2ges,lbig3ges,lhalfges, &
              qletaobs,wgts3,iwgts3, &
              imeta,jmeta,lmetaex,pdres01,tgges,qgges, &
              lmh,etamex,ptop,qtype,hgges,etaiex,qerr,qobs, &
              wbglb,dlon0,sbglb,dlat0,istaghglb,imetaglb,jmetaglb)
       if(mype.eq.0) write(0,*)' sortqs--at 3, mype=',mype
  if(nqdata.gt.0) then
   do k=1,lbig3ges
    do i=1,nqdata
     bighq(k,i)=wgts3(i,k)
     ibighq(k,i)=iwgts3(i,k)
    end do
   end do
  end if
  deallocate(wgts3)
  deallocate(iwgts3)

!-------- make sure all points outside domain are tagged

  qtimemax=-huge(qtimemax)
  qtimemin=huge(qtimemax)
  if(nqdata.gt.0) then
   do i=1,nqdata
    if(qobs(i).gt.9.e4) then
     icode(i)=4
     qges(i)=1.e20
    end if
    if(qerr(i).gt.1000.) then
     icode(i)=5
     qges(i)=1.e20
    end if
    if(qtime(i).lt.-delhour.or.qtime(i).gt.delhour) then
     icode(i)=3
     qges(i)=1.e20
    end if
    qtimemax=max(qtime(i),qtimemax)
    qtimemin=min(qtime(i),qtimemin)
   end do
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(qtimemax,qtimemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qtimemin,qtimeminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) print *,' in sortqs, qtimemax,min,delhour=',qtimemaxall,qtimeminall,delhour
  qtmax=-huge(qtmax)
  qtmin=huge(qtmax)
  qtobsmax=-huge(qtmax)
  qtobsmin=huge(qtmax)
  qtgesmax=-huge(qtmax)
  qtgesmin=huge(qtmax)
  qtsatgesmax=-huge(qtmax)
  qtsatgesmin=huge(qtmax)
  qtrms_8=0._8
  qtbar_8=0._8
  count_8=0._8
       if(mype.eq.0) write(0,*)' sortqs--at 4, mype=',mype
  if(nqdata.gt.0) then
   do i=1,nqdata
    if(qges(i).lt.1.e19) then
     qtobsmax=max(100.*qobs(i)/qsatges(i),qtobsmax)
     qtobsmin=min(100.*qobs(i)/qsatges(i),qtobsmin)
     qtgesmax=max(100.*qges(i)/qsatges(i),qtgesmax)
     qtgesmin=min(100.*qges(i)/qsatges(i),qtgesmin)
     qtsatgesmax=max(qsatges(i),qtsatgesmax)
     qtsatgesmin=min(qsatges(i),qtsatgesmin)
     qtmax=max(100.*(qobs(i)-qges(i))/qsatges(i),qtmax)
     qtmin=min(100.*(qobs(i)-qges(i))/qsatges(i),qtmin)
     qtrms_8=qtrms_8+(100._8*(qobs(i)-qges(i))/qsatges(i))**2
     qtbar_8=qtbar_8+(100._8*(qobs(i)-qges(i))/qsatges(i))
     count_8=count_8+1._8
    end if
   end do
  end if

!------------------- print out stats on fit of ges to data

  call resq(qpres,qobs,qges,qsatges,qtype,nqdata,iwrite)
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(qtsatgesmax,qtsatgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qtsatgesmin,qtsatgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(qtgesmax,qtgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qtgesmin,qtgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(qtobsmax,qtobsmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qtobsmin,qtobsminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(qtmax,qtmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qtmin,qtminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(count_8,countall_8,1,mpi_real,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(qtrms_8,qtrmsall_8,1,mpi_real,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(qtbar_8,qtbarall_8,1,mpi_real,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0.and.countall.gt.0.) then
   qtrmsall_8=sqrt(qtrmsall_8/countall_8)
   qtbarall_8=qtbarall_8/countall_8
   write(iwrite,'('' in sortqs, ges qmax,min='',2es14.5)')qtsatgesmaxall,qtsatgesminall
   write(iwrite,'('' in sortqs, ges qmax,min='',2es14.5)')qtgesmaxall,qtgesminall
   write(iwrite,'('' in sortqs, obs qmax,min='',2es14.5)')qtobsmaxall,qtobsminall
   write(iwrite,'('' in sortqs, residual qmax,min='',2es14.5)')qtmaxall,qtminall
   write(iwrite,'('' residual mean, rmsq ='',2es14.5)')qtbarall_8,qtrmsall_8
  end if

!-------- do gross check for really bad wets

       if(mype.eq.0) write(0,*)' sortqs--at 5, mype=',mype

  qtmax=-huge(qtmax)
  qtmin=huge(qtmax)
  qtrms_8=0._8
  qtbar_8=0._8
  count_8=0._8
  ngrossq=0
  if(nqdata.gt.0) then
   allocate(jwrite(1000))
   do i=1,nqdata
    if(qges(i).lt.1.e19) then
     if(abs(100.*(qobs(i)-qges(i))/qsatges(i)).gt.grossq) then
      icode(i)=2
      ngrossq=ngrossq+1
      if(ngrossq.lt.1000) then
       rqpres=exp(qpres(i))
       write(jwrite(ngrossq),347)qobs(i),qges(i),qsatges(i), &
         qerr(i),qlat(i),qlon(i),rqpres, &
         qtime(i),qtype(i),qstaid(i)
 347   format(1h ,3e9.2,f6.1,f7.2,f9.2,f8.1,f12.2,f6.0,2x,a8)
      qepres(ngrossq) = min(1.e7,exp(qpres(i)))
      qestaid(ngrossq) = qstaid(i)
      qetime(ngrossq) = qtime(i)
      qetype(ngrossq) = qtype(i)
      qeobs(ngrossq) = qobs(i)
      end if
      qges(i)=1.e20
     else
      qtmax=max(100.*(qobs(i)-qges(i))/qsatges(i),qtmax)
      qtmin=min(100.*(qobs(i)-qges(i))/qsatges(i),qtmin)
      qtrms_8=qtrms_8+(100._8*(qobs(i)-qges(i))/qsatges(i))**2
      qtbar_8=qtbar_8+(100._8*(qobs(i)-qges(i))/qsatges(i))
      count_8=count_8+1._8
     end if
    end if
   end do
  end if
  if(mype.eq.0) write(0,*)' sortqs--at 10, mype=',mype
  iq = 4
  irc = 1
  if(mype.eq.0) write(0,*)' sortqs(10)--ngrossq,iq: ',ngrossq,iq
  iunit = iuniterr + mype
! write(filename,'(''qcerror'',i3.3)')mype
! open(iunit,file=filename,form='formatted')
!!if(ngrossq.gt.0) then
!!  write(iunit,500) ngrossq, iq, irc
!!  write(iunit,501) (qestaid(i),qepres(i),qetime(i),qetype(i),i=1,ngrossq)
!!endif
  write(6,500) ngrossq, iq, irc
  write(6,501) (qestaid(i),qepres(i),qetime(i),qetype(i),i=1,ngrossq)
500 format(1x,3i8)
501 format(1x,a10,f10.1,f10.2,f10.0)
502 format(1x,a10,f10.1,f10.2,f10.0,f10.1)

!--------write out guess values

    irc = 3
    if(writges .and. nqdata.gt.0) then
!---------------------replace big qges values with 999999.9
!     do i=1,nqdata
!      if(qges(i).lt.1.e19) then
!       qges0(i)=1.e6*qges(i)
!      else
!       qges0(i)=999999.9
!      end if
!     end do
      iunitg = iuniterr + mype + npes + 1
!     write(iunitg,500) nqdata, iq, irc
!     write(iunitg,502) (qstaid(i),min(1.e7,exp(qpres(i))),qtime(i),qtype(i),qges0(i),i=1,nqdata)
    endif

!------------------ print out stats on fit of ges to data after gross
!-------------------             check

  call mpi_barrier(my_comm,ierror)
  call mpi_allreduce(ngrossq,ngrossqall,1,mpi_integer,mpi_sum,my_comm,ierror)
  if(ngrossqall.gt.0) then
   allocate(ngross_table(0:npes-1)) ; allocate(ngross_disp(0:npes-1))
   call mpi_gather(ngrossq,1,mpi_integer,ngross_table,1,mpi_integer,0,my_comm,ierror)
   ngross_disp=0
   if(mype.eq.0) then
    do ipe=0,npes-1
     if(ipe.gt.0) ngross_disp(ipe)=ngross_disp(ipe-1)+ngross_table(ipe-1)
    end do
   end if
   allocate(jwrite0(ngrossqall))
   call mpi_gatherv(jwrite,90*ngrossq,mpi_character, &
          jwrite0,90*ngross_table,90*ngross_disp,mpi_character,0,my_comm,ierror)
   deallocate(ngross_table) ; deallocate(ngross_disp)
   call mpi_barrier(my_comm,ierror)
   if(mype.eq.0) then
    write(iwrite,*)' qs which fail gross check follow:'
    write(iwrite,*)'  qobs, qges, qsat, qerr, qlat,  qlon, qpres,',' qtime,  qtype, qid '
    do i=1,ngrossqall
     write(iwrite,*)jwrite0(i)
    end do
    write(iwrite,*)' ngrossq=',ngrossqall
    write(iwrite,*)' fit to qs after gross errors removed follows:'
   end if
   deallocate(jwrite0)
   call mpi_barrier(my_comm,ierror)
   call resq(qpres,qobs,qges,qsatges,qtype,nqdata,iwrite)
   call mpi_barrier(my_comm,ierror)
   call mpi_reduce(qtmax,qtmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
   call mpi_reduce(qtmin,qtminall,1,mpi_real,mpi_min,0,my_comm,ierror)
   call mpi_reduce(qtrms_8,qtrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(qtbar_8,qtbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   if(mype.eq.0.and.countall_8.gt.0._8) then
    qtrmsall_8=sqrt(qtrmsall_8/countall_8)
    qtbarall_8=qtbarall_8/countall_8
    write(iwrite,'('' in sortqs, residual qmax,min='',2es14.5)')qtmaxall,qtminall
    write(iwrite,'('' residual mean, rmsq ='',2es14.5)')qtbarall_8,qtrmsall_8
   end if
   call mpi_barrier(my_comm,ierror)
  end if

!   before removing flagged obs, write to events file

 if(nqdata.gt.0) then
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  do i=1,nqdata
   if(icode(i).gt.0)then
    write(ievout,'("QQ.",i3.3,11e13.5,2x,a8)')icode(i), &
     qobs(i),qges0(i),xmsg,qlon(i),qlat(i),exp(qpres(i)),qelev(i), &
     qtime(i),qqm(i),qerr(i),qtype(i),qstaid(i)
   end if
  end do
  close(ievout)
 end if

!   remove flagged obs

       if(mype.eq.0) write(0,*)' sortqs--at 11, mype=',mype
  ii=0
  if(nqdata.gt.0) then
   do i=1,nqdata
    if(qges(i).lt.1.e19) then
     ii=ii+1
     qerr(ii)=qerr(i)
     qlon(ii)=qlon(i)
     qlat(ii)=qlat(i)
     qlong(ii)=qlong(i)
     qlatg(ii)=qlatg(i)
     qpres(ii)=qpres(i)
     qobs(ii)=qobs(i)
     qges(ii)=qges(i)
     qsatges(ii)=qsatges(i)
     qletaobs(ii)=qletaobs(i)
     bighq(1:lbig3ges,ii)=bighq(1:lbig3ges,i)
     ibighq(1:lbig3ges,ii)=ibighq(1:lbig3ges,i)
     qstaid(ii)=qstaid(i)
     qtime(ii)=qtime(i)
     qelev(ii)=qelev(i)
     qtobs(ii)=qtobs(i)
     qqm(ii)=qqm(i)
     qtype(ii)=qtype(i)
     qmaxerr(ii)=qmaxerr(i)
     iqlabel(ii)=iqlabel(i)
    end if
   end do
  end if
  mqdata=ii

  call wrqs(qerr,qlon,qlat,qlong,qlatg,qpres,qobs,qges,qsatges, &
              qletaobs,bighq,ibighq, &
            qstaid,qtime,qelev,qtobs,qqm,qtype,qmaxerr,iqlabel,mqdata,lbig3ges)

  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(nqdata,nqdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(mqdata,mqdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) then
   write(iwrite,*)' in sortqs, nqdata,mqdata=',nqdataall,mqdataall
   close(iwrite)
  end if
       qerrmax=-huge(qerrmax)
       qerrmin=huge(qerrmax)
       if(mqdata.gt.0) then
        do i=1,mqdata
         qerrmax=max(qerr(i),qerrmax)
         qerrmin=min(qerr(i),qerrmin)
        end do
       end if
       call mpi_barrier(my_comm,ierror)
       call mpi_reduce(qerrmax,qerrmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(qerrmin,qerrminall,1,mpi_real,mpi_min,0,my_comm,ierror)
       if(mype.eq.0) print *,'  at end of sortqs, qerrmax,min=',qerrmaxall,qerrminall

   deallocate(qerr)
   deallocate(qpres) ; deallocate(qobs)
   deallocate(qstaid) ; deallocate(qtobs)
   deallocate(qtime) ; deallocate(qtype)
   deallocate(qmaxerr) ; deallocate(qelev)
   deallocate(qlon) ; deallocate(qlat)
   deallocate(qlong) ; deallocate(qlatg)
   deallocate(qges) ; deallocate(qsatges)
   deallocate(qges0)
   deallocate(qepres); deallocate(qestaid)
   deallocate(qetime); deallocate(qetype)
   deallocate(qeobs)
   deallocate(qqm)
   deallocate(iqlabel)
   deallocate(qletaobs)
   deallocate(bighq)
   deallocate(ibighq)
   deallocate(icode)

return
end subroutine sortqs
