subroutine lastgetqs(yoq,eyoq00,xbarbq,qsatges,bighq,ibighq,mqdata,lbig3ges,qdata,qstaid,ivarq, &
                        varlats,nvarlats,varpres,nvarpres,npes)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator

  real(4) yoq(max(1,mqdata)),xbarbq(max(1,mqdata)),bighq(lbig3ges,max(1,mqdata))
  real(4) eyoq00(max(1,mqdata)),qsatges(max(1,mqdata))
  integer(4) ibighq(lbig3ges,max(1,mqdata))
  character(8) qstaid(max(1,mqdata))
  integer(4) ivarq(nvarlats,nvarpres)
  real(4) varlats(nvarlats),varpres(nvarpres)

  integer(8),allocatable::iqlabel(:)
  real(4),allocatable::qlon(:),qlat(:),qpres(:),qletaobs(:)
  real(4),allocatable::qlone(:),qlate(:),qelev(:)
  real(4),allocatable::qtobs(:)
  real(4),allocatable::qtime(:),qtype(:),qmaxerr(:),qqm(:)
  real(4) qdata(max(1,mqdata),6)

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
! call var_3d_locs(qlone,qlate,qpres,mqdata,ivarq,varlats,varpres,nvarlats,nvarpres,npes)

  if(mqdata.gt.0) then
   do i=1,mqdata
    qdata(i,1) = qpres(i)
    qdata(i,2) = yoq(i)
    qdata(i,3) = xbarbq(i)
    qdata(i,4) = qtime(i)
    qdata(i,5) = qtype(i)
    qdata(i,6) = qqm(i)
   end do
  end if

  deallocate(qlone) ; deallocate(qlate)
  deallocate(qelev) ; deallocate(qtobs)
  deallocate(qtime) ; deallocate(qqm)
  deallocate(qtype) ; deallocate(qmaxerr)
  deallocate(qletaobs)

  deallocate(qpres)

  deallocate(qlon) ; deallocate(qlat)
  deallocate(iqlabel)

return
end subroutine lastgetqs
