subroutine lastgettemps(yot,eyot00,xbarbt,bight,ibight,mtdata,lbig3ges,tdata,tstaid,ivart, &
                        varlats,nvarlats,varpres,nvarpres,npes)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator

  real(4) yot(max(1,mtdata)),xbarbt(max(1,mtdata)),bight(lbig3ges,max(1,mtdata))
  real(4) eyot00(max(1,mtdata))
  integer(4) ibight(lbig3ges,max(1,mtdata))
  character(8) tstaid(max(1,mtdata))
  integer(4) ivart(nvarlats,nvarpres)
  real(4) varlats(nvarlats),varpres(nvarpres)

  integer(8),allocatable::itlabel(:)
  real(4),allocatable::tlon(:),tlat(:),tpres(:),tletaobs(:)
  real(4),allocatable::tlone(:),tlate(:),telev(:)
  real(4),allocatable::ttime(:),ttype(:),tqm(:)
  integer(4),allocatable::iqtflg(:)
  real(4) tdata(max(1,mtdata),6)

  allocate(tlon(max(1,mtdata))) ; allocate(tlat(max(1,mtdata)))
  allocate(tlone(max(1,mtdata))) ; allocate(tlate(max(1,mtdata)))
  allocate(tpres(max(1,mtdata)))
  allocate(telev(max(1,mtdata)))
  allocate(ttime(max(1,mtdata)))
  allocate(ttype(max(1,mtdata)))
  allocate(tqm(max(1,mtdata)))
  allocate(iqtflg(max(1,mtdata)))
  allocate(itlabel(max(1,mtdata)))
  allocate(tletaobs(max(1,mtdata)))
!     write(0,*)' at 1 in lastgettemps'
  call rdtemps(eyot00,tlone,tlate,tlon,tlat,tpres,yot,xbarbt, &
             tletaobs,bight,ibight, &
             tstaid,ttime,telev,tqm,ttype,iqtflg,itlabel,mtdata,lbig3ges)
! call var_3d_locs(tlone,tlate,tpres,mtdata,ivart,varlats,varpres,nvarlats,nvarpres,npes)
!     write(0,*)' at 2 in lastgettemps'

  if(mtdata.gt.0) then
   do i=1,mtdata
    tdata(i,1) = tpres(i)
    tdata(i,2) = yot(i)
    tdata(i,3) = xbarbt(i)
    tdata(i,4) = ttime(i)
    tdata(i,5) = ttype(i)
    tdata(i,6) = tqm(i)
   end do
  end if
!     write(0,*)' at 3 in lastgettemps'

  deallocate(tlone) ; deallocate(tlate)
  deallocate(telev)
  deallocate(ttime) ; deallocate(tqm)
  deallocate(ttype)
  deallocate(tletaobs)
  deallocate(iqtflg)

!     write(0,*)' at 4 in lastgettemps'
  deallocate(tpres)

  deallocate(tlon) ; deallocate(tlat)
  deallocate(itlabel)
!     write(0,*)' at 5 in lastgettemps'

return
end subroutine lastgettemps
