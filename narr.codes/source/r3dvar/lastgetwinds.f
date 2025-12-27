subroutine lastgetwinds(yow,eyow00,xbarbw,bighw,ibighw,mwdata,lbig3ges,wdata,wstaid,ivaru,ivarv, &
                        varlats,nvarlats,varpres,nvarpres,npes)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator

  real(4) yow(max(1,mwdata)),xbarbw(max(1,mwdata))
  real(4) bighw(lbig3ges,2,max(1,mwdata))
  real(4) eyow00(max(1,mwdata))
  integer(4) ibighw(lbig3ges,max(1,mwdata))
  real(4) wdata(max(1,mwdata),6)
  character(8) wstaid(max(1,mwdata))
  integer(4) ivaru(nvarlats,nvarpres)
  integer(4) ivarv(nvarlats,nvarpres)
  real(4) varlats(nvarlats),varpres(nvarpres)

  integer(8),allocatable::iwlabel(:)
  real(4),allocatable::wlon(:),wlat(:),wpres(:),wletaobs(:)
  real(4),allocatable::wlone(:),wlate(:),welev(:),etheta(:)
  real(4),allocatable::wtime(:),wtype(:),delta(:),epsilnw(:),wqm(:)
  real(4),allocatable::bigh(:,:)

!   write(0,*)' at 1 in lastgetwinds'

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
! call var_3d_wlocs(wlone,wlate,wpres,etheta,0.,mwdata,ivaru,varlats,varpres,nvarlats,nvarpres,npes)
! call var_3d_wlocs(wlone,wlate,wpres,etheta,90.,mwdata,ivarv,varlats,varpres,nvarlats,nvarpres,npes)
  deallocate(wletaobs)

  if(mwdata.gt.0) then
   do i=1,mwdata
    do m=1,lbig3ges
     bighw(m,1,i)=delta(i)*bigh(m,i)
     bighw(m,2,i)=epsilnw(i)*bigh(m,i)
    end do
   end do
  end if
  deallocate(bigh)

  if(mwdata.gt.0) then
   do i=1,mwdata
    wdata(i,1) = wpres(i)
    wdata(i,2) = yow(i)
    wdata(i,3) = xbarbw(i)
    wdata(i,4) = wtime(i)
    wdata(i,5) = wtype(i)
    wdata(i,6) = wqm(i)
   end do
  end if

  deallocate(wlone) ; deallocate(wlate)
  deallocate(wtime) ; deallocate(wqm)
  deallocate(wtype) ; deallocate(welev) ; deallocate(etheta)

  deallocate(iwlabel)
  deallocate(wpres)



  deallocate(delta) ; deallocate(epsilnw)
  deallocate(wlon) ; deallocate(wlat)

return
end subroutine lastgetwinds
