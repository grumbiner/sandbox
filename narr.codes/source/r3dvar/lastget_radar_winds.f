subroutine lastget_radar_winds(yowr,eyowr00,xbarbwr,bighwr,ibighwr,mwrdata, &
                               lmetaex,lbig2ges,wetages,imeta,jmeta)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator

  real(4) yowr(max(1,mwrdata)),xbarbwr(lmetaex,max(1,mwrdata))
  real(4) bighwr(lbig2ges,2,max(1,mwrdata))
  real(4) eyowr00(max(1,mwrdata))
  integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))
  real(4) wetages(imeta*jmeta,lmetaex,2)

  real(4),allocatable::delta(:),epsilnw(:)
  real(4),allocatable::bigh(:,:)

    write(0,*)' at 1 in lastget_radar_winds'

  allocate(delta(max(1,mwrdata))) ; allocate(epsilnw(max(1,mwrdata)))
  allocate(bigh(lbig2ges,max(1,mwrdata)))
  call rd_radar_winds(eyowr00,delta,epsilnw,yowr,bigh,ibighwr,mwrdata,lbig2ges)

  if(mwrdata.gt.0) then
   do i=1,mwrdata
    do m=1,lbig2ges
     bighwr(m,1,i)=delta(i)*bigh(m,i)
     bighwr(m,2,i)=epsilnw(i)*bigh(m,i)
    end do
   end do
  end if
  deallocate(bigh)

  deallocate(delta) ; deallocate(epsilnw)

!    obtain xbarbwr, the local guess wind profile

  if(mwrdata.gt.0) then
   do i=1,mwrdata
    xbarbwr(1:lmetaex,i)=0.
    kbeamtop=ibighwr(lbig2ges+1,i)
    kbeambot=ibighwr(lbig2ges+2,i)
    do k=kbeamtop,kbeambot
     do kk=1,2
      do m=1,lbig2ges
       xbarbwr(k,i)=xbarbwr(k,i)+bighwr(m,kk,i)*wetages(ibighwr(m,i),k,kk)
      end do
     end do
    end do
   end do
  end if

return
end subroutine lastget_radar_winds
